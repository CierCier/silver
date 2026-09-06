//! Match pattern and enum variant exhaustiveness completion.

use agc::symbol_index::{SymbolIndex, SymbolKind, type_root_name_of_str};
use tower_lsp_server::ls_types::*;

pub(crate) fn complete_match_arms(
    analysis: &SymbolIndex,
    match_expr: &str,
    arm_prefix: &str,
    existing_arms: &[String],
) -> Option<Vec<CompletionItem>> {
    let enum_name = resolve_match_enum(analysis, match_expr)?;
    let qualifier = format!("{enum_name}::");

    let mut unhandled_variants: Vec<(&str, String)> = Vec::new();

    // 1. From indexed symbols
    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() == Some(qualifier.as_str()) && sym.kind == SymbolKind::Variant {
            if !existing_arms.contains(&sym.name) {
                unhandled_variants.push((&sym.name, sym.signature.clone()));
            }
        }
    }

    // 2. Token-based fallback if enum was parsed from broken buffer
    if unhandled_variants.is_empty() {
        for (i, t) in analysis.tokens.iter().enumerate() {
            if matches!(t.kind, agc::lexer::Token::Enum) {
                if let Some(next) = analysis.tokens.get(i + 1) {
                    if let agc::lexer::Token::Identifier(name) = &next.kind {
                        if name == &enum_name {
                            // Collect variant identifiers inside the enum's braces
                            let mut j = i + 2;
                            while j < analysis.tokens.len() && !matches!(analysis.tokens[j].kind, agc::lexer::Token::LeftBrace) {
                                j += 1;
                            }
                            j += 1;
                            while j < analysis.tokens.len() && !matches!(analysis.tokens[j].kind, agc::lexer::Token::RightBrace) {
                                if let agc::lexer::Token::Identifier(var_name) = &analysis.tokens[j].kind {
                                    if !existing_arms.contains(var_name) && !unhandled_variants.iter().any(|(v, _)| v == var_name) {
                                        let mut has_payload = false;
                                        if let Some(after) = analysis.tokens.get(j + 1) {
                                            if matches!(after.kind, agc::lexer::Token::LeftParen | agc::lexer::Token::LeftBrace) {
                                                has_payload = true;
                                            }
                                        }
                                        let sig = if has_payload { format!("{var_name}(...)") } else { var_name.clone() };
                                        unhandled_variants.push((var_name, sig));
                                    }
                                }
                                j += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    // Also check standard Option / Result if applicable
    if unhandled_variants.is_empty() {
        if enum_name == "Option" || enum_name == "Optional" {
            return Some(option_match_arms(existing_arms, arm_prefix));
        } else if enum_name == "Result" {
            return Some(result_match_arms(existing_arms, arm_prefix));
        }
        return None;
    }

    let mut items = Vec::new();

    // 1. "Fill all match arms" snippet
    if unhandled_variants.len() > 1 && arm_prefix.is_empty() {
        let all_arms: Vec<String> = unhandled_variants
            .iter()
            .enumerate()
            .map(|(i, (name, sig))| {
                let has_payload = sig.contains('(') || sig.contains('{');
                if has_payload {
                    format!("{}(${{{}:val}}) => ${{{}:expr}},", name, i * 2 + 1, i * 2 + 2)
                } else {
                    format!("{} => ${{{}:expr}},", name, i + 1)
                }
            })
            .collect();

        items.push(CompletionItem {
            label: format!("... Fill all {} match arms", unhandled_variants.len()),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some(format!("Exhaustive match pattern arms for {enum_name}")),
            insert_text: Some(all_arms.join("\n    ")),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_00_fill_all".to_string()),
            ..Default::default()
        });
    }

    // 2. Individual variant arms
    for (name, sig) in unhandled_variants {
        if !arm_prefix.is_empty() && !name.starts_with(arm_prefix) {
            continue;
        }

        let has_payload = sig.contains('(') || sig.contains('{');
        let insert_text = if has_payload {
            format!("{}(${{1:val}}) => ${{0:expr}},", name)
        } else {
            format!("{} => ${{0:expr}},", name)
        };

        items.push(CompletionItem {
            label: name.to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some(format!("Match arm for {sig}")),
            insert_text: Some(insert_text),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some(format!("0_{}", name)),
            ..Default::default()
        });
    }

    if items.is_empty() {
        None
    } else {
        Some(items)
    }
}

fn resolve_match_enum(analysis: &SymbolIndex, expr: &str) -> Option<String> {
    let clean_expr = expr.trim();
    for s in &analysis.symbols {
        if s.name == clean_expr {
            if let Some(ty) = s.inferred_type.as_deref() {
                if let Some(root) = type_root_name_of_str(ty) {
                    return Some(root);
                }
            }
            let parts: Vec<&str> = s.signature.split_whitespace().collect();
            if parts.len() >= 2 {
                if let Some(root) = type_root_name_of_str(parts[0]) {
                    return Some(root);
                }
            }
        }
    }

    // Direct enum lookup
    if analysis.symbols.iter().any(|s| s.name == clean_expr && s.kind == SymbolKind::Enum) {
        return Some(clean_expr.to_string());
    }

    // Fallback: check tokens for variable declaration `EnumName var`
    for (i, t) in analysis.tokens.iter().enumerate() {
        if let agc::lexer::Token::Identifier(var_name) = &t.kind {
            if var_name == clean_expr && i > 0 {
                if let agc::lexer::Token::Identifier(type_name) = &analysis.tokens[i - 1].kind {
                    return Some(type_name.clone());
                }
            }
        }
    }

    None
}

fn option_match_arms(existing: &[String], prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if !existing.contains(&"Some".to_string()) && (prefix.is_empty() || "Some".starts_with(prefix)) {
        items.push(CompletionItem {
            label: "Some(val) => ...".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Option::Some arm".to_string()),
            insert_text: Some("Some(${1:val}) => ${0:expr},".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_Some".to_string()),
            ..Default::default()
        });
    }
    if !existing.contains(&"None".to_string()) && (prefix.is_empty() || "None".starts_with(prefix)) {
        items.push(CompletionItem {
            label: "None => ...".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Option::None arm".to_string()),
            insert_text: Some("None => ${0:expr},".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_None".to_string()),
            ..Default::default()
        });
    }
    items
}

fn result_match_arms(existing: &[String], prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    if !existing.contains(&"Ok".to_string()) && (prefix.is_empty() || "Ok".starts_with(prefix)) {
        items.push(CompletionItem {
            label: "Ok(val) => ...".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Result::Ok arm".to_string()),
            insert_text: Some("Ok(${1:val}) => ${0:expr},".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_Ok".to_string()),
            ..Default::default()
        });
    }
    if !existing.contains(&"Err".to_string()) && (prefix.is_empty() || "Err".starts_with(prefix)) {
        items.push(CompletionItem {
            label: "Err(err) => ...".to_string(),
            kind: Some(CompletionItemKind::ENUM_MEMBER),
            detail: Some("Result::Err arm".to_string()),
            insert_text: Some("Err(${1:err}) => ${0:expr},".to_string()),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_Err".to_string()),
            ..Default::default()
        });
    }
    items
}
