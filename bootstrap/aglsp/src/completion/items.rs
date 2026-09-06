//! Item, type position, statement and keyword completion.

use agc::symbol_index::{Symbol, SymbolIndex, SymbolKind};
use tower_lsp_server::ls_types::*;

use crate::completion::snippets::*;
use crate::doc;

pub(crate) fn complete_type_position(
    analysis: &SymbolIndex,
    prefix: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // 1. Primitive types
    for &pt in PRIMITIVE_TYPES {
        if pt.starts_with(prefix) {
            items.push(CompletionItem {
                label: pt.to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("primitive type".to_string()),
                sort_text: Some(format!("0_{pt}")),
                ..Default::default()
            });
        }
    }

    // 2. Generic containers with type parameter snippets
    for &(label, snippet, doc_str) in GENERIC_TYPE_SNIPPETS {
        if label.starts_with(prefix) || snippet.starts_with(prefix) {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::CLASS),
                detail: Some(doc_str.to_string()),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("1_{label}")),
                ..Default::default()
            });
        }
    }

    // 3. User & imported Structs, Enums, Traits, and Type Aliases
    for sym in &analysis.symbols {
        if !matches!(
            sym.kind,
            SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait | SymbolKind::TypeAlias
        ) {
            continue;
        }
        if !prefix.is_empty() && !sym.name.starts_with(prefix) {
            continue;
        }

        let kind = match sym.kind {
            SymbolKind::Struct => CompletionItemKind::STRUCT,
            SymbolKind::Enum => CompletionItemKind::ENUM,
            SymbolKind::Trait => CompletionItemKind::INTERFACE,
            SymbolKind::TypeAlias => CompletionItemKind::CLASS,
            _ => CompletionItemKind::CLASS,
        };

        items.push(CompletionItem {
            label: sym.name.clone(),
            kind: Some(kind),
            detail: Some(sym.signature.clone()),
            documentation: sym.doc.as_deref().map(format_doc),
            insert_text: Some(sym.name.clone()),
            sort_text: Some(format!("2_{}", sym.name)),
            ..Default::default()
        });
    }

    // 4. Token-based fallback for user types defined in partially parsed/broken buffers
    for (i, t) in analysis.tokens.iter().enumerate() {
        if matches!(t.kind, agc::lexer::Token::Struct | agc::lexer::Token::Enum | agc::lexer::Token::Trait) {
            if let Some(next) = analysis.tokens.get(i + 1) {
                if let agc::lexer::Token::Identifier(name) = &next.kind {
                    if (prefix.is_empty() || name.starts_with(prefix))
                        && !items.iter().any(|item| &item.label == name)
                    {
                        items.push(CompletionItem {
                            label: name.clone(),
                            kind: Some(match t.kind {
                                agc::lexer::Token::Struct => CompletionItemKind::STRUCT,
                                agc::lexer::Token::Enum => CompletionItemKind::ENUM,
                                _ => CompletionItemKind::INTERFACE,
                            }),
                            detail: Some(format!("type {name}")),
                            insert_text: Some(name.clone()),
                            sort_text: Some(format!("2_{name}")),
                            ..Default::default()
                        });
                    }
                }
            }
        }
    }

    items
}

pub(crate) fn complete_statement_or_expr(
    analysis: &SymbolIndex,
    offset: usize,
    prefix: &str,
    in_loop: bool,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // 1. In-scope locals and parameters
    for sym in &analysis.symbols {
        if !matches!(sym.kind, SymbolKind::Local | SymbolKind::Parameter | SymbolKind::TypeParam) {
            continue;
        }
        if sym.span.end > offset {
            continue;
        }
        if !prefix.is_empty() && !sym.name.starts_with(prefix) {
            continue;
        }

        items.push(CompletionItem {
            label: sym.name.clone(),
            kind: Some(CompletionItemKind::VARIABLE),
            detail: Some(sym.signature.clone()),
            insert_text: Some(sym.name.clone()),
            sort_text: Some(format!("0_{}", sym.name)),
            ..Default::default()
        });
    }

    // 2. Control flow snippets
    for &(label, detail, snippet, doc_str) in CONTROL_FLOW_SNIPPETS {
        if label.starts_with(prefix) {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                detail: Some(detail.to_string()),
                documentation: Some(format_doc(doc_str)),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("1_{label}")),
                ..Default::default()
            });
        }
    }

    // 3. Loop control statements if in loop
    if in_loop {
        if "break".starts_with(prefix) {
            items.push(CompletionItem {
                label: "break".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                insert_text: Some("break;".to_string()),
                sort_text: Some("1_break".to_string()),
                ..Default::default()
            });
        }
        if "continue".starts_with(prefix) {
            items.push(CompletionItem {
                label: "continue".to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                insert_text: Some("continue;".to_string()),
                sort_text: Some("1_continue".to_string()),
                ..Default::default()
            });
        }
    }

    // 4. Functions with parameter snippets
    for sym in &analysis.symbols {
        if !matches!(
            sym.kind,
            SymbolKind::Function | SymbolKind::ExternFunction | SymbolKind::Const | SymbolKind::Global
        ) {
            continue;
        }
        if !prefix.is_empty() && !sym.name.starts_with(prefix) {
            continue;
        }

        items.push(callable_or_value_item(sym));
    }

    // 5. Macro builtins
    items.extend(macro_completions(prefix));

    // 6. Types available for cast / construction
    items.extend(complete_type_position(analysis, prefix));

    items
}

pub(crate) fn complete_top_level(
    analysis: &SymbolIndex,
    prefix: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();

    // 1. Top level declaration snippets
    for &(label, detail, snippet, doc_str) in TOP_LEVEL_SNIPPETS {
        if label.starts_with(prefix) {
            items.push(CompletionItem {
                label: label.to_string(),
                kind: Some(CompletionItemKind::SNIPPET),
                detail: Some(detail.to_string()),
                documentation: Some(format_doc(doc_str)),
                insert_text: Some(snippet.to_string()),
                insert_text_format: Some(InsertTextFormat::SNIPPET),
                sort_text: Some(format!("0_{label}")),
                ..Default::default()
            });
        }
    }

    // 2. Top level keywords
    for &kw in KEYWORDS_TOP_LEVEL {
        if kw.starts_with(prefix) {
            items.push(CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                sort_text: Some(format!("1_{kw}")),
                ..Default::default()
            });
        }
    }

    // 3. Types and functions for top-level references
    items.extend(complete_type_position(analysis, prefix));

    items
}

fn callable_or_value_item(sym: &Symbol) -> CompletionItem {
    let is_callable = matches!(
        sym.kind,
        SymbolKind::Function | SymbolKind::ExternFunction | SymbolKind::Method
    );

    let insert_text = if is_callable {
        if sym.parameters.is_empty() {
            format!("{}()$0", sym.name)
        } else {
            let placeholders: Vec<String> = sym
                .parameters
                .iter()
                .enumerate()
                .map(|(i, p)| {
                    let name = if p.name.is_empty() { "arg" } else { &p.name };
                    format!("${{{}:{}}}", i + 1, name)
                })
                .collect();
            format!("{}({})$0", sym.name, placeholders.join(", "))
        }
    } else {
        sym.name.clone()
    };

    let kind = match sym.kind {
        SymbolKind::Function | SymbolKind::ExternFunction => CompletionItemKind::FUNCTION,
        SymbolKind::Method => CompletionItemKind::METHOD,
        SymbolKind::Const => CompletionItemKind::CONSTANT,
        SymbolKind::Global => CompletionItemKind::VARIABLE,
        _ => CompletionItemKind::TEXT,
    };

    CompletionItem {
        label: sym.name.clone(),
        kind: Some(kind),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(insert_text),
        insert_text_format: if is_callable {
            Some(InsertTextFormat::SNIPPET)
        } else {
            Some(InsertTextFormat::PLAIN_TEXT)
        },
        sort_text: Some(format!("2_{}", sym.name)),
        ..Default::default()
    }
}

fn format_doc(doc_str: &str) -> Documentation {
    Documentation::MarkupContent(MarkupContent {
        kind: MarkupKind::Markdown,
        value: doc::doc_to_markdown(doc_str),
    })
}
