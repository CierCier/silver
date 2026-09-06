//! Member access and static member completion (`expr.` / `Type::` / `Type.`).

use agc::symbol_index::{Symbol, SymbolIndex, SymbolKind, type_root_name_of_str};
use tower_lsp_server::ls_types::*;

use crate::doc;

pub(crate) fn complete_member(
    analysis: &SymbolIndex,
    receiver_text: &str,
    member_prefix: &str,
    is_double_colon: bool,
    recv_span: (usize, usize),
) -> Option<Vec<CompletionItem>> {
    let container_name = resolve_container_name(analysis, receiver_text, recv_span)?;
    let is_direct_type_name = analysis.symbols.iter().any(|s| {
        s.name == receiver_text && matches!(s.kind, SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait)
    });
    let is_static_call = is_double_colon || is_direct_type_name;

    let qualifier = format!("{container_name}::");
    let mut items = Vec::new();

    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() != Some(qualifier.as_str()) {
            continue;
        }
        if !member_prefix.is_empty() && !sym.name.starts_with(member_prefix) {
            continue;
        }

        match sym.kind {
            SymbolKind::Field if !is_static_call => {
                items.push(field_item(sym));
            }
            SymbolKind::Variant => {
                items.push(variant_item(sym));
            }
            SymbolKind::Method => {
                if is_static_call && sym.is_static {
                    items.push(static_method_item(sym));
                } else if !is_static_call && !sym.is_static {
                    items.push(instance_method_item(sym));
                }
            }
            SymbolKind::TypeAlias | SymbolKind::Const if is_static_call => {
                items.push(associated_item(sym));
            }
            _ => {}
        }
    }

    if items.is_empty() {
        None
    } else {
        Some(items)
    }
}

fn resolve_container_name(
    analysis: &SymbolIndex,
    receiver_text: &str,
    recv_span: (usize, usize),
) -> Option<String> {
    // 1. Direct type checker inferred expression type
    if let Some(ty) = analysis.expr_types.get(&recv_span) {
        if let Some(root) = type_root_name_of_str(ty) {
            return Some(root);
        }
    }

    // 2. Direct struct, enum, or trait name
    if analysis.symbols.iter().any(|s| {
        s.name == receiver_text
            && matches!(
                s.kind,
                SymbolKind::Struct | SymbolKind::Enum | SymbolKind::Trait | SymbolKind::TypeAlias
            )
    }) {
        return Some(receiver_text.to_string());
    }

    // 3. Local variable or parameter in scope
    for s in &analysis.symbols {
        if s.name == receiver_text && matches!(s.kind, SymbolKind::Local | SymbolKind::Parameter) {
            if let Some(ty) = s.inferred_type.as_deref().or_else(|| {
                let parts: Vec<&str> = s.signature.split_whitespace().collect();
                if parts.len() >= 2 {
                    Some(parts[0])
                } else {
                    None
                }
            }) {
                if let Some(root) = type_root_name_of_str(ty) {
                    return Some(root);
                }
            }
        }
    }

    None
}

fn field_item(sym: &Symbol) -> CompletionItem {
    CompletionItem {
        label: sym.name.clone(),
        kind: Some(CompletionItemKind::FIELD),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(sym.name.clone()),
        insert_text_format: Some(InsertTextFormat::PLAIN_TEXT),
        sort_text: Some(format!("0_{}", sym.name)),
        ..Default::default()
    }
}

fn variant_item(sym: &Symbol) -> CompletionItem {
    let has_payload = sym.signature.contains('(') || sym.signature.contains('{');
    let insert_text = if has_payload {
        format!("{}($1)$0", sym.name)
    } else {
        sym.name.clone()
    };

    CompletionItem {
        label: sym.name.clone(),
        kind: Some(CompletionItemKind::ENUM_MEMBER),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(insert_text),
        insert_text_format: if has_payload {
            Some(InsertTextFormat::SNIPPET)
        } else {
            Some(InsertTextFormat::PLAIN_TEXT)
        },
        sort_text: Some(format!("0_{}", sym.name)),
        ..Default::default()
    }
}

fn instance_method_item(sym: &Symbol) -> CompletionItem {
    let extra_params = if sym.parameters.len() > 1 {
        &sym.parameters[1..]
    } else {
        &[]
    };

    let insert_text = if extra_params.is_empty() {
        format!("{}()$0", sym.name)
    } else {
        let placeholders: Vec<String> = extra_params
            .iter()
            .enumerate()
            .map(|(i, p)| {
                let name = if p.name.is_empty() { "arg" } else { &p.name };
                format!("${{{}:{}}}", i + 1, name)
            })
            .collect();
        format!("{}({})$0", sym.name, placeholders.join(", "))
    };

    CompletionItem {
        label: sym.name.clone(),
        kind: Some(CompletionItemKind::METHOD),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(insert_text),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_text: Some(format!("1_{}", sym.name)),
        ..Default::default()
    }
}

fn static_method_item(sym: &Symbol) -> CompletionItem {
    let insert_text = if sym.parameters.is_empty() {
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
    };

    CompletionItem {
        label: sym.name.clone(),
        kind: Some(CompletionItemKind::METHOD),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(insert_text),
        insert_text_format: Some(InsertTextFormat::SNIPPET),
        sort_text: Some(format!("1_{}", sym.name)),
        ..Default::default()
    }
}

fn associated_item(sym: &Symbol) -> CompletionItem {
    let kind = if sym.kind == SymbolKind::Const {
        CompletionItemKind::CONSTANT
    } else {
        CompletionItemKind::CLASS
    };
    CompletionItem {
        label: sym.name.clone(),
        kind: Some(kind),
        detail: Some(sym.signature.clone()),
        documentation: sym.doc.as_deref().map(format_doc),
        insert_text: Some(sym.name.clone()),
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
