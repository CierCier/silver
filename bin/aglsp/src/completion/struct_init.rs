//! Struct literal and designated initializer completion (`{ .field = ... }`).

use agc::symbol_index::{SymbolIndex, SymbolKind};
use tower_lsp_server::ls_types::*;

pub(crate) fn complete_struct_init(
    analysis: &SymbolIndex,
    struct_name: &str,
    field_prefix: &str,
    existing_fields: &[String],
) -> Option<Vec<CompletionItem>> {
    let qualifier = format!("{struct_name}::");
    let mut all_fields = Vec::new();
    let mut missing_fields = Vec::new();

    for sym in &analysis.symbols {
        if sym.qualifier.as_deref() == Some(qualifier.as_str()) && sym.kind == SymbolKind::Field {
            all_fields.push(sym);
            if !existing_fields.contains(&sym.name) {
                missing_fields.push(sym);
            }
        }
    }

    if all_fields.is_empty() {
        return None;
    }

    let mut items = Vec::new();

    // 1. "Fill all fields" snippet if more than 1 field is unassigned
    if missing_fields.len() > 1 && field_prefix.is_empty() {
        let fill_all_snippet: Vec<String> = missing_fields
            .iter()
            .enumerate()
            .map(|(i, f)| format!(".{} = ${{{}:{}}}", f.name, i + 1, f.name))
            .collect();

        items.push(CompletionItem {
            label: format!("... Fill all {} fields", missing_fields.len()),
            kind: Some(CompletionItemKind::SNIPPET),
            detail: Some(format!("Initialize all remaining fields of {struct_name}")),
            insert_text: Some(fill_all_snippet.join(", ")),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some("0_00_fill_all".to_string()),
            ..Default::default()
        });
    }

    // 2. Struct fields (unassigned fields sorted first)
    for sym in all_fields {
        let clean_prefix = field_prefix.trim_start_matches('.');
        if !clean_prefix.is_empty() && !sym.name.starts_with(clean_prefix) {
            continue;
        }

        let is_missing = missing_fields.iter().any(|f| f.name == sym.name);
        let sort_prefix = if is_missing { "0" } else { "1" };

        items.push(CompletionItem {
            label: format!(".{}", sym.name),
            kind: Some(CompletionItemKind::FIELD),
            detail: Some(sym.signature.clone()),
            insert_text: Some(format!(".{} = $1", sym.name)),
            insert_text_format: Some(InsertTextFormat::SNIPPET),
            sort_text: Some(format!("{sort_prefix}_{}", sym.name)),
            filter_text: Some(format!(".{}", sym.name)),
            ..Default::default()
        });
    }

    if items.is_empty() {
        None
    } else {
        Some(items)
    }
}
