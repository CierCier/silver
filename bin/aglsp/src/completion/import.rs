//! Module path and selective import completion (`import std.io.` / `import std.io { ... }`).

use agc::symbol_index::{SymbolIndex, SymbolKind};
use tower_lsp_server::ls_types::*;

use crate::util::find_std_search_dirs;

pub(crate) fn complete_import_path(
    analysis: &SymbolIndex,
    segments: &[String],
    prefix: &str,
    after_dot: bool,
) -> Vec<CompletionItem> {
    if segments.is_empty() && !after_dot {
        return root_module_completion(analysis, prefix);
    }
    module_dir_completion(segments, prefix)
}

pub(crate) fn complete_selective_import(
    analysis: &SymbolIndex,
    module_path: &[String],
    prefix: &str,
) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    let mod_prefix = format!("{}.", module_path.join("."));
    let mod_exact = module_path.join(".");

    for sym in &analysis.symbols {
        let is_match = sym.qualifier.as_deref().map_or(false, |q| {
            q.starts_with(&mod_prefix) || q.trim_end_matches("::") == mod_exact
        });
        if !is_match && !sym.name.starts_with(prefix) {
            continue;
        }
        if !prefix.is_empty() && !sym.name.starts_with(prefix) {
            continue;
        }

        let kind = match sym.kind {
            SymbolKind::Function | SymbolKind::ExternFunction => CompletionItemKind::FUNCTION,
            SymbolKind::Struct => CompletionItemKind::STRUCT,
            SymbolKind::Enum => CompletionItemKind::ENUM,
            SymbolKind::Trait => CompletionItemKind::INTERFACE,
            SymbolKind::Const | SymbolKind::Global => CompletionItemKind::CONSTANT,
            _ => continue,
        };

        items.push(CompletionItem {
            label: sym.name.clone(),
            kind: Some(kind),
            detail: Some(sym.signature.clone()),
            insert_text: Some(sym.name.clone()),
            sort_text: Some(format!("0_{}", sym.name)),
            ..Default::default()
        });
    }

    items
}

fn root_module_completion(analysis: &SymbolIndex, prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for dir in find_std_search_dirs() {
        collect_module_entries(&dir, prefix, &mut items);
    }
    for path in &analysis.import_paths {
        if let Some(root) = path.first() {
            if root.starts_with(prefix) {
                items.push(module_item(root, ""));
            }
        }
    }
    items
}

fn module_dir_completion(segments: &[String], prefix: &str) -> Vec<CompletionItem> {
    let mut items = Vec::new();
    for dir in find_std_search_dirs() {
        let mut path = dir;
        for seg in segments {
            path = path.join(seg);
        }
        collect_module_entries(&path, prefix, &mut items);
    }
    items
}

fn collect_module_entries(dir: &std::path::Path, prefix: &str, items: &mut Vec<CompletionItem>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let name = entry.file_name().to_string_lossy().into_owned();
        let is_dir = entry.file_type().map(|t| t.is_dir()).unwrap_or(false);
        if !is_dir && !name.ends_with(".ag") {
            continue;
        }
        let stem = if is_dir {
            name.clone()
        } else {
            name.strip_suffix(".ag").unwrap_or(&name).to_string()
        };
        if stem.is_empty() || !stem.starts_with(prefix) {
            continue;
        }
        items.push(module_item(&stem, ""));
    }
}

fn module_item(stem: &str, detail: &str) -> CompletionItem {
    CompletionItem {
        label: stem.to_string(),
        kind: Some(CompletionItemKind::MODULE),
        detail: if detail.is_empty() {
            Some("module".to_string())
        } else {
            Some(detail.to_string())
        },
        insert_text: Some(stem.to_string()),
        ..Default::default()
    }
}
