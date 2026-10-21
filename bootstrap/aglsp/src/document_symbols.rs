//! Outline / document-symbol tree for the open buffer.
//!
//! The symbol index stores a flat list; symbols of the open buffer are
//! top-level items (qualifier `None`) or children of a container
//! (qualifier `"Name::"` — methods, fields, variants).

use agc::symbol_index::{Symbol, SymbolIndex, SymbolKind as SymKind};
use rustc_hash::FxHashMap as HashMap;
use tower_lsp_server::ls_types::*;

use crate::util::byte_to_position;

pub(crate) fn document_symbols(analysis: &SymbolIndex) -> Vec<DocumentSymbol> {
    let text = &analysis.text;
    let mut children: HashMap<String, Vec<&Symbol>> = HashMap::default();
    let mut top: Vec<&Symbol> = Vec::new();

    for sym in &analysis.symbols {
        // Only symbols defined in the open buffer (inlined std/imported
        // definitions are tracked in `foreign_files`).
        if analysis.foreign_files.contains_key(&sym.span.file) {
            continue;
        }
        if matches!(
            sym.kind,
            SymKind::Parameter | SymKind::Local | SymKind::TypeParam
        ) {
            // Function parameters and locals are tracked for hover/rename but
            // are not outline items (they also lack a parent qualifier).
            continue;
        }
        match &sym.qualifier {
            Some(q) => {
                let parent = q.strip_suffix("::").unwrap_or(q);
                children.entry(parent.to_string()).or_default().push(sym);
            }
            None => top.push(sym),
        }
    }

    top.into_iter()
        .map(|sym| {
            let kids = children.remove(&sym.name).unwrap_or_default();
            to_document_symbol(sym, kids, text)
        })
        .collect()
}

#[allow(deprecated)]
fn to_document_symbol(sym: &Symbol, kids: Vec<&Symbol>, text: &str) -> DocumentSymbol {
    let range = span_to_range(sym, text);
    DocumentSymbol {
        name: sym.name.clone(),
        detail: (!sym.signature.is_empty()).then(|| sym.signature.clone()),
        kind: lsp_kind(sym.kind),
        tags: None,
        deprecated: None,
        range,
        selection_range: range,
        children: if kids.is_empty() {
            None
        } else {
            Some(
                kids.into_iter()
                    .map(|k| to_document_symbol(k, vec![], text))
                    .collect(),
            )
        },
    }
}

fn span_to_range(sym: &Symbol, text: &str) -> Range {
    Range {
        start: byte_to_position(text, sym.span.start),
        end: byte_to_position(text, sym.span.end),
    }
}

fn lsp_kind(kind: SymKind) -> SymbolKind {
    match kind {
        SymKind::Function | SymKind::Macro | SymKind::ExternFunction => SymbolKind::FUNCTION,
        SymKind::Method => SymbolKind::METHOD,
        SymKind::Struct => SymbolKind::STRUCT,
        SymKind::Enum => SymbolKind::ENUM,
        SymKind::Trait => SymbolKind::INTERFACE,
        SymKind::TypeAlias => SymbolKind::CLASS,
        SymKind::Global | SymKind::ExternVariable => SymbolKind::VARIABLE,
        SymKind::Const => SymbolKind::CONSTANT,
        SymKind::Field => SymbolKind::FIELD,
        SymKind::Variant => SymbolKind::ENUM_MEMBER,
        SymKind::Parameter | SymKind::Local => SymbolKind::VARIABLE,
        SymKind::TypeParam => SymbolKind::TYPE_PARAMETER,
    }
}
