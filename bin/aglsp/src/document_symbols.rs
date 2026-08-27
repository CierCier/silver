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

#[cfg(test)]
mod tests {
    use super::*;
    use agc::lexer;
    use agc::semantic::typeck::TypeChecker;
    use agc::symbol_index::analyze;
    use agc::symbol_table::CompilerSymbolTable;

    fn analyze_src(source: &str) -> SymbolIndex {
        let file_id = lexer::register_source("/tmp/test_buffer.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        assert!(!graph.has_errors(), "parse errors: {:?}", graph.errors());
        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (_, _) = tc.check_program_with_table(&mut program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);
        analyze(&program, source, &tokens, expr_types, file_id)
    }

    #[test]
    fn outline_lists_top_level_and_children() {
        let analysis = analyze_src(
            "struct Point { i64 x; i64 y; }\n\
             impl Point { i64 sum(&Point self) { return self.x + self.y; } }\n\
             enum Color { Red; Green; }\n\
             i32 main() { return 0; }",
        );
        let symbols = document_symbols(&analysis);
        let names: Vec<&str> = symbols.iter().map(|s| s.name.as_str()).collect();
        assert_eq!(
            names,
            vec!["Point", "Color", "main"],
            "top-level order: {names:?}"
        );
        let point = &symbols[0];
        assert_eq!(point.kind, SymbolKind::STRUCT);
        let children = point.children.as_ref().unwrap();
        let child_names: Vec<&str> = children.iter().map(|c| c.name.as_str()).collect();
        assert_eq!(
            child_names,
            vec!["x", "y", "sum"],
            "Point children: {child_names:?}"
        );
        assert_eq!(children[0].kind, SymbolKind::FIELD);
        assert_eq!(children[2].kind, SymbolKind::METHOD);
        let color = &symbols[1];
        let variants: Vec<&str> = color
            .children
            .as_ref()
            .unwrap()
            .iter()
            .map(|c| c.name.as_str())
            .collect();
        assert_eq!(variants, vec!["Red", "Green"]);
    }

    #[test]
    fn detail_carries_signature() {
        let analysis = analyze_src("i32 add(i32 a, i32 b) { return a + b; }");
        let symbols = document_symbols(&analysis);
        assert_eq!(symbols.len(), 1);
        assert!(symbols[0].detail.as_deref().unwrap_or("").contains("add"));
    }
}
