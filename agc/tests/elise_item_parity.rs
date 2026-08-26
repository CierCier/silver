//! M3 gate: per-file top-level item counts and kinds must match the legacy
//! parser across the whole corpus.
//!
//! Files where the legacy parser itself reports parse errors are skipped
//! (both parsers are expected to be imperfect there in different ways).

use agc::grammar::{parse_ag, NodeKind, Tok};
use agc::parser::Parser;

fn collect(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "ag") {
            out.push(path);
        }
    }
}

fn corpus() -> Vec<(String, String)> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap();
    let mut files = Vec::new();
    for dir in ["std", "tests", "examples"] {
        collect(&root.join(dir), &mut files);
    }
    files.sort();
    files
        .into_iter()
        .map(|path| {
            let rel = path.strip_prefix(root).unwrap().display().to_string();
            let source = std::fs::read_to_string(&path).expect("corpus read");
            (rel, source)
        })
        .collect()
}

fn legacy_item_kinds(source: &str) -> Option<Vec<&'static str>> {
    let tokens = agc::lexer::lex_with_source(source, 0).ok()?;
    let mut parser = Parser::new(tokens);
    let (program, errors) = parser.parse_program();
    if !errors.is_empty() {
        return None; // legacy could not fully parse: skip strict parity
    }
    Some(
        program
            .items
            .iter()
            .map(|item| match &item.kind {
                agc::parser::ast::ItemKind::Import(_) => "Import",
                agc::parser::ast::ItemKind::ExternFunction(_) => "ExternDecl",
                agc::parser::ast::ItemKind::ExternVariable(_) => "ExternDecl",
                agc::parser::ast::ItemKind::ExternBlock(_) => "ExternBlock",
                agc::parser::ast::ItemKind::Struct(_) => "Struct",
                agc::parser::ast::ItemKind::Enum(_) => "Enum",
                agc::parser::ast::ItemKind::Trait(_) => "Trait",
                agc::parser::ast::ItemKind::Impl(_) => "Impl",
                agc::parser::ast::ItemKind::Function(_) => "Function",
                agc::parser::ast::ItemKind::GlobalVariable(_) => "GlobalVariable",
                agc::parser::ast::ItemKind::Macro(_) => "Macro",
                agc::parser::ast::ItemKind::TypeAlias(_) => "TypeAlias",
                _ => "Other",
            })
            .collect(),
    )
}

/// M3 gate part 1: item-count/kind parity over the corpus.
#[test]
fn item_parity_over_corpus() {
    let mut checked = 0usize;
    let mut skipped = 0usize;

    for (name, source) in corpus() {
        let Some(expected) = legacy_item_kinds(&source) else {
            skipped += 1;
            continue;
        };

        let graph = parse_ag(&source);
        let elise_items: Vec<&'static str> = graph
            .root()
            .children()
            .filter(|c| {
                // Skip trivia leaves (kinds at/above the trivia base) and
                // attribute wrappers: legacy attaches attributes to the
                // following item rather than listing them separately.
                c.kind() < Tok::TriviaLayout as u16
                    && c.kind() != NodeKind::Attribute as u16
            })
            .map(|c| NodeKind::from_u16(c.kind()).map(|k| kind_name(k)).unwrap_or("UNKNOWN"))
            .collect();

        assert_eq!(
            elise_items, expected,
            "{name}: item sequence differs (raw kinds: {:?})",
            graph.root().children().map(|c| c.kind()).collect::<Vec<_>>()
        );
        checked += 1;
    }

    eprintln!("elise item parity verified over {checked} files ({skipped} skipped)");
    assert!(checked > 200);
}

fn kind_name(kind: NodeKind) -> &'static str {
    match kind {
        NodeKind::File => "File",
        NodeKind::Import => "Import",
        NodeKind::ExternDecl => "ExternDecl",
        NodeKind::ExternBlock => "ExternBlock",
        NodeKind::Struct => "Struct",
        NodeKind::Enum => "Enum",
        NodeKind::Trait => "Trait",
        NodeKind::Impl => "Impl",
        NodeKind::Macro => "Macro",
        NodeKind::TypeAlias => "TypeAlias",
        NodeKind::Function => "Function",
        NodeKind::GlobalVariable => "GlobalVariable",
        NodeKind::Attribute => "Attribute",
    }
}
