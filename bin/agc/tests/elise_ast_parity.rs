//! Differential AST Parity test: compares `lower_source_graph(&parse_ag(src))`
//! against the legacy `Parser::parse_program()`.

use agc::grammar::{lower_source_graph, parse_ag};
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
        .unwrap()
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

#[test]
fn ast_lowering_smoketest() {
    let src = "import std.io;\ni32 main() { return 0; }\n";
    let graph = parse_ag(src);
    let program = lower_source_graph(&graph, 0);

    assert_eq!(program.items.len(), 2);
}

#[test]
fn ast_lowering_corpus_smoketest() {
    let mut checked = 0usize;
    let mut skipped = 0usize;

    for (name, source) in corpus() {
        let tokens = match agc::lexer::lex_with_source(&source, 0) {
            Ok(toks) => toks,
            Err(_) => {
                skipped += 1;
                continue;
            }
        };
        let mut parser = Parser::new(tokens);
        let (expected_prog, errors) = parser.parse_program();
        if !errors.is_empty() {
            skipped += 1;
            continue;
        }

        let graph = parse_ag(&source);
        let actual_prog = lower_source_graph(&graph, 0);

        assert_eq!(
            actual_prog.items.len(),
            expected_prog.items.len(),
            "item count mismatch in {name}: expected {}, got {}",
            expected_prog.items.len(),
            actual_prog.items.len()
        );

        checked += 1;
    }

    assert!(checked > 200, "checked {checked} corpus files (skipped {skipped})");
}
