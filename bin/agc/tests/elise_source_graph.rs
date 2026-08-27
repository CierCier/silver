//! M2 gate part 3: the Silver grammar's own tokens, over the whole corpus.
//!
//! Every repo file must produce a lossless source graph: the graph text is
//! byte-identical to the input, and the root covers exactly the file.

use agc::grammar::SilverLexSpec;
use elise_lex::LexSpec as _;
use elise_parse::{Event, TreeBuilder};

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

#[test]
fn source_graph_round_trip_over_corpus() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .parent()
        .unwrap();
    let mut files = Vec::new();
    for dir in ["std", "tests", "examples"] {
        collect(&root.join(dir), &mut files);
    }
    assert!(files.len() > 200, "corpus discovery broke");

    for path in &files {
        let source = std::fs::read_to_string(path).expect("corpus read");
        let mut spec = SilverLexSpec::new();
        let buf = match elise_lex::scan(&mut spec, &source) {
            Ok(buf) => buf,
            Err(err) => panic!("{}: lex failed: {err:?}", path.display()),
        };
        let significant = buf.rows().iter().filter(|r| r.len > 0).count();
        let events = [Event::Advance(significant as u32)];
        let graph = TreeBuilder::new(&source, &buf).finish(&events);

        assert_eq!(
            graph.text(),
            &source[..],
            "{}: source graph is not lossless",
            path.display()
        );
        let (start, end) = graph.root().span();
        assert_eq!(
            (start, end),
            (0, source.len()),
            "{}: root span does not cover the file",
            path.display()
        );
    }
}
