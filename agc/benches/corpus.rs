//! Baseline performance harness for the legacy hand-written lexer/parser,
//! measured over the entire Silver corpus (`std/`, `tests/`, `examples/`).
//!
//! This is the reference line every elise milestone is compared against.
//! See PERFLOG.md at the repo root. This bench is expected to be deleted at
//! parser cutover, when elise's own benches take over.

use std::fs;
use std::path::{Path, PathBuf};

use criterion::{black_box, criterion_group, criterion_main, Criterion, Throughput};

fn collect(dir: &Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = fs::read_dir(dir) else {
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

/// All corpus sources, grouped by top-level directory.
fn corpus() -> Vec<(String, String)> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).parent().unwrap();
    let mut files = Vec::new();
    for dir in ["std", "tests", "examples"] {
        collect(&root.join(dir), &mut files);
    }
    files.sort();
    files
        .into_iter()
        .map(|path| {
            let rel = path.strip_prefix(root).unwrap().display().to_string();
            let source = fs::read_to_string(&path).expect("corpus read");
            (rel, source)
        })
        .collect()
}

fn subset<'a>(corpus: &'a [(String, String)], prefix: &str) -> Vec<(&'a str, &'a str)> {
    corpus
        .iter()
        .filter(|(name, _)| name.starts_with(prefix))
        .map(|(name, src)| (name.as_str(), src.as_str()))
        .collect()
}

fn bytes_and_lines(sources: &[(&str, &str)]) -> (u64, u64) {
    let mut bytes = 0;
    let mut lines = 0;
    for (_, src) in sources {
        bytes += src.len() as u64;
        lines += src.lines().count() as u64;
    }
    (bytes, lines)
}

fn bench_lex_group(c: &mut Criterion, name: &str, sources: &[(&str, &str)]) {
    let (bytes, _lines) = bytes_and_lines(sources);
    if bytes == 0 {
        return;
    }
    let mut group = c.benchmark_group(format!("lex/{name}"));
    group.throughput(Throughput::Bytes(bytes));
    group.bench_function(name.to_string(), |b| {
        b.iter(|| {
            for (_, src) in sources {
                let tokens = agc::lexer::lex(black_box(src)).expect("lex");
                black_box(tokens.len());
            }
        })
    });
    group.finish();
}

fn bench_parse_group(c: &mut Criterion, name: &str, sources: &[(&str, &str)]) {
    // Pre-tokenize outside the timer: this bench isolates parsing.
    let tokenized: Vec<_> = sources
        .iter()
        .map(|(_, src)| agc::lexer::lex(src).expect("lex"))
        .collect();
    let (bytes, _lines) = bytes_and_lines(sources);
    if bytes == 0 {
        return;
    }
    let mut group = c.benchmark_group(format!("parse/{name}"));
    group.throughput(Throughput::Bytes(bytes));
    group.bench_function(name.to_string(), |b| {
        b.iter(|| {
            for tokens in &tokenized {
                let mut parser = agc::parser::Parser::new(tokens.clone());
                let (program, _) = parser.parse_program();
                black_box(program.items.len());
            }
        })
    });
    group.finish();
}

fn bench_pipeline_group(c: &mut Criterion, name: &str, sources: &[(&str, &str)]) {
    let (bytes, _lines) = bytes_and_lines(sources);
    if bytes == 0 {
        return;
    }
    let mut group = c.benchmark_group(format!("pipeline/{name}"));
    group.throughput(Throughput::Bytes(bytes));
    group.bench_function(name.to_string(), |b| {
        b.iter(|| {
            for (_, src) in sources {
                let tokens = agc::lexer::lex(black_box(src)).expect("lex");
                let mut parser = agc::parser::Parser::new(tokens);
                let (program, _) = parser.parse_program();
                black_box(program.items.len());
            }
        })
    });
    group.finish();
}

fn bench_elise_lex_group(c: &mut Criterion, name: &str, sources: &[(&str, &str)]) {
    let (bytes, _lines) = bytes_and_lines(sources);
    if bytes == 0 {
        return;
    }
    let mut group = c.benchmark_group(format!("elise_lex/{name}"));
    group.throughput(Throughput::Bytes(bytes));
    group.bench_function(name.to_string(), |b| {
        b.iter(|| {
            for (_, src) in sources {
                let mut spec = agc::grammar::SilverLexSpec::new();
                let buf = elise_lex::scan(&mut spec, black_box(src)).expect("lex");
                black_box(buf.len());
            }
        })
    });
    group.finish();
}

fn corpus_benches(c: &mut Criterion) {
    let corpus = corpus();
    assert!(!corpus.is_empty(), "corpus discovery found no .ag files");

    for (prefix, label) in [("std", "std"), ("tests", "tests"), ("examples", "examples")] {
        let sources = subset(&corpus, prefix);
        bench_lex_group(c, label, &sources);
        bench_parse_group(c, label, &sources);
        bench_pipeline_group(c, label, &sources);
    }

    let all: Vec<(&str, &str)> = corpus
        .iter()
        .map(|(name, src)| (name.as_str(), src.as_str()))
        .collect();
    bench_lex_group(c, "all", &all);
    bench_parse_group(c, "all", &all);
    bench_pipeline_group(c, "all", &all);
    // elise M1 comparison point: same corpus, elise-lex Silver spec.
    for (prefix, label) in [("std", "std"), ("tests", "tests"), ("examples", "examples")] {
        let sources = subset(&corpus, prefix);
        bench_elise_lex_group(c, label, &sources);
    }
    bench_elise_lex_group(c, "all", &all);

    let (_, lines) = bytes_and_lines(&all);
    eprintln!(
        "DBG CORPUS: {} files, {} bytes, {} lines",
        corpus.len(),
        bytes_and_lines(&all).0,
        lines
    );
}

criterion_group!(
    name = benches;
    config = Criterion::default().measurement_time(std::time::Duration::from_secs(5));
    targets = corpus_benches
);
criterion_main!(benches);
