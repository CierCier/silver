//! M2 gate: lossless round-trip through the event fold + green tree.

use elise_lex::{CommonKinds, KeywordMap, LexError, LexSpec, OpTrie, TokenBuf};
use elise_parse::{Event, TreeBuilder};

/// Minimal identifier/operator/whitespace spec for exercising the fold.
#[derive(Debug, Clone, Copy)]
struct MiniSpec {
    common: CommonKinds,
}

impl MiniSpec {
    fn new() -> Self {
        MiniSpec {
            common: CommonKinds {
                layout: LAYOUT,
                line_comment: 4,
                doc_line_comment: 5,
                block_comment: 6,
                doc_block_comment: 7,
                eof: 0,
            },
        }
    }
}

const IDENT: u16 = 1;
const OP: u16 = 2;
const LAYOUT: u16 = 3;

impl LexSpec for MiniSpec {
    fn common_kinds(&self) -> &CommonKinds {
        &self.common
    }
    fn op_trie(&self) -> &OpTrie {
        // Operators route through `scan_other` in this mini spec; an empty
        // trie makes the driver's longest-match step a no-op.
        static EMPTY: std::sync::OnceLock<OpTrie> = std::sync::OnceLock::new();
        EMPTY.get_or_init(OpTrie::default)
    }
    fn keywords(&self) -> &KeywordMap {
        static EMPTY: std::sync::OnceLock<KeywordMap> = std::sync::OnceLock::new();
        EMPTY.get_or_init(KeywordMap::default)
    }
    fn comment_config(&self) -> Option<elise_lex::CommentConfig> {
        None
    }
    fn is_layout(&self, byte: u8) -> bool {
        matches!(byte, b' ' | b'\t' | b'\n')
    }
    fn ident_start(&self, _byte: u8) -> bool {
        false // everything non-layout goes through scan_other
    }
    fn ident_continue(&self, _byte: u8) -> bool {
        false
    }
    fn ident_kind(&self) -> u16 {
        IDENT
    }

    fn scan_other(
        &mut self,
        bytes: &[u8],
        pos: usize,
        buf: &mut TokenBuf,
    ) -> Result<usize, LexError> {
        // Total spec: any other single byte is an OP leaf, so the driver can
        // never fail on test inputs.
        buf.push_token(OP as u16, pos as u32, 1);
        Ok(pos + 1)
    }
}

fn lex(src: &str) -> Result<TokenBuf, LexError> {
    elise_lex::scan(&mut MiniSpec::new(), src)
}

fn flat_events(src: &str) -> (TokenBuf, Vec<Event>) {
    let tokens = lex(src).expect("lex");
    let significant = tokens.rows().iter().filter(|r| r.len > 0).count();
    // Root is pre-seeded by the builder; a bare Advance consumes everything.
    let events = vec![Event::Advance(significant as u32)];
    (tokens, events)
}

fn assert_round_trip(src: &str) {
    let (tokens, events) = flat_events(src);
    let graph = TreeBuilder::new(src, &tokens)
        .root_kind(0)
        .errors(Vec::new())
        .finish(&events);
    assert_eq!(
        graph.text(),
        src,
        "lossless round-trip failed for {src:?}"
    );
    let (start, end) = graph.root().span();
    assert_eq!((start, end), (0, src.len()));
}

#[test]
fn round_trip_basic() {
    assert_round_trip("a + b");
    assert_round_trip("");
    assert_round_trip("   \n\t  ");
    assert_round_trip("foo(bar_1, baz)*quux");
}

#[test]
fn round_trip_utf8_content() {
    // Byte-safe even though Silver identifiers are ASCII: the tree layer
    // only tracks widths.
    assert_round_trip("\"héllo\" + \"wörld\"");
    assert_round_trip("x = 1; // comment: 日本語テキスト\nlet y = 2;");
}

#[test]
fn structured_tree_spans_and_kinds() {
    // Tokens: `a` `+` `bc` with one space of trivia before each.
    let src = "a + bc";
    let (tokens, _) = flat_events(src);

    // Tree convention: the builder pre-seeds the root from `root_kind`, so
    // events here describe the ROOT'S CHILDREN:
    //   root(0) -> [ node(10){ "a" }, node(20){ " + bc" } ]
    let events = vec![
        Event::Enter(10),
        Event::Advance(1), // a
        Event::Exit,
        Event::Enter(20),
        Event::Advance(2), // + bc
        Event::Exit,
    ];
    let graph = TreeBuilder::new(src, &tokens)
        .root_kind(0)
        .finish(&events);

    let root = graph.root();
    assert_eq!(root.text(), src);
    eprintln!("DBG children dump:");
    for c in root.children() {
        eprintln!("  kind={} span={:?} text={:?}", c.kind(), c.span(), c.text());
    }
    let kids: Vec<_> = root.children().collect();
    assert_eq!(kids.len(), 3);

    let first = kids[0];
    assert_eq!(first.kind(), 10);
    // Node 10 closed before any trivia existed, so it covers only "a".
    assert_eq!(first.span(), (0, 1));
    assert_eq!(first.text(), "a");
    assert!(!first.is_leaf());

    let second = kids[1];
    assert_eq!(second.kind(), 20);
    // Trivia preceding "+" flushes into node 20 at Advance time.
    assert_eq!(second.span(), (1, 5));
    assert_eq!(second.text(), " + b");

    // Unconsumed significant leaves fall back into the root.
    let third = kids[2];
    assert!(third.is_leaf());
    assert_eq!(third.text(), "c");

    // Leaf walk sees every byte exactly once.
    let mut seen = String::new();
    root.walk_leaves(&mut |_, text| seen.push_str(text));
    assert_eq!(seen, src);
}
