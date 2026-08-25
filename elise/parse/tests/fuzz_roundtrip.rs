//! M2 gate part 2: deterministic mutation fuzzing.
//!
//! Random byte mutations of seed strings must never break losslessness —
//! whenever the lexer succeeds, the tree must round-trip exactly.

use elise_lex::{
    CommonKinds, KeywordMap, LexError, LexSpec, OpTrie, TokenBuf,
};

#[derive(Debug, Clone, Copy, Default)]
struct ByteSpec;

fn common() -> &'static CommonKinds {
    static C: std::sync::OnceLock<CommonKinds> = std::sync::OnceLock::new();
    C.get_or_init(|| CommonKinds {
        layout: 3,
        line_comment: 4,
        doc_line_comment: 5,
        block_comment: 6,
        doc_block_comment: 7,
        eof: 0,
    })
}
fn trie() -> &'static OpTrie {
    static T: std::sync::OnceLock<OpTrie> = std::sync::OnceLock::new();
    T.get_or_init(OpTrie::default)
}
fn keywords() -> &'static KeywordMap {
    static K: std::sync::OnceLock<KeywordMap> = std::sync::OnceLock::new();
    K.get_or_init(KeywordMap::default)
}

impl LexSpec for ByteSpec {
    fn common_kinds(&self) -> &CommonKinds {
        common()
    }
    fn op_trie(&self) -> &OpTrie {
        trie()
    }
    fn keywords(&self) -> &KeywordMap {
        keywords()
    }
    fn comment_config(&self) -> Option<elise_lex::CommentConfig> {
        None
    }
    fn is_layout(&self, b: u8) -> bool {
        matches!(b, b' ' | b'\t' | b'\n')
    }
    fn ident_start(&self, _b: u8) -> bool {
        false
    }
    fn ident_continue(&self, _b: u8) -> bool {
        false
    }
    fn ident_kind(&self) -> u16 {
        1
    }
    fn scan_other(
        &mut self,
        bytes: &[u8],
        pos: usize,
        buf: &mut TokenBuf,
    ) -> Result<usize, LexError> {
        buf.push_token(1, pos as u32, 1);
        Ok(pos + 1)
    }
}

fn xorshift(state: &mut u64) -> u64 {
    *state ^= *state << 13;
    *state ^= *state >> 7;
    *state ^= *state << 17;
    *state
}

const ALPHABET: &[u8] = b"abc01_ +*()\n\"";

#[test]
fn mutated_inputs_round_trip_when_they_lex() {
    let mut rng: u64 = 0x5EED_1234_ABCD_0001;

    let seeds = ["fn main() {}", "a + b", "  x  ", "let it go"];

    let mut checked = 0usize;
    let mut rejected = 0usize;

    for iteration in 0..4000u32 {
        let r = xorshift(&mut rng);
        let seed = seeds[(r % seeds.len() as u64) as usize];
        let mut src = seed.as_bytes().to_vec();

        // Apply 1..=4 random edits: insert / delete / replace using alphabet
        // bytes only, so every byte remains claimable by the spec.
        let edits = 1 + (r % 4) as usize;
        for edit in 0..edits {
            let rr = xorshift(&mut rng);
            match rr % 3 {
                0 => {
                    let at = (rr >> 8) as usize % (src.len() + 1);
                    let byte = ALPHABET[((rr >> 16) as usize) % ALPHABET.len()];
                    src.insert(at.min(src.len()), byte);
                }
                1 if !src.is_empty() => {
                    let at = (rr >> 8) as usize % src.len();
                    src.remove(at);
                }
                _ if !src.is_empty() => {
                    let at = (rr >> 8) as usize % src.len();
                    src[at] = ALPHABET[((rr >> 16) as usize) % ALPHABET.len()];
                }
                _ => {}
            }
            let _ = edit;
        }

        let Ok(source_str) = std::str::from_utf8(&src) else {
            rejected += 1;
            continue;
        };

        let mut spec = ByteSpec;
        match elise_lex::scan(&mut spec, source_str) {
            Ok(buf) => {
                let significant = buf.rows().iter().filter(|r| r.len > 0).count();
                let events = vec![elise_parse::Event::Advance(significant as u32)];
                let graph =
                    elise_parse::TreeBuilder::new(source_str, &buf).finish(&events);
                assert_eq!(
                    graph.text(),
                    source_str,
                    "round-trip broke on iteration {iteration}"
                );
                checked += 1;
            }
            Err(_) => rejected += 1,
        }
    }

    assert!(
        checked > 3000,
        "fuzzer produced too few valid cases: {checked} (rejected {rejected})"
    );
    eprintln!("mutation fuzz: {checked} round-tripped, {rejected} rejected");
}
