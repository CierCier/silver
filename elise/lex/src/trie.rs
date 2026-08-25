//! Longest-match operator/punctuation trie.
//!
//! Built at spec-construction time from a list of `(lexeme, kind)` pairs;
//! lookup walks one byte at a time and remembers the deepest match, so `<=`
//! wins over `<`, `<<=` wins over `<<`, etc. Flat storage: children live in
//! a single Vec per node — tiny, cache-friendly, no allocations during scan.

/// Longest-match trie for fixed operator lexemes.
#[derive(Debug, Default, Clone)]
pub struct OpTrie {
    nodes: Vec<Node>,
}

#[derive(Debug, Default, Clone)]
struct Node {
    /// `(byte, child_index)` edges, sorted by byte for binary search when
    /// fan-out grows; linear scan is faster for the small fan-outs here.
    edges: Vec<(u8, u32)>,
    /// Set when a lexeme terminates at this node.
    kind: Option<u16>,
}

impl OpTrie {
    pub fn new(operators: &[(&str, u16)]) -> Self {
        let mut trie = OpTrie {
            nodes: vec![Node::default()],
        };
        for (lexeme, kind) in operators {
            trie.insert(lexeme.as_bytes(), *kind);
        }
        trie
    }

    fn insert(&mut self, lexeme: &[u8], kind: u16) {
        let mut node = 0u32;
        for &b in lexeme {
            let next = self.nodes[node as usize]
                .edges
                .iter()
                .find(|(edge_byte, _)| *edge_byte == b)
                .map(|(_, idx)| *idx);
            node = match next {
                Some(idx) => idx,
                None => {
                    let idx = self.nodes.len() as u32;
                    self.nodes.push(Node::default());
                    self.nodes[node as usize].edges.push((b, idx));
                    idx
                }
            };
        }
        self.nodes[node as usize].kind = Some(kind);
    }

    /// Longest match against `bytes` starting at offset 0.
    /// Returns `(kind, matched_len)` or `None` when no lexeme matches.
    #[inline]
    pub fn longest_match(&self, bytes: &[u8]) -> Option<(u16, usize)> {
        let mut node = 0u32;
        let mut best: Option<(u16, usize)> = None;
        for (offset, &b) in bytes.iter().enumerate() {
            let edges = &self.nodes[node as usize].edges;
            match edges.iter().find(|(edge_byte, _)| *edge_byte == b) {
                Some((_, idx)) => {
                    node = *idx;
                    if let Some(kind) = self.nodes[node as usize].kind {
                        best = Some((kind, offset + 1));
                    }
                }
                None => break,
            }
        }
        best
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        // Only the root exists => no operators registered.
        self.nodes.len() <= 1 && self.nodes[0].edges.is_empty()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn longest_match_wins() {
        let trie = OpTrie::new(&[
            ("<", 1),
            ("<<", 2),
            ("<<=", 3),
            ("<=", 4),
            ("..", 5),
            ("...", 6),
        ]);
        assert_eq!(trie.longest_match(b"<<= x"), Some((3, 3)));
        assert_eq!(trie.longest_match(b"<< "), Some((2, 2)));
        assert_eq!(trie.longest_match(b"<= "), Some((4, 2)));
        assert_eq!(trie.longest_match(b"< "), Some((1, 1)));
        assert_eq!(trie.longest_match(b"..."), Some((6, 3)));
        assert_eq!(trie.longest_match(b"..x"), Some((5, 2)));
        assert_eq!(trie.longest_match(b"+"), None);
    }
}
