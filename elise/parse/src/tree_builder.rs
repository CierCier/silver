//! Fold parser events + lexed tokens into an immutable green tree wrapped
//! in a lossless [`SourceGraph`].

use elise_core::graph::SourceGraph;
use elise_core::green::{Green, GreenChild};
use elise_core::SyntaxError;
use elise_lex::TokenBuf;
use std::rc::Rc;

use crate::events::Event;

/// One leaf in unified source order: significant tokens and trivia
/// interleaved exactly as they appear in the text.
#[derive(Debug, Clone, Copy)]
struct Leaf {
    kind: u16,
    width: u32,
    is_trivia: bool,
}

/// Builds a [`SourceGraph`] from parse events.
///
/// Trivia handling: every [`Event::Advance`] first attaches all pending
/// trivia that precedes the consumed token, so the output tree covers the
/// source byte-for-byte without parsers ever reasoning about whitespace.
pub struct TreeBuilder<'a> {
    src: &'a str,
    tokens: &'a TokenBuf,
    errors: Vec<SyntaxError>,
    root_kind: u16,
}

impl<'a> TreeBuilder<'a> {
    pub fn new(src: &'a str, tokens: &'a TokenBuf) -> Self {
        // Default root kind mirrors the lexer's EOF sentinel kind so the
        // graph is well-formed even for a bare `Advance` stream.
        let root_kind = tokens.rows().last().map(|row| row.kind).unwrap_or(0);
        TreeBuilder {
            src,
            tokens,
            errors: Vec::new(),
            root_kind,
        }
    }

    /// Override the root node kind.
    pub fn root_kind(mut self, kind: u16) -> Self {
        self.root_kind = kind;
        self
    }

    /// Attach syntax errors collected during parsing.
    pub fn errors(mut self, errors: Vec<SyntaxError>) -> Self {
        self.errors = errors;
        self
    }

    /// Fold `events` into the tree and produce the graph.
    pub fn finish(self, events: &[Event]) -> SourceGraph {
        let leaves = self.merged_leaves();
        let mut fold = Fold {
            leaves: &leaves,
            leaf_pos: 0,
            stack: vec![(self.root_kind, Vec::new())],
        };

        for event in events {
            match *event {
                Event::Enter(kind) => fold.stack.push((kind, Vec::new())),
                Event::Nop => {}
                Event::Advance(count) => {
                    for _ in 0..count {
                        // Trivia preceding this token joins the open node.
                        while matches!(
                            fold.leaves.get(fold.leaf_pos),
                            Some(leaf) if leaf.is_trivia
                        ) {
                            let leaf = fold.leaves[fold.leaf_pos];
                            fold.leaf_pos += 1;
                            fold.push_leaf(leaf.kind, leaf.width);
                        }
                        let Some(leaf) = fold
                            .leaves
                            .get(fold.leaf_pos)
                            .filter(|leaf| !leaf.is_trivia)
                        else {
                            if std::env::var_os("ELISE_DBG_ADVANCE").is_some() {
                                eprintln!(
                                    "DBG advance overflow at leaf_pos={}/{} byte={}",
                                    fold.leaf_pos,
                                    fold.leaves.len(),
                                    fold.leaves.iter().map(|l| l.width as usize).sum::<usize>()
                                );
                            }
                            break; // out of tokens: malformed events tolerated
                        };
                        let leaf = *leaf;
                        fold.leaf_pos += 1;
                        fold.push_leaf(leaf.kind, leaf.width);
                    }
                }
                Event::Exit => {
                    if fold.stack.len() > 1 {
                        fold.attach_child();
                    }
                }
            }
        }

        // Tolerate malformed streams: close whatever stayed open.
        while fold.stack.len() > 1 {
            fold.attach_child();
        }

        // Trailing trivia after the last consumed token lands in the root.
        let (root_kind, mut children) = fold.stack.pop().unwrap();
        while fold.leaf_pos < fold.leaves.len() {
            let leaf = fold.leaves[fold.leaf_pos];
            fold.leaf_pos += 1;
            children.push(GreenChild::Token {
                kind: leaf.kind,
                width: leaf.width,
            });
        }

        let root = Rc::new(Green::new(root_kind, children));
        SourceGraph::new(self.src, root, self.errors)
    }

    /// Significant tokens and trivia merged in source order.
    fn merged_leaves(&self) -> Vec<Leaf> {
        let mut leaves = Vec::with_capacity(self.tokens.rows().len() + self.tokens.trivia().len());

        let mut trivia = self.tokens.trivia().iter().filter(|t| t.len > 0);
        let mut significant = self.tokens.rows().iter().filter(|r| r.len > 0);

        let mut trivia_next = trivia.next();
        let mut sig_next = significant.next();

        loop {
            match (trivia_next.map(|t| t.start), sig_next.map(|r| r.start)) {
                (Some(t), Some(s)) => {
                    if t <= s {
                        let row = trivia_next.unwrap();
                        leaves.push(Leaf {
                            kind: row.kind,
                            width: row.len,
                            is_trivia: true,
                        });
                        trivia_next = trivia.next();
                    } else {
                        let row = sig_next.unwrap();
                        leaves.push(Leaf {
                            kind: row.kind,
                            width: row.len,
                            is_trivia: false,
                        });
                        sig_next = significant.next();
                    }
                }
                (Some(_), None) | (None, Some(_)) => {
                    if let Some(row) = trivia_next.take() {
                        leaves.push(Leaf {
                            kind: row.kind,
                            width: row.len,
                            is_trivia: true,
                        });
                        trivia_next = trivia.next();
                    } else if let Some(row) = sig_next.take() {
                        leaves.push(Leaf {
                            kind: row.kind,
                            width: row.len,
                            is_trivia: false,
                        });
                        sig_next = significant.next();
                    } else {
                        break;
                    }
                }
                (None, None) => break,
            }
        }
        leaves
    }
}

struct Fold<'a> {
    leaves: &'a [Leaf],
    leaf_pos: usize,
    /// Open-node stack: `(kind, accumulated children)`.
    stack: Vec<(u16, Vec<GreenChild>)>,
}

impl Fold<'_> {
    #[inline]
    fn attach_child(&mut self) {
        let (kind, children) = self.stack.pop().expect("tree fold: no open node");
        let green = Green::new(kind, children);
        self.stack
            .last_mut()
            .expect("tree fold: no parent for child")
            .1
            .push(GreenChild::Node(Rc::new(green)));
    }

    #[inline]
    fn push_leaf(&mut self, kind: u16, width: u32) {
        self.stack
            .last_mut()
            .expect("tree fold: no open node")
            .1
            .push(GreenChild::Token { kind, width });
    }
}
