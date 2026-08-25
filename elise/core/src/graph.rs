//! The user-facing view of a parsed file: lossless text, errors, and a
//! navigable cursor over the green tree.

use crate::green::Green;
use std::rc::Rc;

/// A syntax error recorded during parsing (spanned, non-fatal).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxError {
    pub start: u32,
    pub end: u32,
    pub message: String,
}

/// A fully materialized parse result: lossless by construction.
///
/// The graph owns the source text so token/node text can be reconstructed
/// lazily without the caller keeping the input alive.
#[derive(Debug, Clone)]
pub struct SourceGraph {
    src: Rc<str>,
    root: Rc<Green>,
    errors: Vec<SyntaxError>,
}

impl SourceGraph {
    pub fn new(src: impl Into<Rc<str>>, root: Rc<Green>, errors: Vec<SyntaxError>) -> Self {
        SourceGraph {
            src: src.into(),
            root,
            errors,
        }
    }

    /// The exact source this graph was parsed from.
    #[inline]
    pub fn text(&self) -> &str {
        &self.src
    }

    #[inline]
    pub fn errors(&self) -> &[SyntaxError] {
        &self.errors
    }

    #[inline]
    pub fn root(&self) -> NodeRef<'_> {
        NodeRef {
            graph: self,
            green: Some(&self.root),
            kind: self.root.kind,
            offset: 0,
            width: self.root.text_len as usize,
        }
    }
}

/// A borrowed position in the tree with absolute source offsets.
///
/// Leaf tokens are surfaced through this same type (`green == None`): they
/// have a kind, span, and text but no children. Traversal stays uniform
/// without allocating wrapper nodes.
#[derive(Debug, Clone, Copy)]
pub struct NodeRef<'g> {
    graph: &'g SourceGraph,
    green: Option<&'g Green>,
    kind: u16,
    offset: usize,
    width: usize,
}

impl<'g> NodeRef<'g> {
    #[inline]
    pub fn kind(&self) -> u16 {
        self.kind
    }

    /// True for leaf tokens; false for interior nodes.
    #[inline]
    pub fn is_leaf(&self) -> bool {
        self.green.is_none()
    }

    /// Absolute byte range `[start, end)` this node covers.
    #[inline]
    pub fn span(&self) -> (usize, usize) {
        (self.offset, self.offset + self.width)
    }

    /// The node's exact source text (trivia included — lossless).
    #[inline]
    pub fn text(&self) -> &'g str {
        &self.graph.src[self.offset..][..self.width]
    }

    #[inline]
    pub fn text_len(&self) -> u32 {
        self.width as u32
    }

    /// Direct children in source order: nested nodes and leaf tokens alike.
    pub fn children(&self) -> Children<'g> {
        let slots = match self.green {
            None => Vec::new(),
            Some(green) => green
                .children
                .iter()
                .map(|child| {
                    let width = child.width() as usize;
                    match child {
                        crate::green::GreenChild::Token { kind, .. } => Slot {
                            green: None,
                            kind: *kind,
                            width,
                        },
                        crate::green::GreenChild::Node(node) => Slot {
                            green: Some(node.as_ref()),
                            kind: node.kind,
                            width,
                        },
                    }
                })
                .collect::<Vec<_>>(),
        };
        Children {
            graph: self.graph,
            offset: self.offset,
            slots: slots.into_iter(),
        }
    }

    /// Depth-first walk of every leaf `(kind, text)` inside this subtree,
    /// trivia leaves included.
    pub fn walk_leaves(&self, visit: &mut impl FnMut(u16, &str)) {
        match self.green {
            None => visit(self.kind, self.text()),
            Some(green) => {
                let mut offset = self.offset;
                for child in &green.children {
                    let width = child.width() as usize;
                    match child {
                        crate::green::GreenChild::Token { kind, .. } => visit(
                            *kind,
                            &self.graph.src[offset as usize..][..width],
                        ),
                        crate::green::GreenChild::Node(node) => {
                            NodeRef {
                                graph: self.graph,
                                green: Some(node.as_ref()),
                                kind: node.kind,
                                offset,
                                width,
                            }
                            .walk_leaves(visit);
                        }
                    }
                    offset += width;
                }
            }
        }
    }
}

/// Iterator over a node's direct children.
pub struct Children<'g> {
    graph: &'g SourceGraph,
    offset: usize,
    slots: std::vec::IntoIter<Slot<'g>>,
}

struct Slot<'g> {
    green: Option<&'g Green>,
    kind: u16,
    width: usize,
}

impl<'g> Iterator for Children<'g> {
    type Item = NodeRef<'g>;

    fn next(&mut self) -> Option<Self::Item> {
        let slot = self.slots.next()?;
        let node = NodeRef {
            graph: self.graph,
            green: slot.green,
            kind: slot.kind,
            offset: self.offset,
            width: slot.width,
        };
        self.offset += slot.width;
        Some(node)
    }
}
