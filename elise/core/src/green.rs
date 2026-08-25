//! Position-independent immutable syntax trees ("green" layer).
//!
//! A [`Green`] node knows its kind and total byte width but nothing about
//! where it sits in the file. Children are either leaf tokens (kind +
//! width; text lives in the source) or shared child nodes (`Rc<Green>`).
//!
//! This representation is what makes cheap clones, structural hashing, and
//! (later) incremental reparsing possible: identical subtrees share memory,
//! and an edit only invalidates the ancestors whose extent changed.

use std::rc::Rc;

/// A node in the immutable green tree.
#[derive(Debug)]
pub struct Green {
    /// Opaque kind owned by the grammar spec.
    pub kind: u16,
    /// Total byte length of everything this node covers (tokens + trivia).
    pub text_len: u32,
    pub children: Vec<GreenChild>,
}

#[derive(Debug, Clone)]
pub enum GreenChild {
    /// A leaf token covering `width` bytes of source.
    Token { kind: u16, width: u32 },
    /// A nested node.
    Node(Rc<Green>),
}

impl Green {
    pub fn new(kind: u16, children: Vec<GreenChild>) -> Self {
        let text_len = children.iter().map(GreenChild::width).sum();
        Green {
            kind,
            text_len,
            children,
        }
    }

    /// Wrap in an `Rc` for sharing between trees.
    pub fn shared(self) -> Rc<Self> {
        Rc::new(self)
    }

    #[inline]
    pub fn text_len(&self) -> u32 {
        self.text_len
    }
}

impl GreenChild {
    #[inline]
    pub fn width(&self) -> u32 {
        match self {
            GreenChild::Token { width, .. } => *width,
            GreenChild::Node(node) => node.text_len,
        }
    }

    #[inline]
    pub fn kind(&self) -> u16 {
        match self {
            GreenChild::Token { kind, .. } => *kind,
            GreenChild::Node(node) => node.kind,
        }
    }

    pub fn as_node(&self) -> Option<&Rc<Green>> {
        match self {
            GreenChild::Node(node) => Some(node),
            _ => None,
        }
    }

    pub fn as_token(&self) -> Option<(u16, u32)> {
        match self {
            GreenChild::Token { kind, width } => Some((*kind, *width)),
            _ => None,
        }
    }
}
