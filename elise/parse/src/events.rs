//! Parser events — the flat protocol between a grammar's parse routine and
//! the tree builder.
//!
//! The parser never builds nodes. It emits `Enter(kind)`, consumes tokens
//! with `Advance(n)` (n = number of *significant* tokens), and closes with
//! `Exit`. Trivia is interleaved into leaves by the [`TreeBuilder`], so
//! parsers stay trivially correct about trivia and the output stays lossless.
//!
//! Convention: the builder pre-seeds the tree with a root node from
//! `TreeBuilder::root_kind` — events describe the ROOT'S CHILDREN.
//!
//! [`Event::Nop`] is a placeholder used by marker rotation for
//! left-recursive binary expressions: the parser pushes a Nop before the
//! left operand, parses the rest, then rewrites the Nop in place as the
//! binary node's `Enter`, keeping prefix order without buffering.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Event {
    /// Open a child node of the currently open node.
    Enter(u16),
    /// Consume the next `count` significant tokens as leaves of the
    /// currently open node.
    Advance(u32),
    /// Close the most recently opened (and not yet closed) child node.
    Exit,
    /// Placeholder rewritten into `Enter` by marker rotation.
    Nop,
}
