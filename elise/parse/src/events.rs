//! Parser events — the flat protocol between a grammar's parse routine and
//! the tree builder.
//!
//! The parser never builds nodes. It emits `Enter(kind)`, consumes tokens
//! with `Advance(n)` (n = number of *significant* tokens), and closes with
//! `Exit`. Trivia is interleaved into leaves by the [`TreeBuilder`], so
//! parsers stay trivially correct about trivia and the output stays lossless.

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Event {
    /// Open a child node of the currently open node.
    ///
    /// Convention: the builder pre-seeds the tree with a root node built
    /// from `TreeBuilder::root_kind` — events describe the ROOT'S CHILDREN,
    /// never a duplicate outermost Enter/Exit pair.
    Enter(u16),
    /// Consume the next `count` significant tokens as leaves of the
    /// currently open node.
    Advance(u32),
    /// Close the most recently opened (and not yet closed) child node.
    Exit,
}

/// Convenience: a complete single-token event sequence.
pub const fn one_token(kind: u16) -> [Event; 3] {
    [Event::Enter(kind), Event::Advance(1), Event::Exit]
}
