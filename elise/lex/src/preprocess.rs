//! Preprocessing hooks (roadmap decision 9).
//!
//! Elise owns the lexical stage and lets host languages plug transforms in
//! at two well-defined points:
//!
//! 1. **Source-level** — before lexing (conditional compilation, include
//!    expansion). Implement [`Preprocess::transform_source`].
//! 2. **Token/trivia-level** — after lexing, before parsing. Filters and
//!    rewriters walk the [`TokenBuf`](crate::TokenBuf); nothing here touches
//!    semantics.
//!
//! Hooks are optional: with none registered the pipeline is a monomorphic
//! pass with zero dispatch overhead.

use std::borrow::Cow;

/// A pre-lex source transform.
pub trait Preprocess {
    /// Rewrite or annotate the raw source. Borrow by default; return
    /// `Cow::Owned` only when a transform actually changed something.
    fn transform_source<'a>(&self, src: &'a str) -> Cow<'a, str> {
        Cow::Borrowed(src)
    }
}

/// The default no-op hook. Monomorphic, inlined, free.
#[derive(Debug, Clone, Copy, Default)]
pub struct NoPreprocess;

impl Preprocess for NoPreprocess {}

/// Chain two hooks: outer runs first on source, inner second.
#[derive(Debug, Clone, Copy, Default)]
pub struct Chain<A, B> {
    pub outer: A,
    pub inner: B,
}

impl<A: Preprocess, B: Preprocess> Preprocess for Chain<A, B> {
    fn transform_source<'a>(&self, src: &'a str) -> Cow<'a, str> {
        match self.outer.transform_source(src) {
            Cow::Borrowed(borrowed) => self.inner.transform_source(borrowed),
            Cow::Owned(owned) => Cow::Owned(self.inner.transform_source(&owned).into_owned()),
        }
    }
}
