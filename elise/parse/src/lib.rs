//! elise-parse: the syntactic stage of the elise pipeline.
//!
//! Parsers emit [`Event`] streams; the [`TreeBuilder`] folds them together
//! with an [`elise_lex::TokenBuf`] into a lossless
//! [`elise_core::SourceGraph`].

pub mod events;
pub mod tree_builder;

pub use events::Event;
pub use tree_builder::TreeBuilder;
