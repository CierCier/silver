//! elise-lex: the lexical stage of the elise pipeline.
//!
//! A [`LexSpec`](spec::LexSpec) describes a language's surface (operators,
//! keywords, comment style, plus one scan hook for literals and oddities);
//! [`scan`] turns source into a flat, allocation-light [`TokenBuf`].
//!
//! Trivia (whitespace/comments) is stored out of band so parsers see only
//! significant tokens while lossless reconstruction always holds.

pub mod driver;
pub mod error;
pub mod keywords;
pub mod preprocess;
pub mod spec;
pub mod token;
pub mod trie;

pub use driver::scan;
pub use error::LexError;
pub use keywords::KeywordMap;
pub use preprocess::{Chain, NoPreprocess, Preprocess};
pub use spec::{CommonKinds, CommentConfig, LexSpec};
pub use token::{TokenBuf, TokenRow, TriviaRow};
pub use trie::OpTrie;
