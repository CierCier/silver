//! The Silver grammar definition, expressed through elise.//!
//! This module is the boundary between the generic elise engine and the
//! Silver language: it owns the lexical spec (see [`lexspec`]) and the
//! item-level parser (see [`parser`]). Nothing Silver-specific may leak
//! into `elise/`.

pub mod ast_view;
pub mod body;
pub mod lexspec;
pub mod lower;
pub mod parser;

pub use ast_view::*;
pub use lexspec::{SilverLexSpec, Tok};
pub use lower::lower_source_graph;
pub use parser::{parse_ag, NodeKind};
