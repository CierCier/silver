//! The Silver grammar definition, expressed through elise.
//!
//! This module is the boundary between the generic elise engine and the
//! Silver language: it owns the lexical spec (see [`lexspec`]) and will own
//! the syntactic grammar + source-graph projection as the migration
//! progresses. Nothing Silver-specific may leak into `elise/`.

pub mod lexspec;

pub use lexspec::{SilverLexSpec, Tok};
