//! elise-core: spans, source graphs, and the immutable green tree layer.

pub mod graph;
pub mod green;

pub use graph::{Children, NodeRef, SourceGraph, SyntaxError};
pub use green::{Green, GreenChild};
