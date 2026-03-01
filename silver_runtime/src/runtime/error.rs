use std::fmt;

use super::heap::HeapError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RuntimeError {
    MethodNotFound { type_name: String, method: String },
    CastNotFound { from: String, to: String },
    InvalidReceiver { expected: String },
    ArityMismatch { expected: usize, found: usize },
    TypeMismatch { expected: String, found: String },
    Heap(HeapError),
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RuntimeError::MethodNotFound { type_name, method } => {
                write!(f, "method not found: {type_name}::{method}")
            }
            RuntimeError::CastNotFound { from, to } => {
                write!(f, "cast not found: {from} -> {to}")
            }
            RuntimeError::InvalidReceiver { expected } => {
                write!(f, "invalid receiver (expected {expected})")
            }
            RuntimeError::ArityMismatch { expected, found } => {
                write!(
                    f,
                    "wrong argument count: expected {expected}, found {found}"
                )
            }
            RuntimeError::TypeMismatch { expected, found } => {
                write!(f, "type mismatch: expected {expected}, found {found}")
            }
            RuntimeError::Heap(e) => write!(f, "heap error: {e:?}"),
        }
    }
}

impl std::error::Error for RuntimeError {}

impl From<HeapError> for RuntimeError {
    fn from(value: HeapError) -> Self {
        RuntimeError::Heap(value)
    }
}
