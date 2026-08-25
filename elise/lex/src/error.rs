//! Errors surfaced by the driver or a spec hook. Spanned by byte offset.

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    /// A byte no rule claimed.
    UnexpectedByte { byte: u8, pos: u32 },
    /// Input ended mid-token (unterminated string/comment/...).
    UnexpectedEof { pos: u32 },
    /// Spec-described failure (invalid number/string/escape, ...).
    Message { pos: u32, message: Box<str> },
}

impl LexError {
    /// Build a [`LexError::Message`] at `pos`.
    pub fn message(pos: usize, message: impl Into<Box<str>>) -> Self {
        LexError::Message {
            pos: pos as u32,
            message: message.into(),
        }
    }
}
