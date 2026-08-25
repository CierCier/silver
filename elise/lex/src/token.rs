//! Flat token storage for elise lexers.
//!
//! Design: fixed-layout rows in contiguous vectors — sequential scans stay
//! on the prefetcher, and there are no heap allocations per token. Token
//! *kinds* are opaque `u16`s owned by the [`LexSpec`](crate::LexSpec); the
//! buffer never interprets them.
//!
//! Trivia (whitespace, comments) is stored **out of band**: it never appears
//! in the token rows, but every byte of source is covered by either a token
//! or a trivia entry, so lossless reconstruction always holds.

/// One significant token: an opaque kind plus its byte range in the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct TokenRow {
    pub kind: u16,
    pub start: u32,
    pub len: u32,
}

/// One trivia run (whitespace or comment) covering `[start, start+len)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct TriviaRow {
    pub start: u32,
    pub len: u32,
    /// `TriviaKind` discriminant owned by the spec (opaque to the buffer).
    pub kind: u16,
}

/// Flat, allocation-light output of a lex pass.
///
/// Rows always end with a zero-length EOF row whose kind is the spec's EOF
/// marker, mirroring the trailing-sentinel convention parsers rely on.
#[derive(Debug, Clone, Default)]
pub struct TokenBuf {
    rows: Vec<TokenRow>,
    trivia: Vec<TriviaRow>,
}

impl TokenBuf {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_token(&mut self, kind: u16, start: u32, len: u32) {
        self.rows.push(TokenRow { kind, start, len });
    }

    pub fn push_trivia(&mut self, kind: u16, start: u32, len: u32) {
        self.trivia.push(TriviaRow { start, len, kind });
    }

    pub(crate) fn push_eof(&mut self, eof_kind: u16, pos: u32) {
        self.rows.push(TokenRow {
            kind: eof_kind,
            start: pos,
            len: 0,
        });
    }

    /// Number of rows including the trailing EOF sentinel.
    #[inline]
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    #[inline]
    pub fn row(&self, index: usize) -> Option<TokenRow> {
        self.rows.get(index).copied()
    }

    /// Raw rows, including the EOF sentinel. Parsers walk this slice.
    #[inline]
    pub fn rows(&self) -> &[TokenRow] {
        &self.rows
    }

    /// Out-of-band trivia runs, ordered by source offset.
    #[inline]
    pub fn trivia(&self) -> &[TriviaRow] {
        &self.trivia
    }

    /// Source text of row `index`.
    #[inline]
    pub fn text<'src>(&self, src: &'src str, index: usize) -> &'src str {
        let row = &self.rows[index];
        &src[row.start as usize..][..row.len as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rows_round_trip() {
        let mut buf = TokenBuf::new();
        buf.push_token(7, 0, 3);
        buf.push_trivia(1, 3, 2);
        buf.push_token(9, 5, 1);
        buf.push_eof(0, 6);
        // Two significant tokens plus the EOF sentinel; trivia is separate.
        assert_eq!(buf.len(), 3);
        assert_eq!(buf.row(0), Some(TokenRow { kind: 7, start: 0, len: 3 }));
        assert_eq!(buf.trivia().len(), 1);
    }
}
