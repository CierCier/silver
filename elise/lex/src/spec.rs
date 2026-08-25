//! The [`LexSpec`] trait: everything elise-lex needs to know about a
//! language's surface syntax, expressed as data + one small scan hook.

use crate::keywords::KeywordMap;
use crate::token::TokenBuf;
use crate::trie::OpTrie;
use crate::LexError;

/// Kinds the driver itself emits for trivia and EOF. Opaque `u16`s so the
/// host language owns its own numbering.
#[derive(Debug, Clone, Copy)]
pub struct CommonKinds {
    pub layout: u16,
    pub line_comment: u16,
    pub doc_line_comment: u16,
    pub block_comment: u16,
    pub doc_block_comment: u16,
    pub eof: u16,
}

/// Comment syntax description for the generic comment scanner.
///
/// Silver-style: `//` / `///` lines, nestable `/* */` blocks with `/**` doc
/// marker — all configurable. `None` from
/// [`LexSpec::comment_config`] disables comments entirely.
#[derive(Debug, Clone, Copy)]
pub struct CommentConfig {
    /// `/* /* nested */ */` support.
    pub nested_blocks: bool,
}

/// Language-specific scanning contract.
///
/// The driver handles: layout skipping, comments (via
/// [`LexSpec::comment_config`]), identifier words (`ident_start` /
/// `ident_continue` / [`LexSpec::keywords`]), and operator longest-match
/// ([`LexSpec::op_trie`]). Everything else — numbers, strings, char
/// literals, lifetimes, language oddities — funnels into
/// [`LexSpec::scan_other`] with the first byte unconsumed.
pub trait LexSpec {
    fn common_kinds(&self) -> &CommonKinds;
    fn op_trie(&self) -> &OpTrie;
    fn keywords(&self) -> &KeywordMap;
    fn comment_config(&self) -> Option<CommentConfig>;

    /// Hot classification hooks — implement as table lookups.
    fn is_layout(&self, byte: u8) -> bool;
    fn ident_start(&self, byte: u8) -> bool;
    fn ident_continue(&self, byte: u8) -> bool;

    /// Kind pushed for identifiers that resolved to no keyword.
    fn ident_kind(&self) -> u16;

    /// Scan a token whose first byte is at `pos` and which is none of:
    /// layout, comment start, identifier, or an operator-trie match.
    ///
    /// Push exactly one row onto `buf` covering `[pos, new_pos)` and return
    /// `new_pos`, or fail with a spanned [`LexError`].
    fn scan_other(
        &mut self,
        bytes: &[u8],
        pos: usize,
        buf: &mut TokenBuf,
    ) -> Result<usize, LexError>;
}
