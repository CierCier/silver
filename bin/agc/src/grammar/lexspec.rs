//! Silver's lexical specification expressed through elise-lex.
//!
//! This module replicates the legacy hand-written lexer's behavior *exactly*
//! — including its quirks (byte-wise identifier continuation over UTF-8, the
//! `'lifetime` reading of an apostrophe followed by a word, complex-literal
//! `i` suffixes, and the absence of shift/compound-bitwise tokens, which the
//! expression parser joins from two `<`/`>` tokens instead). The
//! differential parity test (`tests/elise_lexer_parity.rs`) pins this
//! against the old implementation across the whole corpus.

use elise_lex::{CommonKinds, KeywordMap, LexError, LexSpec, OpTrie, TokenBuf};

/// Every distinct token classification the Silver grammar recognizes.
///
/// Discriminants are the `u16` kinds written into [`elise_lex::TokenRow`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Tok {
    // Literals
    IntLit,
    FloatLit,
    ComplexLit,
    StrLit,
    CharLit,
    Lifetime,

    // Keywords
    Struct,
    Enum,
    Impl,
    Trait,
    Mut,
    Const,
    Static,
    Volatile,
    If,
    Else,
    While,
    For,
    Match,
    Break,
    Continue,
    Return,
    Type,
    Let,
    SelfType,
    Defer,
    Import,
    Comptime,
    Cast,
    Move,
    Extern,
    Private,
    Asm,
    As,
    In,
    Macro,
    Launch,
    Wait,
    True,
    False,

    // Primitive type names
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    F80,
    C32,
    C64,
    C80,
    Bool,
    Str,
    Char,
    Void,
    Vec,
    Optional,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    AndAnd,
    OrOr,
    Not,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Increment,
    Decrement,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semi,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    Colon,
    ColonColon,
    Question,
    At,
    Hash,

    Ident,
    Eof,

    // Trivia — never appears in token rows; kept in the same enum so the
    // parity test can name every emitted classification.
    TriviaLayout = 0x100,
    TriviaLineComment,
    TriviaDocLineComment,
    TriviaBlockComment,
    TriviaDocBlockComment,
}

/// Operators, longest-first. Exactly the legacy set — note absences:
/// no `<<`/`>>` (the expression parser joins two `<`/`>` tokens), no
/// compound bitwise assignments, no arrow.
const OPERATORS: &[(&str, Tok)] = &[
    ("...", Tok::DotDotDot),
    ("..", Tok::DotDot),
    ("++", Tok::Increment),
    ("+=", Tok::PlusAssign),
    ("--", Tok::Decrement),
    ("-=", Tok::MinusAssign),
    ("*=", Tok::StarAssign),
    ("/=", Tok::SlashAssign),
    ("%=", Tok::PercentAssign),
    ("==", Tok::Equal),
    ("!=", Tok::NotEqual),
    ("<=", Tok::LessEqual),
    (">=", Tok::GreaterEqual),
    ("&&", Tok::AndAnd),
    ("||", Tok::OrOr),
    ("::", Tok::ColonColon),
    ("+", Tok::Plus),
    ("-", Tok::Minus),
    ("*", Tok::Star),
    ("/", Tok::Slash),
    ("%", Tok::Percent),
    ("=", Tok::Assign),
    ("!", Tok::Not),
    ("<", Tok::Less),
    (">", Tok::Greater),
    ("&", Tok::BitAnd),
    ("|", Tok::BitOr),
    ("^", Tok::BitXor),
    ("~", Tok::BitNot),
    (":", Tok::Colon),
    (".", Tok::Dot),
    ("(", Tok::LParen),
    (")", Tok::RParen),
    ("{", Tok::LBrace),
    ("}", Tok::RBrace),
    ("[", Tok::LBracket),
    ("]", Tok::RBracket),
    (";", Tok::Semi),
    (",", Tok::Comma),
    ("?", Tok::Question),
    ("@", Tok::At),
    ("#", Tok::Hash),
];

/// Keywords and primitive-type words, exactly as the legacy keyword match.
const KEYWORDS: &[(&str, Tok)] = &[
    ("struct", Tok::Struct),
    ("enum", Tok::Enum),
    ("impl", Tok::Impl),
    ("trait", Tok::Trait),
    ("mut", Tok::Mut),
    ("const", Tok::Const),
    ("static", Tok::Static),
    ("volatile", Tok::Volatile),
    ("if", Tok::If),
    ("else", Tok::Else),
    ("while", Tok::While),
    ("for", Tok::For),
    ("match", Tok::Match),
    ("break", Tok::Break),
    ("continue", Tok::Continue),
    ("return", Tok::Return),
    ("let", Tok::Let),
    ("defer", Tok::Defer),
    ("import", Tok::Import),
    ("comptime", Tok::Comptime),
    ("cast", Tok::Cast),
    ("move", Tok::Move),
    ("type", Tok::Type),
    ("Self", Tok::SelfType),
    ("extern", Tok::Extern),
    ("private", Tok::Private),
    ("asm", Tok::Asm),
    ("as", Tok::As),
    ("in", Tok::In),
    ("macro", Tok::Macro),
    ("true", Tok::True),
    ("false", Tok::False),
    ("launch", Tok::Launch),
    ("wait", Tok::Wait),
    ("i8", Tok::I8),
    ("i16", Tok::I16),
    ("i32", Tok::I32),
    ("i64", Tok::I64),
    ("i128", Tok::I128),
    ("u8", Tok::U8),
    ("u16", Tok::U16),
    ("u32", Tok::U32),
    ("u64", Tok::U64),
    ("u128", Tok::U128),
    ("f32", Tok::F32),
    ("f64", Tok::F64),
    ("f80", Tok::F80),
    ("c32", Tok::C32),
    ("c64", Tok::C64),
    ("c80", Tok::C80),
    ("bool", Tok::Bool),
    ("str", Tok::Str),
    ("char", Tok::Char),
    ("void", Tok::Void),
    ("Vec", Tok::Vec),
    ("Optional", Tok::Optional),
];

/// The compiled Silver lexical spec. Build once, reuse across files.
#[derive(Debug, Clone)]
pub struct SilverLexSpec {
    ops: OpTrie,
    keywords: KeywordMap,
    common: CommonKinds,
}

impl Default for SilverLexSpec {
    fn default() -> Self {
        Self::new()
    }
}

impl SilverLexSpec {
    pub fn new() -> Self {
        let ops = OpTrie::new(
            &OPERATORS.iter().map(|(s, t)| (*s, *t as u16)).collect::<Vec<_>>(),
        );
        let keywords = KeywordMap::new(
            &KEYWORDS.iter().map(|(s, t)| (*s, *t as u16)).collect::<Vec<_>>(),
        );
        Self {
            ops,
            keywords,
            common: CommonKinds {
                layout: Tok::TriviaLayout as u16,
                line_comment: Tok::TriviaLineComment as u16,
                doc_line_comment: Tok::TriviaDocLineComment as u16,
                block_comment: Tok::TriviaBlockComment as u16,
                doc_block_comment: Tok::TriviaDocBlockComment as u16,
                eof: Tok::Eof as u16,
            },
        }
    }

    /// Legacy quirk: identifier continuation accepts any byte whose Latin-1
    /// reinterpretation (`byte as char`) is alphanumeric — so UTF-8 trailing
    /// bytes flow into identifiers exactly like the old lexer.
    #[inline]
    fn legacy_ident_continue(b: u8) -> bool {
        b == b'_' || (b as char).is_alphanumeric()
    }
}

impl LexSpec for SilverLexSpec {
    fn common_kinds(&self) -> &CommonKinds {
        &self.common
    }

    fn op_trie(&self) -> &OpTrie {
        &self.ops
    }

    fn keywords(&self) -> &KeywordMap {
        &self.keywords
    }

    fn comment_config(&self) -> Option<elise_lex::CommentConfig> {
        Some(elise_lex::CommentConfig { nested_blocks: true })
    }

    #[inline]
    fn is_layout(&self, byte: u8) -> bool {
        matches!(byte, b' ' | b'\r' | b'\t' | b'\n')
    }

    #[inline]
    fn ident_start(&self, byte: u8) -> bool {
        byte.is_ascii_alphabetic() || byte == b'_'
    }

    #[inline]
    fn ident_continue(&self, byte: u8) -> bool {
        Self::legacy_ident_continue(byte)
    }

    fn ident_kind(&self) -> u16 {
        Tok::Ident as u16
    }

    fn scan_other(
        &mut self,
        bytes: &[u8],
        pos: usize,
        buf: &mut TokenBuf,
    ) -> Result<usize, LexError> {
        let first = bytes[pos];
        let (kind, end) = match first {
            b'"' => scan_string(bytes, pos)?,
            b'\'' => scan_char_or_lifetime(bytes, pos)?,
            b'0'..=b'9' => scan_number(bytes, pos)?,
            _ => return Err(LexError::UnexpectedByte { byte: first, pos: pos as u32 }),
        };
        buf.push_token(kind as u16, pos as u32, (end - pos) as u32);
        Ok(end)
    }
}

// ---------------------------------------------------------------------------
// Literal scanners — byte-for-byte ports of the legacy behavior.
// Each returns (kind, end_offset).
// ---------------------------------------------------------------------------

fn scan_number(bytes: &[u8], start: usize) -> Result<(Tok, usize), LexError> {
    let mut pos = start;

    // Hex literal: 0x<hexdigits>+ — no suffix handling after hex.
    if bytes[pos] == b'0' && pos + 1 < bytes.len() && (bytes[pos + 1] | 0x20) == b'x' {
        pos += 2;
        let digits_start = pos;
        while pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
            pos += 1;
        }
        if pos == digits_start {
            return Err(LexError::message(
                start,
                "Invalid hex literal: expected hex digits after 0x",
            ));
        }
        return Ok((Tok::IntLit, pos));
    }

    // Integer part.
    while pos < bytes.len() && bytes[pos].is_ascii_digit() {
        pos += 1;
    }

    // Fraction: '.' only when followed by another digit.
    if pos + 1 < bytes.len() && bytes[pos] == b'.' && bytes[pos + 1].is_ascii_digit() {
        pos += 1; // '.'
        while pos < bytes.len() && bytes[pos].is_ascii_digit() {
            pos += 1;
        }
        // Complex float: 3.5i
        if pos < bytes.len() && bytes[pos] == b'i' {
            return Ok((Tok::ComplexLit, pos + 1));
        }
        return Ok((Tok::FloatLit, pos));
    }

    // Complex int: 5i
    if pos < bytes.len() && bytes[pos] == b'i' {
        return Ok((Tok::ComplexLit, pos + 1));
    }

    Ok((Tok::IntLit, pos))
}

fn scan_char_or_lifetime(bytes: &[u8], start: usize) -> Result<(Tok, usize), LexError> {
    let mut pos = start + 1; // consume opening quote

    if pos >= bytes.len() {
        return Err(LexError::message(start, "Unterminated character literal"));
    }

    let first = bytes[pos];
    // Legacy lifetime quirk: `'word` (no closing quote right after a letter)
    // lexes as a LIFETIME token, consuming identifier-ish bytes.
    if first != b'\\'
        && ((first as char).is_alphabetic() || first == b'_')
        && (pos + 1 >= bytes.len() || bytes[pos + 1] != b'\'')
    {
        pos += 1;
        while pos < bytes.len()
            && ((bytes[pos] as char).is_alphanumeric() || bytes[pos] == b'_')
        {
            pos += 1;
        }
        return Ok((Tok::Lifetime, pos));
    }

    // Regular char literal: value or escape, then closing quote.
    if first == b'\\' {
        pos += 1;
        if pos >= bytes.len() {
            return Err(LexError::message(start, "Unterminated character literal"));
        }
        match bytes[pos] {
            b'n' | b't' | b'r' | b'\\' | b'"' | b'\'' | b'0' => {}
            other => {
                return Err(LexError::message(
                    pos,
                    format!("Invalid escape sequence: \\{}", other as char),
                ));
            }
        }
        pos += 1;
    } else {
        pos += 1;
    }

    if pos >= bytes.len() || bytes[pos] != b'\'' {
        return Err(LexError::message(start, "Unterminated character literal"));
    }
    pos += 1;
    Ok((Tok::CharLit, pos))
}

fn scan_string(bytes: &[u8], mut pos: usize) -> Result<(Tok, usize), LexError> {
    debug_assert_eq!(bytes[pos], b'"');
    pos += 1;

    while pos < bytes.len() && bytes[pos] != b'"' {
        let width = utf8_width(bytes[pos]);
        if pos + width > bytes.len() {
            break;
        }
        if bytes[pos] == b'\\' {
            pos += 1;
            if pos >= bytes.len() {
                return Err(LexError::message(bytes.len(), "Unterminated string literal"));
            }
            match bytes[pos] {
                b'n' | b't' | b'r' | b'\\' | b'"' | b'\'' | b'0' => pos += 1,
                b'x' => {
                    pos += 1;
                    if pos + 2 > bytes.len()
                        || !bytes[pos].is_ascii_hexdigit()
                        || !bytes[pos + 1].is_ascii_hexdigit()
                    {
                        let hi = bytes.get(pos).copied().unwrap_or(b'?') as char;
                        let lo = bytes.get(pos + 1).copied().unwrap_or(b'?') as char;
                        return Err(LexError::message(pos, format!("Invalid hex escape: \\x{hi}{lo}")));
                    }
                    pos += 2;
                }
                b'u' => {
                    pos += 1;
                    if pos >= bytes.len() || bytes[pos] != b'{' {
                        return Err(LexError::message(pos, "Expected `{` after \\u"));
                    }
                    pos += 1;
                    let mut digits = 0usize;
                    while pos < bytes.len() && bytes[pos] != b'}' && digits < 6 {
                        if !bytes[pos].is_ascii_hexdigit() {
                            return Err(LexError::message(pos, "Invalid hex digit in \\u escape"));
                        }
                        pos += 1;
                        digits += 1;
                    }
                    if digits == 0 {
                        return Err(LexError::message(pos, "Empty \\u{} escape"));
                    }
                    if pos >= bytes.len() || bytes[pos] != b'}' {
                        return Err(LexError::message(pos, "Unclosed \\u{ escape"));
                    }
                    pos += 1;
                }
                other => {
                    return Err(LexError::message(
                        pos,
                        format!("Invalid escape sequence: \\{}", other as char),
                    ));
                }
            }
        } else {
            pos += width;
        }
    }

    if pos >= bytes.len() {
        return Err(LexError::message(bytes.len(), "Unterminated string literal"));
    }
    pos += 1; // closing quote
    Ok((Tok::StrLit, pos))
}

fn utf8_width(first: u8) -> usize {
    match first {
        0x00..=0x7F => 1,
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        _ => 4,
    }
}

impl Tok {
    /// Map a raw row kind back to its variant. Significant kinds are dense
    /// starting at 0; trivia lives at 0x100+. Direct match — no auxiliary
    /// table, so there is a single source of truth for variant order.
    pub fn from_discriminant(kind: u16) -> Option<Tok> {
        Some(match kind {
            x if x == Tok::IntLit as u16 => Tok::IntLit,
            x if x == Tok::FloatLit as u16 => Tok::FloatLit,
            x if x == Tok::ComplexLit as u16 => Tok::ComplexLit,
            x if x == Tok::StrLit as u16 => Tok::StrLit,
            x if x == Tok::CharLit as u16 => Tok::CharLit,
            x if x == Tok::Lifetime as u16 => Tok::Lifetime,
            x if x == Tok::Struct as u16 => Tok::Struct,
            x if x == Tok::Enum as u16 => Tok::Enum,
            x if x == Tok::Impl as u16 => Tok::Impl,
            x if x == Tok::Trait as u16 => Tok::Trait,
            x if x == Tok::Mut as u16 => Tok::Mut,
            x if x == Tok::Const as u16 => Tok::Const,
            x if x == Tok::Static as u16 => Tok::Static,
            x if x == Tok::Volatile as u16 => Tok::Volatile,
            x if x == Tok::If as u16 => Tok::If,
            x if x == Tok::Else as u16 => Tok::Else,
            x if x == Tok::While as u16 => Tok::While,
            x if x == Tok::For as u16 => Tok::For,
            x if x == Tok::Match as u16 => Tok::Match,
            x if x == Tok::Break as u16 => Tok::Break,
            x if x == Tok::Continue as u16 => Tok::Continue,
            x if x == Tok::Return as u16 => Tok::Return,
            x if x == Tok::Type as u16 => Tok::Type,
            x if x == Tok::Let as u16 => Tok::Let,
            x if x == Tok::SelfType as u16 => Tok::SelfType,
            x if x == Tok::Defer as u16 => Tok::Defer,
            x if x == Tok::Import as u16 => Tok::Import,
            x if x == Tok::Comptime as u16 => Tok::Comptime,
            x if x == Tok::Cast as u16 => Tok::Cast,
            x if x == Tok::Move as u16 => Tok::Move,
            x if x == Tok::Extern as u16 => Tok::Extern,
            x if x == Tok::Private as u16 => Tok::Private,
            x if x == Tok::Asm as u16 => Tok::Asm,
            x if x == Tok::As as u16 => Tok::As,
            x if x == Tok::In as u16 => Tok::In,
            x if x == Tok::Macro as u16 => Tok::Macro,
            x if x == Tok::Launch as u16 => Tok::Launch,
            x if x == Tok::Wait as u16 => Tok::Wait,
            x if x == Tok::True as u16 => Tok::True,
            x if x == Tok::False as u16 => Tok::False,
            x if x == Tok::I8 as u16 => Tok::I8,
            x if x == Tok::I16 as u16 => Tok::I16,
            x if x == Tok::I32 as u16 => Tok::I32,
            x if x == Tok::I64 as u16 => Tok::I64,
            x if x == Tok::I128 as u16 => Tok::I128,
            x if x == Tok::U8 as u16 => Tok::U8,
            x if x == Tok::U16 as u16 => Tok::U16,
            x if x == Tok::U32 as u16 => Tok::U32,
            x if x == Tok::U64 as u16 => Tok::U64,
            x if x == Tok::U128 as u16 => Tok::U128,
            x if x == Tok::F32 as u16 => Tok::F32,
            x if x == Tok::F64 as u16 => Tok::F64,
            x if x == Tok::F80 as u16 => Tok::F80,
            x if x == Tok::C32 as u16 => Tok::C32,
            x if x == Tok::C64 as u16 => Tok::C64,
            x if x == Tok::C80 as u16 => Tok::C80,
            x if x == Tok::Bool as u16 => Tok::Bool,
            x if x == Tok::Str as u16 => Tok::Str,
            x if x == Tok::Char as u16 => Tok::Char,
            x if x == Tok::Void as u16 => Tok::Void,
            x if x == Tok::Vec as u16 => Tok::Vec,
            x if x == Tok::Optional as u16 => Tok::Optional,
            x if x == Tok::Plus as u16 => Tok::Plus,
            x if x == Tok::Minus as u16 => Tok::Minus,
            x if x == Tok::Star as u16 => Tok::Star,
            x if x == Tok::Slash as u16 => Tok::Slash,
            x if x == Tok::Percent as u16 => Tok::Percent,
            x if x == Tok::Equal as u16 => Tok::Equal,
            x if x == Tok::NotEqual as u16 => Tok::NotEqual,
            x if x == Tok::Less as u16 => Tok::Less,
            x if x == Tok::Greater as u16 => Tok::Greater,
            x if x == Tok::LessEqual as u16 => Tok::LessEqual,
            x if x == Tok::GreaterEqual as u16 => Tok::GreaterEqual,
            x if x == Tok::AndAnd as u16 => Tok::AndAnd,
            x if x == Tok::OrOr as u16 => Tok::OrOr,
            x if x == Tok::Not as u16 => Tok::Not,
            x if x == Tok::Assign as u16 => Tok::Assign,
            x if x == Tok::PlusAssign as u16 => Tok::PlusAssign,
            x if x == Tok::MinusAssign as u16 => Tok::MinusAssign,
            x if x == Tok::StarAssign as u16 => Tok::StarAssign,
            x if x == Tok::SlashAssign as u16 => Tok::SlashAssign,
            x if x == Tok::PercentAssign as u16 => Tok::PercentAssign,
            x if x == Tok::BitAnd as u16 => Tok::BitAnd,
            x if x == Tok::BitOr as u16 => Tok::BitOr,
            x if x == Tok::BitXor as u16 => Tok::BitXor,
            x if x == Tok::BitNot as u16 => Tok::BitNot,
            x if x == Tok::Increment as u16 => Tok::Increment,
            x if x == Tok::Decrement as u16 => Tok::Decrement,
            x if x == Tok::LParen as u16 => Tok::LParen,
            x if x == Tok::RParen as u16 => Tok::RParen,
            x if x == Tok::LBrace as u16 => Tok::LBrace,
            x if x == Tok::RBrace as u16 => Tok::RBrace,
            x if x == Tok::LBracket as u16 => Tok::LBracket,
            x if x == Tok::RBracket as u16 => Tok::RBracket,
            x if x == Tok::Semi as u16 => Tok::Semi,
            x if x == Tok::Comma as u16 => Tok::Comma,
            x if x == Tok::Dot as u16 => Tok::Dot,
            x if x == Tok::DotDot as u16 => Tok::DotDot,
            x if x == Tok::DotDotDot as u16 => Tok::DotDotDot,
            x if x == Tok::Colon as u16 => Tok::Colon,
            x if x == Tok::ColonColon as u16 => Tok::ColonColon,
            x if x == Tok::Question as u16 => Tok::Question,
            x if x == Tok::At as u16 => Tok::At,
            x if x == Tok::Hash as u16 => Tok::Hash,
            x if x == Tok::Ident as u16 => Tok::Ident,
            x if x == Tok::Eof as u16 => Tok::Eof,
            _ => return None,
        })
    }
}

#[cfg(test)]
mod discriminant_tests {
    use super::*;

    /// Guards the from_discriminant table against enum/table drift.
    #[test]
    fn discriminant_table_covers_every_variant() {
        let all = [
            Tok::IntLit, Tok::FloatLit, Tok::ComplexLit, Tok::StrLit,
            Tok::CharLit, Tok::Lifetime, Tok::Struct, Tok::Enum,
            Tok::Impl, Tok::Trait, Tok::Mut, Tok::Const, Tok::Static,
            Tok::Volatile, Tok::If, Tok::Else, Tok::While, Tok::For,
            Tok::Match, Tok::Break, Tok::Continue, Tok::Return,
            Tok::Type, Tok::Let, Tok::SelfType, Tok::Defer,
            Tok::Import, Tok::Comptime, Tok::Cast, Tok::Move,
            Tok::Extern, Tok::Private, Tok::Asm, Tok::As, Tok::In,
            Tok::Macro, Tok::Launch, Tok::Wait, Tok::True, Tok::False,
            Tok::I8, Tok::I16, Tok::I32, Tok::I64, Tok::I128, Tok::U8,
            Tok::U16, Tok::U32, Tok::U64, Tok::U128, Tok::F32,
            Tok::F64, Tok::F80, Tok::C32, Tok::C64, Tok::C80,
            Tok::Bool, Tok::Str, Tok::Char, Tok::Void, Tok::Vec,
            Tok::Optional, Tok::Plus, Tok::Minus, Tok::Star,
            Tok::Slash, Tok::Percent, Tok::Equal, Tok::NotEqual,
            Tok::Less, Tok::Greater, Tok::LessEqual, Tok::GreaterEqual,
            Tok::AndAnd, Tok::OrOr, Tok::Not, Tok::Assign,
            Tok::PlusAssign, Tok::MinusAssign, Tok::StarAssign,
            Tok::SlashAssign, Tok::PercentAssign, Tok::BitAnd,
            Tok::BitOr, Tok::BitXor, Tok::BitNot, Tok::Increment,
            Tok::Decrement, Tok::LParen, Tok::RParen, Tok::LBrace,
            Tok::RBrace, Tok::LBracket, Tok::RBracket, Tok::Semi,
            Tok::Comma, Tok::Dot, Tok::DotDot, Tok::DotDotDot,
            Tok::Colon, Tok::ColonColon, Tok::Question, Tok::At,
            Tok::Hash, Tok::Ident, Tok::Eof,
        ];
        for variant in all {
            assert_eq!(
                Tok::from_discriminant(variant as u16),
                Some(variant),
                "discriminant collision or gap at {variant:?}"
            );
        }
        assert!(Tok::from_discriminant(Tok::TriviaLayout as u16).is_none());
    }
}
