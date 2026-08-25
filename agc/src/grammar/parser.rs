//! Item-level parsing over elise token streams.
//!
//! Phase-scope note (M3): this pass recognizes top-level item boundaries and
//! kinds, emitting one child node per item under the root. Attributes and
//! visibility qualifiers merge into the following item (matching the legacy
//! parser). Bodies are consumed as flat token leaves; statement/expression
//! structure arrives in M4. Consumption is delimiter-driven (balanced braces
//! / semicolons), so a malformed body cannot cascade into the next item.

use elise_lex::{LexError, TokenBuf};
use elise_parse::{Event, TreeBuilder};
use elise_core::{SourceGraph, SyntaxError};

use super::lexspec::{SilverLexSpec, Tok};

/// Node kinds for the Silver source graph.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum NodeKind {
    File = 0,
    Import = 1,
    ExternDecl = 2,
    ExternBlock = 3,
    Struct = 4,
    Enum = 5,
    Trait = 6,
    Impl = 7,
    Macro = 8,
    TypeAlias = 9,
    Function = 10,
    GlobalVariable = 11,
    /// Attribute group attached to a following item; kept distinct so the
    /// item-count parity check can ignore it while the tree stays lossless.
    Attribute = 12,
}

impl NodeKind {
    pub fn from_u16(v: u16) -> Option<Self> {
        Some(match v {
            v if v == NodeKind::File as u16 => NodeKind::File,
            v if v == NodeKind::Import as u16 => NodeKind::Import,
            v if v == NodeKind::ExternDecl as u16 => NodeKind::ExternDecl,
            v if v == NodeKind::ExternBlock as u16 => NodeKind::ExternBlock,
            v if v == NodeKind::Struct as u16 => NodeKind::Struct,
            v if v == NodeKind::Enum as u16 => NodeKind::Enum,
            v if v == NodeKind::Trait as u16 => NodeKind::Trait,
            v if v == NodeKind::Impl as u16 => NodeKind::Impl,
            v if v == NodeKind::Macro as u16 => NodeKind::Macro,
            v if v == NodeKind::TypeAlias as u16 => NodeKind::TypeAlias,
            v if v == NodeKind::Function as u16 => NodeKind::Function,
            v if v == NodeKind::GlobalVariable as u16 => NodeKind::GlobalVariable,
            v if v == NodeKind::Attribute as u16 => NodeKind::Attribute,
            _ => return None,
        })
    }
}

/// Parse a Silver source file into a [`SourceGraph`].
pub fn parse_ag(src: &str) -> SourceGraph {
    let mut spec = SilverLexSpec::new();
    let tokens = match elise_lex::scan(&mut spec, src) {
        Ok(buf) => buf,
        Err(err) => {
            let pos = match &err {
                LexError::UnexpectedByte { pos, .. } | LexError::UnexpectedEof { pos } => {
                    *pos as usize
                }
                LexError::Message { pos, .. } => *pos as usize,
            };
            let pos = pos.min(src.len());
            let errors = vec![SyntaxError {
                start: pos as u32,
                end: src.len() as u32,
                message: format!("{err:?}"),
            }];
            // Degenerate but lossless: whole file is one error leaf.
            let mut children = Vec::new();
            if !src.is_empty() {
                children.push(elise_core::GreenChild::Token {
                    kind: Tok::Eof as u16,
                    width: src.len() as u32,
                });
            }
            return SourceGraph::new(
                src,
                std::rc::Rc::new(elise_core::Green::new(
                    NodeKind::File as u16,
                    children,
                )),
                errors,
            );
        }
    };

    let mut parser = ItemParser {
        rows: tokens.rows(),
        pos: 0usize,
        events: Vec::new(),
        errors: Vec::new(),
    };
    parser.parse_file();
    TreeBuilder::new(src, &tokens)
        .root_kind(NodeKind::File as u16)
        .errors(parser.errors)
        .finish(&parser.events)
}

struct ItemParser<'a> {
    rows: &'a [elise_lex::TokenRow],
    pos: usize,
    events: Vec<Event>,
    errors: Vec<SyntaxError>,
}

impl<'a> ItemParser<'a> {
    /// Next significant (non-trivia) token at/after `index`.
    /// Returns `None` at end of input.
    #[inline]
    fn peek_sig(&self, index: usize) -> Option<(usize, Tok)> {
        let mut i = index;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return None; // EOF sentinel
            }
            if !is_trivia(row.kind) {
                if let Some(tok) = Tok::from_discriminant(row.kind) {
                    return Some((i, tok));
                }
            }
            i += 1;
        }
        None
    }

    fn parse_file(&mut self) {
        while let Some((idx, first_tok)) = self.peek_sig(self.pos) {
            // Attributes and visibility merge into the following item.
            let mut start = idx;
            let mut cursor = idx;

            loop {
                match self.peek_sig(cursor) {
                    Some((hash_idx, Tok::Hash)) => {
                        let after_group =
                            self.skip_balanced(hash_idx + 1, Tok::LBracket, Tok::RBracket);
                        self.emit_item(NodeKind::Attribute, hash_idx, after_group);
                        cursor = after_group;
                    }
                    Some((priv_idx, Tok::Private)) => {
                        cursor = priv_idx + 1;
                    }
                    Some(_) => break,
                    None => break,
                }
            }

            let Some((item_idx, tok)) = self.peek_sig(cursor) else {
                break;
            };
            let _ = start;
            let start = idx; // item node spans from its first attribute

            let (kind, end) = match tok {
                Tok::Import => (NodeKind::Import, self.thru_terminator(item_idx + 1)),
                Tok::Type => (NodeKind::TypeAlias, self.thru_terminator(item_idx + 1)),
                Tok::Extern => {
                    let kind = match self.peek_sig(item_idx + 1) {
                        Some((_, Tok::StrLit)) => match self.peek_sig(item_idx + 2) {
                            Some((_, Tok::LBrace)) => NodeKind::ExternBlock,
                            _ => NodeKind::ExternDecl,
                        },
                        Some((abi_end, _)) if matches!(self.peek_sig(abi_end), Some((_, Tok::LBrace))) => {
                            NodeKind::ExternBlock
                        }
                        _ => NodeKind::ExternDecl,
                    };
                    let end = match kind {
                        NodeKind::ExternBlock => self.thru_braces(item_idx + 1),
                        _ => self.thru_terminator(item_idx + 1),
                    };
                    (kind, end)
                }
                Tok::Struct => (NodeKind::Struct, self.thru_braces(item_idx + 1)),
                Tok::Enum => (NodeKind::Enum, self.thru_braces(item_idx + 1)),
                Tok::Trait => (NodeKind::Trait, self.thru_braces(item_idx + 1)),
                Tok::Impl => (NodeKind::Impl, self.thru_braces(item_idx + 1)),
                Tok::Macro => (NodeKind::Macro, self.thru_braces(item_idx + 1)),
                Tok::Const | Tok::Static | Tok::Volatile => {
                    let end = self.classify_tail(item_idx + 1);
                    (NodeKind::GlobalVariable, self.absorb_trailing_semi(end))
                }
                _ => {
                    let end = self.classify_tail(item_idx + 1);
                    if self.tail_is_function(item_idx + 1, end) {
                        (NodeKind::Function, end)
                    } else {
                        (NodeKind::GlobalVariable, self.absorb_trailing_semi(end))
                    }
                }
            };

            self.emit_item(kind, start, end);
            self.pos = end;
        }

        // Any trailing garbage that peek_sig could not classify is skipped by
        // advancing one row at a time until progress stops.
        while self.pos < self.rows.len() {
            let before = self.pos;
            match self.peek_sig(self.pos) {
                Some((idx, tok)) => {
                    let end = self.thru_terminator(idx + 1);
                    self.emit_item(NodeKind::GlobalVariable, idx, end);
                    self.pos = end;
                    if end <= before {
                        break;
                    }
                }
                None => break,
            }
        }
    }

    fn emit_item(&mut self, kind: NodeKind, start: usize, end: usize) {
        self.events.push(Event::Enter(kind as u16));
        self.consume_range(start, end);
        self.events.push(Event::Exit);
    }

    /// Emit `Advance(1)` per significant row in `[start, end)`.
    fn consume_range(&mut self, start: usize, end: usize) {
        for i in start..end.min(self.rows.len()) {
            let row = &self.rows[i];
            if !is_trivia(row.kind) && row.len > 0 {
                self.events.push(Event::Advance(1));
            }
        }
    }

    /// From `from` (which may be ON the opening delimiter), skip a balanced
    /// group. Returns index just past the closer (or input end).
    fn skip_balanced(&self, from: usize, open: Tok, close: Tok) -> usize {
        let mut i = from;
        let mut depth = 0i64;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return i;
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            let tok = num_to_tok(row.kind);
            if tok == Some(open) {
                depth += 1;
            } else if tok == Some(close) {
                depth -= 1;
                if depth <= 0 {
                    return i + 1;
                }
            }
            i += 1;
        }
        i
    }

    /// From `start`, consume through the next top-level semicolon.
    fn thru_terminator(&self, start: usize) -> usize {
        let mut i = start;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return i;
            }
            if !is_trivia(row.kind) && num_to_tok(row.kind) == Some(Tok::Semi) {
                return i + 1;
            }
            i += 1;
        }
        i
    }

    /// From `start`, consume through the matching `}` of the first `{`.
    fn thru_braces(&self, start: usize) -> usize {
        let mut i = start;
        let mut depth = 0i64;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return i;
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            match num_to_tok(row.kind) {
                Some(Tok::LBrace) => depth += 1,
                Some(Tok::RBrace) => {
                    depth -= 1;
                    if depth <= 0 {
                        return i + 1;
                    }
                }
                _ => {}
            }
            i += 1;
        }
        i
    }

    /// Distinguish function (header + block) from global (semicolon).
    fn classify_tail(&self, start: usize) -> usize {
        let mut i = start;
        let mut depth = 0i64;
        let mut seen_brace = false;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return i;
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            match num_to_tok(row.kind) {
                Some(Tok::LBrace) => {
                    depth += 1;
                    seen_brace = true;
                }
                Some(Tok::RBrace) => {
                    depth -= 1;
                    if depth <= 0 && seen_brace {
                        return i + 1;
                    }
                }
                Some(Tok::Semi) if depth <= 0 && !seen_brace => return i + 1,
                _ => {}
            }
            i += 1;
        }
        i
    }

    /// Decide Function vs GlobalVariable for a tail that ends in either a
    /// body block or a semicolon.
    ///
    /// Rule: an identifier immediately before a top-level `(` means the item
    /// is a function header (`RetType name(params)`). A top-level `=` or
    /// `;` seen before any such paren — including brace initializers like
    /// `BufWriter STDOUT = { ... };` — means it is a global.
    fn tail_is_function(&self, start: usize, end: usize) -> bool {
        let mut prev_ident = false;
        let mut i = start;
        let mut depth = 0i64;
        while i < end.min(self.rows.len()) {
            let row = &self.rows[i];
            if row.len == 0 || is_trivia(row.kind) {
                i += 1;
                continue;
            }
            match num_to_tok(row.kind) {
                Some(Tok::LParen) if depth == 0 && prev_ident => return true,
                Some(Tok::LParen) | Some(Tok::LBracket) | Some(Tok::LBrace) => {
                    depth += 1;
                }
                Some(Tok::RParen)
                | Some(Tok::RBracket)
                | Some(Tok::RBrace) => {
                    depth = depth.saturating_sub(1);
                }
                Some(Tok::Ident) | Some(Tok::SelfType) => prev_ident = true,
                // Qualifiers/type keywords keep the previous-ident state so
                // `Vec<T> name(...)` still resolves as a function header.
                _ => {}
            }
            i += 1;
        }
        false
    }

    /// After a brace-initialized global (`= { ... };`), absorb the trailing
    /// semicolon when it directly follows the closing brace (only trivia
    /// in between).
    fn absorb_trailing_semi(&self, end: usize) -> usize {
        match self.peek_sig(end) {
            Some((semi_idx, Tok::Semi))
                if ((end..semi_idx).all(|i| is_trivia(self.rows[i].kind))) =>
            {
                semi_idx + 1
            }
            _ => end,
        }
    }

    fn next_is_lbrace(&self, index: usize) -> bool {
        matches!(self.peek_sig(index), Some((_, Tok::LBrace)))
    }
}

#[inline]
fn is_trivia(kind: u16) -> bool {
    kind >= Tok::TriviaLayout as u16
}

#[inline]
fn num_to_tok(kind: u16) -> Option<Tok> {
    Tok::from_discriminant(kind)
}
