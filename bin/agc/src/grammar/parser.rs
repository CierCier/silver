//! Item-level parsing over elise token streams.
//!
//! Phase-scope note (M3/M4): recognizes top-level item boundaries and kinds;
//! function bodies get structured statement/expression children via
//! [`body::BodyParser`]. Attributes and visibility qualifiers merge into the
//! following item, matching the legacy parser. Consumption is
//! delimiter-driven so a malformed body cannot cascade into the next item.

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
                elise_lex::LexError::UnexpectedByte { pos, .. }
                | elise_lex::LexError::UnexpectedEof { pos } => *pos as usize,
                elise_lex::LexError::Message { pos, .. } => *pos as usize,
            };
            let pos = pos.min(src.len());
            let errors = vec![SyntaxError {
                start: pos as u32,
                end: src.len() as u32,
                message: format!("{err:?}"),
            }];
            let mut children = Vec::new();
            if !src.is_empty() {
                children.push(elise_core::GreenChild::Token {
                    kind: Tok::Eof as u16,
                    width: src.len() as u32,
                });
            }
            return SourceGraph::new(
                src,
                std::rc::Rc::new(elise_core::Green::new(NodeKind::File as u16, children)),
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
        while let Some((first_idx, _)) = self.peek_sig(self.pos) {
            // Absorb leading attributes (`#[...]`) and visibility
            // (`private`) into the item span — the legacy parser attaches
            // them to the following item rather than listing separately.
            let mut cursor = first_idx;
            loop {
                match self.peek_sig(cursor) {
                    Some((h, Tok::Hash)) => {
                        cursor =
                            self.skip_balanced(h + 1, Tok::LBracket, Tok::RBracket);
                    }
                    Some((p, Tok::Private)) => {
                        cursor = p + 1;
                    }
                    _ => break,
                }
            }

            let Some((item_idx, tok)) = self.peek_sig(cursor) else {
                // Trailing attributes with nothing after them: consume flat
                // so losslessness holds.
                while self.pos < self.rows.len() && self.rows[self.pos].len > 0 {
                    self.events.push(Event::Advance(1));
                    self.pos += 1;
                }
                break;
            };

            let start = first_idx;

            match tok {
                Tok::Import => {
                    let end = self.thru_terminator(item_idx + 1);
                    self.emit_flat_span(NodeKind::Import, start, end);
                    self.pos = end;
                }
                Tok::Type => {
                    let end = self.thru_terminator(item_idx + 1);
                    self.emit_flat_span(NodeKind::TypeAlias, start, end);
                    self.pos = end;
                }
                Tok::Extern => {
                    let kind = match self.peek_sig(item_idx + 1) {
                        Some((_, Tok::StrLit)) => match self.peek_sig(item_idx + 2) {
                            Some((_, Tok::LBrace)) => NodeKind::ExternBlock,
                            _ => NodeKind::ExternDecl,
                        },
                        Some((abi_end, _))
                            if matches!(self.peek_sig(abi_end), Some((_, Tok::LBrace))) =>
                        {
                            NodeKind::ExternBlock
                        }
                        _ => NodeKind::ExternDecl,
                    };
                    let end = match kind {
                        NodeKind::ExternBlock => self.thru_braces(item_idx + 1),
                        _ => self.thru_terminator(item_idx + 1),
                    };
                    self.emit_flat_span(kind, start, end);
                    self.pos = end;
                }
                Tok::Struct => {
                    let end = self.thru_braces(item_idx + 1);
                    self.emit_flat_span(NodeKind::Struct, start, end);
                    self.pos = end;
                }
                Tok::Enum => {
                    let end = self.thru_braces(item_idx + 1);
                    self.emit_flat_span(NodeKind::Enum, start, end);
                    self.pos = end;
                }
                Tok::Trait => {
                    let end = self.thru_braces(item_idx + 1);
                    self.emit_flat_span(NodeKind::Trait, start, end);
                    self.pos = end;
                }
                Tok::Impl => {
                    let end = self.thru_braces(item_idx + 1);
                    self.emit_flat_span(NodeKind::Impl, start, end);
                    self.pos = end;
                }
                Tok::Macro => {
                    let end = self.thru_braces(item_idx + 1);
                    self.emit_flat_span(NodeKind::Macro, start, end);
                    self.pos = end;
                }
                Tok::Const | Tok::Static | Tok::Volatile => {
                    let (end, _braces) = self.classify_tail(item_idx + 1);
                    let end = self.absorb_trailing_semi(end);
                    self.emit_flat_span(NodeKind::GlobalVariable, start, end);
                    self.pos = end;
                }
                _ => {
                    let (end, braces) = self.classify_tail(item_idx + 1);
                    if self.tail_is_function(item_idx + 1, end) {
                        self.emit_function(start, item_idx, end, braces);
                    } else {
                        let end = self.absorb_trailing_semi(end);
                        self.emit_flat_span(NodeKind::GlobalVariable, start, end);
                    }
                    self.pos = end;
                }
            }
        }

        // Any trailing rows that were never classified: consume flat so
        // losslessness always holds.
        while self.pos < self.rows.len() {
            let row = &self.rows[self.pos];
            if row.len == 0 {
                break;
            }
            self.events.push(Event::Advance(1));
            self.pos += 1;
        }
    }

    fn emit_flat_span(&mut self, kind: NodeKind, start: usize, end: usize) {
        self.events.push(Event::Enter(kind as u16));
        self.consume_range(start, end);
        self.events.push(Event::Exit);
    }

    /// Function items carry a structured Body child: header leaves, then
    /// statements/expressions parsed by [`BodyParser`], then the braces.
    fn emit_function(
        &mut self,
        start: usize,
        item_idx: usize,
        end: usize,
        braces: Option<(usize, usize)>,
    ) {
        self.events.push(Event::Enter(NodeKind::Function as u16));
        match braces {
            Some((open_idx, close_idx)) => {
                // Header up to and including `{`.
                self.consume_range(start, open_idx + 1);
                let mut events = std::mem::take(&mut self.events);
                let body_start = events.len();
                {
                    let mut body = crate::grammar::body::BodyParser::new(
                        self.rows,
                        open_idx + 1,
                        close_idx,
                        &mut events,
                        &mut self.errors,
                    );
                    body.parse_block_inner(close_idx);
                }
                // Verify event stream balance: Enters == Exits in body.
                let mut enters = 0usize;
                let mut exits = 0usize;
                for e in &events[body_start..] {
                    match e {
                        Event::Enter(_) => enters += 1,
                        Event::Exit => exits += 1,
                        _ => {}
                    }
                }
                if enters != exits {
                    events.truncate(body_start);
                    for row in &self.rows[open_idx + 1..close_idx] {
                        if row.len > 0 && !is_trivia(row.kind) {
                            events.push(Event::Advance(1));
                        }
                    }
                }
                self.events = events;
                // Closing brace of the body.
                self.pos = close_idx;
                self.advance_row();
            }
            None => {
                // No body braces found (malformed): consume to end flat.
                self.consume_range(item_idx + 1, end);
            }
        }
        self.events.push(Event::Exit);
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

    /// Emit `Advance(1)` per significant row in `[start, end)`.
    fn consume_range(&mut self, start: usize, end: usize) {
        for i in start..end.min(self.rows.len()) {
            let row = &self.rows[i];
            if !is_trivia(row.kind) && row.len > 0 {
                self.events.push(Event::Advance(1));
            }
        }
    }

    /// Advance past one significant row (e.g. a closing brace).
    fn advance_row(&mut self) {
        self.events.push(Event::Advance(1));
        self.pos += 1;
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
    /// Returns the tail end plus, for functions, the body brace indices.
    /// Distinguish function (header + block) from global (semicolon).
    ///
    /// Tracks paren/bracket/brace nesting. Key rules:
    /// - Top-level `=` before any braces → global with initializer.
    /// - Top-level `;` outside all groups → global.
    /// - `{` at paren-depth 0 AFTER seeing balanced parens → function body;
    ///   scan to its matching `}`.
    /// - `{` at paren-depth 0 WITHOUT prior balanced parens → initializer.
    fn classify_tail(&self, start: usize) -> (usize, Option<(usize, usize)>) {
        let mut i = start;
        let mut paren = 0i64;
        let mut bracket = 0i64;
        let mut brace = 0i64;
        let mut saw_balanced_parens = false;
        let mut seen_top_level_assign = false;
        while i < self.rows.len() {
            let row = &self.rows[i];
            if row.len == 0 {
                return (i, None);
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            match num_to_tok(row.kind) {
                Some(Tok::LParen) => paren += 1,
                Some(Tok::RParen) => {
                    paren -= 1;
                    if paren == 0 {
                        saw_balanced_parens = true;
                    }
                }
                Some(Tok::LBracket) => bracket += 1,
                Some(Tok::RBracket) => bracket -= 1,
                Some(Tok::Assign) if paren <= 0 && bracket <= 0 && brace == 0 => {
                    seen_top_level_assign = true;
                }
                Some(Tok::LBrace) => {
                    // Function bodies come after balanced param parens and
                    // without a preceding top-level `=`.
                    if saw_balanced_parens
                        && !seen_top_level_assign
                        && paren <= 0
                        && bracket <= 0
                    {
                        let open = i;
                        brace += 1;
                        i += 1;
                        while i < self.rows.len() {
                            let row = &self.rows[i];
                            if row.len == 0 {
                                return (i, None);
                            }
                            if !is_trivia(row.kind) {
                                match num_to_tok(row.kind) {
                                    Some(Tok::LBrace) => brace += 1,
                                    Some(Tok::RBrace) => {
                                        brace -= 1;
                                        if brace == 0 {
                                            return (i + 1, Some((open, i)));
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            i += 1;
                        }
                        return (i, None);
                    } else {
                        brace += 1;
                    }
                }
                Some(Tok::RBrace) => brace -= 1,
                // Top-level semicolon outside all groups terminates globals.
                Some(Tok::Semi)
                    if paren <= 0 && bracket <= 0 && brace <= 0 =>
                {
                    return (i + 1, None);
                }
                _ => {}
            }
            i += 1;
        }
        (i, None)
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
                Some(Tok::LParen)
                | Some(Tok::LBracket)
                | Some(Tok::LBrace) => depth += 1,
                Some(Tok::RParen)
                | Some(Tok::RBracket)
                | Some(Tok::RBrace) => depth = depth.saturating_sub(1),
                Some(Tok::Semi) if depth == 0 => return false,
                Some(Tok::Ident) | Some(Tok::SelfType) => prev_ident = true,
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
}

#[inline]
fn num_to_tok(kind: u16) -> Option<Tok> {
    Tok::from_discriminant(kind)
}

#[inline]
pub(crate) fn is_trivia(kind: u16) -> bool {
    kind >= Tok::TriviaLayout as u16
}
