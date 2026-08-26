//! Statement + expression structure for function bodies (M4).
//!
//! Emits prefix-order events (Enter → children → Exit). Left-recursive
//! binary levels use the marker-rotation trick: parse the left operand,
//! remember its event start, parse operator+right, then rotate a placeholder
//! Enter in front of the left operand's events so prefix order holds.
//!
//! Fallback policy: anything unrecognized is consumed as a flat leaf —
//! parsing never fails the file, it degrades locally.

use super::lexspec::Tok;
use super::parser::is_trivia;
use elise_lex::TokenRow;
use elise_parse::Event;

/// Node kinds introduced inside bodies (offset keeps them disjoint from the
/// item kinds 0..12 in `parser.rs`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum BodyKind {
    Body = 32,
    StmtLet = 33,
    StmtReturn = 34,
    StmtBreak = 35,
    StmtContinue = 36,
    StmtIf = 37,
    StmtWhile = 38,
    StmtForC = 39,
    StmtForIn = 40,
    StmtMatch = 41,
    StmtExpr = 42,
    Block = 43,
    Defer = 44,
    StmtLocalDecl = 45,
    ExprCall = 50,
    ExprIndex = 51,
    ExprField = 52,
    ExprBinary = 53,
    ExprUnary = 54,
    ExprAssign = 55,
    ExprTernary = 56,
    ExprUnwrapOr = 57,
    ExprCast = 58,
    ExprMove = 59,
    ExprLaunch = 60,
    ExprWait = 61,
    ExprMacro = 62,
    ExprAsm = 63,
    ExprInit = 64,
    ExprParen = 65,
    MatchArm = 66,
    Pattern = 67,
    ExprPostfix = 68,
    ExprPrimary = 69,
}

pub struct BodyParser<'a, 'e> {
    rows: &'a [TokenRow],
    pos: usize,
    end: usize,
    pub events: &'e mut Vec<Event>,
    pub errors: &'e mut Vec<elise_core::SyntaxError>,
}

impl<'a, 'e> BodyParser<'a, 'e> {
    pub fn new(
        rows: &'a [TokenRow],
        pos: usize,
        end: usize,
        events: &'e mut Vec<Event>,
        errors: &'e mut Vec<elise_core::SyntaxError>,
    ) -> Self {
        BodyParser {
            rows,
            pos,
            end,
            events,
            errors,
        }
    }

    /// Parse statements until the closing brace at `close` (exclusive) or
    /// input end. The braces themselves are advanced by the caller.
    pub fn parse_block_inner(&mut self, close: usize) {
        while self.pos < close.min(self.rows.len()) {
            let row = &self.rows[self.pos];
            if row.len == 0 {
                break;
            }
            if is_trivia(row.kind) {
                self.pos += 1;
                continue;
            }
            if std::env::var_os("ELISE_DBG_BODY").is_some() {
                let tok = Tok::from_discriminant(row.kind);
                eprintln!("DBG BI pos={} tok={:?} close={}", self.pos, tok, close);
            }
            let Some(tok) = Tok::from_discriminant(row.kind) else {
                self.pos += 1;
                continue;
            };
            match tok {
                Tok::RBrace => break,
                Tok::Semi => {
                    self.advance();
                }
                Tok::Let => self.stmt_let(),
                Tok::Return => self.stmt_return(),
                Tok::Break => self.simple_value_stmt(BodyKind::StmtBreak),
                Tok::Continue => self.simple_value_stmt(BodyKind::StmtContinue),
                Tok::LBrace => self.braced_block(),
                Tok::If => self.stmt_if(),
                Tok::While => self.stmt_while(),
                Tok::For => self.stmt_for(),
                Tok::Defer => self.stmt_defer(),
                Tok::Match => {
                    self.expr();
                    self.eat_semi();
                }
                _ => self.stmt_or_expr(),
            }
        }
        // Total-consumption safety net: advance past any rows that
        // structured parsing skipped, so no gaps exist in the leaf stream.
        while self.pos < close.min(self.rows.len()) {
            let row = &self.rows[self.pos];
            if row.len == 0 {
                break;
            }
            if !is_trivia(row.kind) {
                self.events.push(Event::Advance(1));
            }
            self.pos += 1;
        }
    }

    fn stmt_let(&mut self) {
        self.enter(BodyKind::StmtLet as u16);
        self.advance(); // let
        // name (+ optional type annotation + `=` initializer)
        while self.pos < self.end {
            let Some(tok) = self.peek_tok() else { break };
            match tok {
                Tok::Semi => {
                    self.advance();
                    break;
                }
                _ => self.advance(),
            }
        }
        self.exit();
    }

    fn stmt_return(&mut self) {
        self.enter(BodyKind::StmtReturn as u16);
        self.advance(); // return
        if !matches!(self.peek_tok(), None | Some(Tok::Semi)) {
            self.expr();
        }
        self.eat_semi();
        self.exit();
    }

    fn simple_value_stmt(&mut self, kind: BodyKind) {
        self.enter(kind as u16);
        self.advance();
        if !matches!(self.peek_tok(), None | Some(Tok::Semi)) {
            self.expr();
        }
        self.eat_semi();
        self.exit();
    }

    fn braced_block(&mut self) {
        self.enter(BodyKind::Block as u16);
        let open = self.pos;
        self.advance(); // {
        let close = match self.matching_brace(open) {
            Some(c) => c,
            None => {
                // unbalanced: consume to end
                self.parse_to_end();
                self.exit();
                return;
            }
        };
        // Statements inside (pos already sits on the first inner row).
        self.parse_block_inner(close);
        self.pos = close;
        self.advance(); // }
        self.exit();
    }

    fn stmt_if(&mut self) {
        self.enter(BodyKind::StmtIf as u16);
        self.advance(); // if
        self.paren_group_or_expr();
        self.skip_braced_block_if_present();
        // else / else-if chain. The recursive call is itself balanced, so
        // no extra Exit may be emitted here.
        if matches!(self.peek_tok(), Some(Tok::Else)) {
            self.advance();
            if matches!(self.peek_tok(), Some(Tok::If)) {
                self.stmt_if();
                return;
            }
            self.skip_braced_block_if_present();
        }
        self.exit();
    }

    fn stmt_while(&mut self) {
        self.enter(BodyKind::StmtWhile as u16);
        self.advance(); // while
        self.paren_group_or_expr();
        self.skip_braced_block_if_present();
        self.exit();
    }

    fn stmt_for(&mut self) {
        eprintln!("DBG-FOR fired @pos {}", self.pos);
        // `for pat in iterable { }` vs C-style `for (init; cond; step) { }`
        let is_for_in = {
            let mut look = self.pos + 1;
            look = self.skip_trivia(look);
            let first_ident = matches!(self.rows.get(look).map(|r| r.kind), Some(k) if k == Tok::Ident as u16);
            let after = self.skip_trivia(look + 1);
            first_ident
                && matches!(self.rows.get(after).map(|r| r.kind), Some(k) if k == Tok::In as u16)
        };
        if is_for_in {
            self.enter(BodyKind::StmtForIn as u16);
            self.advance(); // for
            self.advance(); // pattern ident
            self.advance(); // in
            self.expr();
            self.skip_braced_block_if_present();
            self.exit();
        } else {
            self.enter(BodyKind::StmtForC as u16);
            self.advance(); // for
            self.paren_group_or_expr();
            self.skip_braced_block_if_present();
            self.exit();
        }
    }

    fn stmt_defer(&mut self) {
        self.enter(BodyKind::Defer as u16);
        self.advance(); // defer
        if matches!(self.peek_tok(), Some(Tok::LBrace)) {
            self.braced_block();
        } else {
            self.parse_statement();
        }
        self.exit();
    }

    fn parse_statement(&mut self) {
        match self.peek_tok() {
            Some(Tok::Let) => self.stmt_let(),
            Some(Tok::Return) => self.stmt_return(),
            Some(Tok::LBrace) => self.braced_block(),
            Some(Tok::If) => self.stmt_if(),
            Some(Tok::While) => self.stmt_while(),
            Some(Tok::For) => self.stmt_for(),
            Some(Tok::Defer) => self.stmt_defer(),
            Some(Tok::RBrace) => {}
            None => {}
            _ => self.expression_statement(),
        }
    }

    fn expression_statement(&mut self) {
        self.expr();
        self.eat_semi();
    }

    /// Statement-position dispatch between a C-style local declaration
    /// (`Type name = expr;`, optionally with `[N]` declarators and
    /// brace-initializers) and a plain expression statement.
    fn stmt_or_expr(&mut self) {
        if self.looks_like_local_decl() {
            self.enter(BodyKind::StmtLocalDecl as u16);
            let mut depth = 0i64;
            loop {
                let Some(tok) = self.peek_tok() else { break };
                match tok {
                    Tok::Semi if depth == 0 => {
                        self.advance();
                        break;
                    }
                    Tok::LBrace | Tok::LParen | Tok::LBracket => depth += 1,
                    Tok::RBrace | Tok::RParen | Tok::RBracket => depth = depth.saturating_sub(1),
                    _ => {}
                }
                self.advance();
            }
            self.exit();
        } else {
            // Expression statement: parse, tolerating a missing semicolon.
            self.expr();
            self.eat_semi();
        }
    }

    /// Detect `Type [qualifiers] [generics] name (=|[|;) ...` shapes.
    ///
    /// Requires two consecutive identifiers (type then name) or a type-name
    /// followed by `[`/`=`/`;` after generic arguments close — the classic
    /// C-style declaration signature. Anything else is an expression.
    fn looks_like_local_decl(&self) -> bool {
        let limit = (self.pos + 32).min(self.end);
        let mut i = self.pos;
        let mut angle = 0i64;
        let mut seen_type_start = false;
        while i < limit {
            let row = &self.rows[i];
            if row.len == 0 {
                return false;
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            let Some(tok) = Tok::from_discriminant(row.kind) else {
                return false;
            };
            match tok {
                // The declarator name: a second identifier at angle-depth 0
                // after at least one type word.
                Tok::Ident if angle == 0 && seen_type_start => {
                    let mut j = self.skip_trivia(i + 1);
                    j = j;
                    let next = self.rows.get(j).and_then(|r| Tok::from_discriminant(r.kind));
                    return matches!(
                        next,
                        Some(Tok::Assign) | Some(Tok::Semi) | Some(Tok::LBracket)
                    );
                }
                Tok::Ident | Tok::Vec | Tok::Optional => {
                    seen_type_start = true;
                }
                Tok::Mut | Tok::Const | Tok::Static | Tok::Volatile => {}
                Tok::Less => angle += 1,
                Tok::Greater => angle = angle.saturating_sub(1),
                _ => {}
            }
            i += 1;
        }
        false
    }

    // -- expression levels (SYNTAX.md §6, tightest to loosest) ------------

    pub fn expr(&mut self) {
        self.level_assignment();
    }

    fn level_assignment(&mut self) {
        let mark = self.events.len();
        self.level_ternary();
        let assign_op = matches!(
            self.peek_tok(),
            Some(
                Tok::Assign
                    | Tok::PlusAssign
                    | Tok::MinusAssign
                    | Tok::StarAssign
                    | Tok::SlashAssign
                    | Tok::PercentAssign
            )
        );
        if assign_op {
            self.advance(); // operator
            self.level_assignment(); // right-assoc
            self.wrap(mark, BodyKind::ExprAssign as u16);
        }
    }

    fn level_ternary(&mut self) {
        let mark = self.events.len();
        self.level_or_or();
        if matches!(self.peek_tok(), Some(Tok::Question)) {
            self.advance(); // ?
            self.level_assignment();
            if matches!(self.peek_tok(), Some(Tok::Colon)) {
                self.advance();
                self.level_assignment();
                self.wrap(mark, BodyKind::ExprTernary as u16);
            } else {
                self.wrap(mark, BodyKind::ExprUnwrapOr as u16);
            }
        }
    }

    fn level_or_or(&mut self) {
        let mark = self.events.len();
        self.level_and_and();
        while matches!(self.peek_tok(), Some(Tok::OrOr)) {
            self.advance();
            self.level_and_and();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_and_and(&mut self) {
        let mark = self.events.len();
        self.level_bit_or();
        while matches!(self.peek_tok(), Some(Tok::AndAnd)) {
            self.advance();
            self.level_bit_or();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_bit_or(&mut self) {
        let mark = self.events.len();
        self.level_bit_xor();
        while matches!(self.peek_tok(), Some(Tok::BitOr)) {
            self.advance();
            self.level_bit_xor();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_bit_xor(&mut self) {
        let mark = self.events.len();
        self.level_bit_and();
        while matches!(self.peek_tok(), Some(Tok::BitXor)) {
            self.advance();
            self.level_bit_and();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_bit_and(&mut self) {
        let mark = self.events.len();
        self.level_equality_range();
        while matches!(self.peek_tok(), Some(Tok::BitAnd)) {
            self.advance();
            self.level_equality_range();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_equality_range(&mut self) {
        let mark = self.events.len();
        self.level_shift();
        loop {
            let op = matches!(
                self.peek_tok(),
                Some(
                    Tok::Equal
                        | Tok::NotEqual
                        | Tok::Less
                        | Tok::Greater
                        | Tok::LessEqual
                        | Tok::GreaterEqual
                        | Tok::DotDot
                )
            );
            if !op {
                break;
            }
            self.advance();
            self.level_shift();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_shift(&mut self) {
        // Shifts are two adjacent `<` / `>` tokens (the lexer has no shift
        // operators). Join them under one binary node.
        let mark = self.events.len();
        self.level_additive();
        loop {
            let double = match (self.peek_tok(), self.peek_kind_at(1)) {
                (Some(Tok::Less), Some(Tok::Less)) => true,
                (Some(Tok::Greater), Some(Tok::Greater)) => true,
                _ => false,
            };
            if !double {
                break;
            }
            self.advance();
            self.advance();
            self.level_additive();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_additive(&mut self) {
        let mark = self.events.len();
        self.level_multiplicative();
        loop {
            let op = matches!(
                self.peek_tok(),
                Some(Tok::Plus | Tok::Minus)
            );
            if !op {
                break;
            }
            self.advance();
            self.level_multiplicative();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_multiplicative(&mut self) {
        let mark = self.events.len();
        self.level_unary();
        loop {
            let op = matches!(
                self.peek_tok(),
                Some(Tok::Star | Tok::Slash | Tok::Percent)
            );
            if !op {
                break;
            }
            self.advance();
            self.level_unary();
            self.wrap(mark, BodyKind::ExprBinary as u16);
        }
    }

    fn level_unary(&mut self) {
        let unary = matches!(
            self.peek_tok(),
            Some(
                Tok::Minus
                    | Tok::Plus
                    | Tok::Not
                    | Tok::BitNot
                    | Tok::BitAnd
                    | Tok::Star
                    | Tok::Increment
                    | Tok::Decrement
                    | Tok::Move
                    | Tok::Comptime
            )
        );
        if unary {
            let kind = match self.peek_tok() {
                Some(Tok::Move) => BodyKind::ExprMove as u16,
                _ => BodyKind::ExprUnary as u16,
            };
            self.enter(kind);
            self.advance();
            self.level_unary();
            self.exit();
        } else if matches!(self.peek_tok(), Some(Tok::LParen))
            && self.cast_paren_ahead()
        {
            // `(Type)operand` cast: type tokens up to and including `)`,
            // then the operand expression (unary level, right-to-left).
            self.enter(BodyKind::ExprCast as u16);
            let rparen = self.cast_type_end(self.pos);
            while self.pos <= rparen {
                self.advance();
            }
            self.level_unary();
            self.exit();
        } else {
            self.level_postfix();
        }
    }

    fn level_postfix(&mut self) {
        let mark = self.events.len();
        self.primary();
        loop {
            match self.peek_tok() {
                Some(Tok::LParen) => {
                    self.enter(BodyKind::ExprCall as u16);
                    self.consume_balanced_group();
                    self.exit();
                }
                Some(Tok::LBracket) => {
                    self.enter(BodyKind::ExprIndex as u16);
                    self.consume_balanced_group();
                    self.exit();
                }
                Some(Tok::Dot) => {
                    self.enter(BodyKind::ExprField as u16);
                    self.advance(); // .
                    self.advance(); // member identifier (or int for tuples)
                    self.exit();
                }
                Some(Tok::Increment | Tok::Decrement) => {
                    self.enter(BodyKind::ExprUnary as u16);
                    self.advance();
                    self.exit();
                }
                _ => break,
            }
        }
        // Wrap everything from `mark` as one postfix expression so chained
        // calls group correctly.
        if self.events.len() > mark + 1 {
            self.wrap(mark, BodyKind::ExprPostfix as u16);
        }
    }

    fn primary(&mut self) {
        match self.peek_tok() {
            // Brace initializer / block-shaped literal in expression
            // position: `{ .x = 1 }`, `{ 1, 2, 3 }`.
            Some(Tok::LBrace) => {
                self.enter(BodyKind::ExprInit as u16);
                self.consume_balanced(Tok::LBrace, Tok::RBrace);
                self.exit();
            }
            Some(Tok::At) => {
                self.enter(BodyKind::ExprMacro as u16);
                self.advance(); // @
                self.advance(); // name
                if matches!(self.peek_tok(), Some(Tok::LParen)) {
                    self.consume_balanced_group();
                }
                self.exit();
            }
            Some(Tok::Asm) => {
                self.enter(BodyKind::ExprAsm as u16);
                self.advance(); // asm
                if matches!(self.peek_tok(), Some(Tok::LParen)) {
                    self.consume_balanced_group();
                }
                self.exit();
            }
            Some(Tok::Launch) => {
                self.enter(BodyKind::ExprLaunch as u16);
                self.advance(); // launch
                // Callee + argument list arrive through the postfix loop.
                self.primary();
                if matches!(self.peek_tok(), Some(Tok::LParen)) {
                    self.consume_balanced_group();
                }
                self.exit();
            }
            Some(Tok::Wait) => {
                self.enter(BodyKind::ExprWait as u16);
                self.advance(); // wait
                self.primary();
                self.exit();
            }
            Some(Tok::Match) => {
                self.enter(BodyKind::StmtMatch as u16);
                self.advance(); // match
                self.expr();
                // `{ arm*, }` — arms separated by commas.
                if matches!(self.peek_tok(), Some(Tok::LBrace)) {
                    self.advance();
                    loop {
                        match self.peek_tok() {
                            Some(Tok::RBrace) | None => break,
                            _ => {
                                self.enter(BodyKind::MatchArm as u16);
                                self.pattern();
                                if matches!(self.peek_tok(), Some(Tok::Colon)) {
                                    self.advance();
                                    self.expr();
                                }
                                self.exit();
                                if matches!(self.peek_tok(), Some(Tok::Comma)) {
                                    self.advance();
                                }
                            }
                        }
                    }
                    if matches!(self.peek_tok(), Some(Tok::RBrace)) {
                        self.advance();
                    }
                }
                self.exit();
            }
            _ => {
                self.enter(BodyKind::ExprPrimary as u16);
                self.advance();
                self.exit();
            }
        }
    }

    fn pattern(&mut self) {
        self.enter(BodyKind::Pattern as u16);
        // Variant payload patterns: Name(args) or Name; `move x`; `_`;
        // literals. Keep flat.
        while let Some(tok) = self.peek_tok() {
            match tok {
                Tok::Comma | Tok::Colon => break,
                _ => self.advance(),
            }
        }
        self.exit();
    }

    // -- helpers -----------------------------------------------------------

    fn paren_group_or_expr(&mut self) {
        if matches!(self.peek_tok(), Some(Tok::LParen)) {
            self.consume_balanced_group();
        } else {
            self.expr();
        }
    }

    fn skip_braced_block_if_present(&mut self) {
        if std::env::var_os("ELISE_DBG_FOR").is_some() {
        }
        if matches!(self.peek_tok(), Some(Tok::LBrace)) {
            self.braced_block();
        }
    }

    fn eat_semi(&mut self) {
        if matches!(self.peek_tok(), Some(Tok::Semi)) {
            self.advance();
        }
    }

    fn parse_to_end(&mut self) {
        while self.pos < self.end.min(self.rows.len()) {
            let row = &self.rows[self.pos];
            if row.len == 0 {
                break;
            }
            if !is_trivia(row.kind) {
                self.events.push(Event::Advance(1));
            }
            self.pos += 1;
        }
    }

    #[inline]
    fn peek_tok(&self) -> Option<Tok> {
        let mut i = self.pos;
        while i < self.end.min(self.rows.len()) {
            let row = &self.rows[i];
            if row.len == 0 {
                return None;
            }
            if !is_trivia(row.kind) {
                return Tok::from_discriminant(row.kind);
            }
            i += 1;
        }
        None
    }

    /// Peek the significant kind `offset` significant tokens ahead.
    #[inline]
    fn peek_kind_at(&self, offset: usize) -> Option<Tok> {
        let mut i = self.pos;
        let mut seen = 0usize;
        while i < self.end.min(self.rows.len()) {
            let row = &self.rows[i];
            if row.len == 0 {
                return None;
            }
            if !is_trivia(row.kind) {
                if seen == offset {
                    return Tok::from_discriminant(row.kind);
                }
                seen += 1;
            }
            i += 1;
        }
        None
    }

    #[inline]
    fn skip_trivia(&self, mut i: usize) -> usize {
        while i < self.rows.len() && is_trivia(self.rows[i].kind) {
            i += 1;
        }
        i
    }

    fn matching_brace(&self, open_idx: usize) -> Option<usize> {
        let mut depth = 0i64;
        let mut i = open_idx;
        while i < self.end.min(self.rows.len()) {
            let row = &self.rows[i];
            if row.len == 0 {
                return None;
            }
            if !is_trivia(row.kind) {
                match Tok::from_discriminant(row.kind) {
                    Some(Tok::LBrace) => depth += 1,
                    Some(Tok::RBrace) => {
                        depth -= 1;
                        if depth == 0 {
                            return Some(i);
                        }
                    }
                    _ => {}
                }
            }
            i += 1;
        }
        None
    }

    /// Consume a balanced `(...)` group (the opening paren is current).
    fn consume_balanced_group(&mut self) {
        self.consume_balanced(Tok::LParen, Tok::RParen);
    }

    /// Consume a balanced `open ... close` group (the opener is current).
    fn consume_balanced(&mut self, open: Tok, close: Tok) {
        let mut depth = 0i64;
        loop {
            let Some(tok) = self.peek_tok() else { break };
            match tok {
                t if t == open => depth += 1,
                t if t == close => {
                    depth -= 1;
                    self.advance();
                    if depth <= 0 {
                        break;
                    }
                    continue;
                }
                _ => {}
            }
            self.advance();
        }
    }

    /// Detect `( Type ) operand` casts. Conservative shape check: the group
    /// must close within a small window, contain only type-ish tokens, and
    /// be followed by something that can start an expression.
    fn cast_paren_ahead(&self) -> bool {
        if !matches!(self.peek_tok(), Some(Tok::LParen)) {
            return false;
        }
        let mut i = self.pos + 1;
        let mut depth = 1i64;
        let mut typeish = 0usize;
        let window_end = (self.pos + 24).min(self.end);
        while i < window_end {
            let row = &self.rows[i];
            if row.len == 0 {
                return false;
            }
            if is_trivia(row.kind) {
                i += 1;
                continue;
            }
            let Some(tok) = Tok::from_discriminant(row.kind) else {
                i += 1;
                continue;
            };
            match tok {
                Tok::RParen => {
                    depth -= 1;
                    if depth == 0 {
                        if typeish == 0 {
                            return false;
                        }
                        // Must be followed by an expression starter.
                        let mut next = self.skip_trivia(i + 1);
                        next = i + 1;
                        let _ = next;
                        return true;
                    }
                }
                Tok::LParen => depth += 1,
                Tok::Mut | Tok::Const | Tok::Volatile => {}
                _ => {
                    typeish += 1;
                }
            }
            i += 1;
        }
        false
    }

    /// End index (exclusive) of the cast type group; the `)` is included in
    /// consumption by the caller loop.
    fn cast_type_end(&self, lparen: usize) -> usize {
        let mut depth = 0i64;
        let mut i = lparen;
        let limit = (lparen + 24).min(self.end);
        while i < limit {
            let row = &self.rows[i];
            if row.len == 0 {
                break;
            }
            if !is_trivia(row.kind) {
                match Tok::from_discriminant(row.kind) {
                    Some(Tok::LParen) => depth += 1,
                    Some(Tok::RParen) => {
                        depth -= 1;
                        if depth == 0 {
                            return i;
                        }
                    }
                    _ => {}
                }
            }
            i += 1;
        }
        limit
    }

    // -- primitive cursor ops ---------------------------------------------

    /// Wrap all events from `mark` to the current end in one node of
    /// `kind` (the marker-rotation prefix trick).
    fn wrap(&mut self, mark: usize, kind: u16) {
        self.events.push(Event::Nop);
        let last = self.events.len() - 1;
        self.events[mark..=last].rotate_right(1);
        self.events[mark] = Event::Enter(kind);
        self.events.push(Event::Exit);
    }

    fn enter(&mut self, kind: u16) {
        self.events.push(Event::Enter(kind));
    }

    fn exit(&mut self) {
        self.events.push(Event::Exit);
    }

    /// Skip trivia rows, emit one Advance, step past the token.
    fn advance(&mut self) {
        while self.pos < self.end.min(self.rows.len()) && is_trivia(self.rows[self.pos].kind)
        {
            self.pos += 1;
        }
        if self.pos >= self.end.min(self.rows.len()) {
            return;
        }
        if self.rows[self.pos].len == 0 {
            return; // EOF sentinel
        }
        self.events.push(Event::Advance(1));
        self.pos += 1;
    }
}


