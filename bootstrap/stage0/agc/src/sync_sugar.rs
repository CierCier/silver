//! Sugar for sharing data between threads, rewritten on the token stream
//! right after lexing so every later pass sees ordinary code:
//!
//! ```text
//! shared T                  =>  Mutex<T>
//! shared T name = init;     =>  Mutex<T> name = Mutex<T>.new(init);
//! lock place [as name] { body }
//!     =>  { let __lock_N_guard = place.lock();
//!           let __lock_N__name = __lock_N_guard.get_ptr();
//!           body, with each use of `name` written as (*__lock_N__name) }
//! ```
//!
//! The guard unlocks on every exit from the block. `shared` is a keyword only
//! before a type name and `lock` only at the start of a statement before a
//! place, so both stay usable as ordinary names (`m.lock()`, `RawMutex lock;`).
//! Synthesized tokens carry the span of the source token they replace, so
//! diagnostics inside a lock block point at the user's own code.

use crate::lexer::{LexToken, Span, Token};

/// A malformed `lock` statement, with the span to report it at.
#[derive(Debug, Clone)]
pub struct SugarError {
    pub message: String,
    pub span: Span,
}

/// Rewrites `shared` types and `lock` blocks in `tokens`.
pub fn desugar(tokens: Vec<LexToken>) -> Result<Vec<LexToken>, SugarError> {
    let shared = desugar_shared(&tokens);
    let mut counter = 0;
    desugar_locks(&shared, &mut counter)
}

fn token(kind: Token, text: &str, span: Span) -> LexToken {
    LexToken {
        kind,
        span,
        text: text.to_string(),
    }
}

fn identifier(name: &str, span: Span) -> LexToken {
    token(Token::Identifier(name.to_string()), name, span)
}

fn is_comment(token: &LexToken) -> bool {
    matches!(token.kind, Token::Comment { .. })
}

/// The index of the nearest non-comment token after `index`.
fn next_significant(tokens: &[LexToken], index: usize) -> Option<usize> {
    (index + 1..tokens.len()).find(|&i| !is_comment(&tokens[i]))
}

/// The index of the nearest non-comment token before `index`.
fn previous_significant(tokens: &[LexToken], index: usize) -> Option<usize> {
    (0..index).rev().find(|&i| !is_comment(&tokens[i]))
}

fn is_identifier(token: &LexToken, name: &str) -> bool {
    matches!(&token.kind, Token::Identifier(text) if text == name)
}

/// Primitive and builtin type keywords.
fn is_type_keyword(kind: &Token) -> bool {
    matches!(
        kind,
        Token::I8
            | Token::I16
            | Token::I32
            | Token::I64
            | Token::I128
            | Token::U8
            | Token::U16
            | Token::U32
            | Token::U64
            | Token::U128
            | Token::F32
            | Token::F64
            | Token::F80
            | Token::C32
            | Token::C64
            | Token::C80
            | Token::Bool
            | Token::Str
            | Token::Char
            | Token::Void
            | Token::Vec
            | Token::Optional
            | Token::SelfType
    )
}

fn is_type_start(kind: &Token) -> bool {
    matches!(kind, Token::Identifier(_)) || is_type_keyword(kind)
}

/// The end (exclusive) of the type name starting at `start`: its `a::b::T`
/// path and a balanced `<...>` argument list. A `*` after it is not part of
/// the shared type: `shared T*` is a pointer to a shared T.
fn type_end(tokens: &[LexToken], start: usize) -> usize {
    let mut last = start;
    while let Some(colons) = next_significant(tokens, last)
        && matches!(tokens[colons].kind, Token::DoubleColon)
        && let Some(segment) = next_significant(tokens, colons)
        && is_type_start(&tokens[segment].kind)
    {
        last = segment;
    }
    let Some(next) = next_significant(tokens, last) else {
        return last + 1;
    };
    if !matches!(tokens[next].kind, Token::Less) {
        return last + 1;
    }
    let mut depth = 0usize;
    for (i, token) in tokens.iter().enumerate().skip(next) {
        match token.kind {
            Token::Less => depth += 1,
            Token::Greater => {
                depth -= 1;
                if depth == 0 {
                    return i + 1;
                }
            }
            Token::Semicolon | Token::LeftBrace | Token::RightBrace => break,
            _ => {}
        }
    }
    last + 1
}

/// The index of the `;` that ends the expression starting at `start`.
fn expression_end(tokens: &[LexToken], start: usize) -> Option<usize> {
    let mut depth = 0i64;
    for (i, token) in tokens.iter().enumerate().skip(start) {
        match token.kind {
            Token::LeftParen | Token::LeftBracket | Token::LeftBrace => depth += 1,
            Token::RightParen | Token::RightBracket | Token::RightBrace => depth -= 1,
            Token::Semicolon if depth == 0 => return Some(i),
            _ => {}
        }
        if depth < 0 {
            return None;
        }
    }
    None
}

/// Expansion tags for synthesized code, above any macro expansion id.
const SUGAR_EXPANSION_BASE: u32 = 0x8000_0000;

/// What a synthesized token is for; keeps tags at one source span apart.
#[derive(Clone, Copy)]
enum Role {
    Shared = 0,
    Guard = 1,
    Pointer = 2,
}

/// `span` tagged as synthesized code for `role`. The tag depends only on
/// the source position, so lexing the same file twice yields the same
/// spans and span-keyed analyses keep matching.
fn synthesized(span: Span, role: Role) -> Span {
    let slot = (span.start as u32).wrapping_mul(4).wrapping_add(role as u32);
    Span {
        expansion: SUGAR_EXPANSION_BASE | (slot & !SUGAR_EXPANSION_BASE),
        ..span
    }
}

/// `Mutex<` + `ty` + `>`, every synthesized token at `span`.
fn mutex_of(ty: &[LexToken], span: Span, out: &mut Vec<LexToken>) {
    out.push(identifier("Mutex", span));
    out.push(token(Token::Less, "<", span));
    out.extend(desugar_shared(ty));
    out.push(token(Token::Greater, ">", span));
}

/// Rewrites `shared T` to `Mutex<T>`, and wraps the initializer of a
/// `shared T name = init;` declaration in `Mutex<T>.new(...)`.
fn desugar_shared(tokens: &[LexToken]) -> Vec<LexToken> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;
    while i < tokens.len() {
        let current = &tokens[i];
        // A keyword only before `T name` or `T* name`: `shared create()`
        // and `shared x;` keep `shared` as a type name.
        let keyword = is_identifier(current, "shared")
            && !previous_significant(tokens, i)
                .is_some_and(|p| matches!(tokens[p].kind, Token::Dot));
        let declared = keyword
            .then(|| next_significant(tokens, i))
            .flatten()
            .filter(|&start| is_type_start(&tokens[start].kind))
            .map(|start| (start, type_end(tokens, start)))
            .filter(|&(_, end)| {
                next_significant(tokens, end - 1).is_some_and(|n| {
                    matches!(tokens[n].kind, Token::Identifier(_) | Token::Star)
                })
            });
        let Some((start, end)) = declared else {
            out.push(current.clone());
            i += 1;
            continue;
        };
        let span = synthesized(current.span, Role::Shared);
        let ty = &tokens[start..end];
        mutex_of(ty, span, &mut out);
        i = end;
        // `shared T name = init;`: the initializer is a plain T.
        if let Some(name) = next_significant(tokens, end.saturating_sub(1))
            && matches!(tokens[name].kind, Token::Identifier(_))
            && let Some(assign) = next_significant(tokens, name)
            && matches!(tokens[assign].kind, Token::Assign)
            && let Some(semicolon) = expression_end(tokens, assign + 1)
        {
            out.extend(tokens[end..=assign].iter().cloned());
            mutex_of(ty, span, &mut out);
            out.push(token(Token::Dot, ".", span));
            out.push(identifier("new", span));
            out.push(token(Token::LeftParen, "(", span));
            out.extend(desugar_shared(&tokens[assign + 1..semicolon]));
            out.push(token(Token::RightParen, ")", span));
            i = semicolon;
        }
    }
    out
}

/// True when `index` begins a statement: the previous token ends one.
fn starts_statement(tokens: &[LexToken], index: usize) -> bool {
    match previous_significant(tokens, index) {
        None => true,
        Some(p) => matches!(
            tokens[p].kind,
            Token::Semicolon | Token::LeftBrace | Token::RightBrace
        ),
    }
}

/// The parsed head of `lock place [as name] {`.
struct LockHead {
    place: std::ops::Range<usize>,
    name: String,
    open: usize,
}

/// Parses a `lock` statement head at `index`, or returns `Ok(None)` when the
/// tokens are not a lock statement (for example a type named `lock`).
fn lock_head(tokens: &[LexToken], index: usize) -> Result<Option<LockHead>, SugarError> {
    let Some(place_start) = next_significant(tokens, index) else {
        return Ok(None);
    };
    if !matches!(tokens[place_start].kind, Token::Identifier(_)) {
        return Ok(None);
    }
    let mut depth = 0i64;
    let mut cursor = place_start;
    while cursor < tokens.len() {
        match tokens[cursor].kind {
            Token::LeftParen | Token::LeftBracket => depth += 1,
            Token::RightParen | Token::RightBracket => depth -= 1,
            Token::LeftBrace | Token::As if depth == 0 => break,
            Token::Semicolon | Token::Assign | Token::RightBrace | Token::Eof => {
                return Ok(None);
            }
            _ => {}
        }
        cursor += 1;
    }
    if cursor >= tokens.len() {
        return Ok(None);
    }
    let place = place_start..cursor;
    let single = place
        .clone()
        .filter(|&i| !is_comment(&tokens[i]))
        .count()
        == 1;
    if matches!(tokens[cursor].kind, Token::As) {
        let name = next_significant(tokens, cursor);
        let open = name.and_then(|n| next_significant(tokens, n));
        return match (name, open) {
            (Some(n), Some(o))
                if matches!(tokens[o].kind, Token::LeftBrace)
                    && matches!(tokens[n].kind, Token::Identifier(_)) =>
            {
                Ok(Some(LockHead {
                    place,
                    name: tokens[n].text.clone(),
                    open: o,
                }))
            }
            _ => Err(SugarError {
                message: "expected `lock <place> as <name> { ... }`".to_string(),
                span: tokens[cursor].span,
            }),
        };
    }
    if !single {
        return Err(SugarError {
            message: "name the locked value: `lock <place> as <name> { ... }`".to_string(),
            span: tokens[place_start].span,
        });
    }
    Ok(Some(LockHead {
        name: tokens[place_start].text.clone(),
        place,
        open: cursor,
    }))
}

/// The spelling of each non-comment token in `tokens`.
fn significant_text(tokens: &[LexToken]) -> Vec<&str> {
    tokens
        .iter()
        .filter(|token| !is_comment(token))
        .map(|token| token.text.as_str())
        .collect()
}

/// The user's name inside a generated `__lock_N__name` local, or None for
/// any other name (including the guard, `__lock_N_guard`).
pub fn locked_name(local: &str) -> Option<&str> {
    let rest = local.strip_prefix("__lock_")?;
    let digits = rest.find(|c: char| !c.is_ascii_digit())?;
    rest[digits..].strip_prefix("__")
}

/// The index of the `}` matching the `{` at `open`.
fn matching_brace(tokens: &[LexToken], open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (i, token) in tokens.iter().enumerate().skip(open) {
        match token.kind {
            Token::LeftBrace => depth += 1,
            Token::RightBrace => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }
    None
}

/// True when `name` at `index` is being declared rather than used: it
/// follows `let`, a type name, a primitive type, or `]`.
fn declares(tokens: &[LexToken], index: usize) -> bool {
    let Some(previous) = previous_significant(tokens, index) else {
        return false;
    };
    let declared_by = matches!(tokens[previous].kind, Token::Let | Token::RightBracket)
        || is_type_keyword(&tokens[previous].kind)
        || (matches!(tokens[previous].kind, Token::Identifier(_))
            && !previous_significant(tokens, previous)
                .is_some_and(|p| matches!(tokens[p].kind, Token::Dot)));
    declared_by
        && next_significant(tokens, index).is_some_and(|n| {
            matches!(
                tokens[n].kind,
                Token::Assign | Token::Semicolon | Token::Comma | Token::RightParen
            )
        })
}

/// Rewrites every `lock` statement in `tokens`, innermost blocks included.
fn desugar_locks(tokens: &[LexToken], counter: &mut usize) -> Result<Vec<LexToken>, SugarError> {
    let mut out = Vec::with_capacity(tokens.len());
    let mut i = 0;
    while i < tokens.len() {
        let head = if is_identifier(&tokens[i], "lock") && starts_statement(tokens, i) {
            lock_head(tokens, i)?
        } else {
            None
        };
        let Some(head) = head else {
            out.push(tokens[i].clone());
            i += 1;
            continue;
        };
        let lock_span = tokens[i].span;
        // The guard and pointer locals must not share a source range: some
        // passes key locals by byte range alone. The guard sits at `lock`,
        // the pointer at the block's `{`, each with its own expansion tag.
        let guard_span = synthesized(lock_span, Role::Guard);
        let pointer_span = synthesized(tokens[head.open].span, Role::Pointer);
        let close = matching_brace(tokens, head.open).ok_or_else(|| SugarError {
            message: "this lock block is never closed".to_string(),
            span: tokens[head.open].span,
        })?;
        let body = &tokens[head.open + 1..close];
        let place = significant_text(&tokens[head.place.clone()]);
        for (j, token) in body.iter().enumerate() {
            if is_identifier(token, "lock")
                && starts_statement(body, j)
                && let Some(inner) = lock_head(body, j)?
                && significant_text(&body[inner.place]) == place
            {
                return Err(SugarError {
                    message: format!(
                        "`{}` is already locked by an enclosing lock block; locking it again would deadlock",
                        place.concat()
                    ),
                    span: token.span,
                });
            }
            if is_identifier(token, &head.name) && declares(body, j) {
                return Err(SugarError {
                    message: format!(
                        "`{}` names the locked value inside `lock {} {{ ... }}`; declare a different name",
                        head.name, head.name
                    ),
                    span: token.span,
                });
            }
        }
        let body = desugar_locks(body, counter)?;
        // Named after the locked value so editor tooling can map it back.
        let value = format!("__lock_{}__{}", *counter, head.name);
        let guard = format!("{value}_guard");
        *counter += 1;

        out.push(token(Token::LeftBrace, "{", lock_span));
        out.push(token(Token::Let, "let", guard_span));
        out.push(identifier(&guard, guard_span));
        out.push(token(Token::Assign, "=", guard_span));
        let place = &tokens[head.place];
        let wrap = place
            .first()
            .is_some_and(|t| matches!(t.kind, Token::Star | Token::BitwiseAnd));
        if wrap {
            out.push(token(Token::LeftParen, "(", guard_span));
        }
        out.extend(place.iter().cloned());
        if wrap {
            out.push(token(Token::RightParen, ")", guard_span));
        }
        out.push(token(Token::Dot, ".", guard_span));
        out.push(identifier("lock", guard_span));
        out.push(token(Token::LeftParen, "(", guard_span));
        out.push(token(Token::RightParen, ")", guard_span));
        out.push(token(Token::Semicolon, ";", guard_span));
        out.push(token(Token::Let, "let", pointer_span));
        out.push(identifier(&value, pointer_span));
        out.push(token(Token::Assign, "=", pointer_span));
        out.push(identifier(&guard, pointer_span));
        out.push(token(Token::Dot, ".", pointer_span));
        out.push(identifier("get_ptr", pointer_span));
        out.push(token(Token::LeftParen, "(", pointer_span));
        out.push(token(Token::RightParen, ")", pointer_span));
        out.push(token(Token::Semicolon, ";", pointer_span));
        for (j, token_in_body) in body.iter().enumerate() {
            let field = previous_significant(&body, j)
                .is_some_and(|p| matches!(body[p].kind, Token::Dot));
            if is_identifier(token_in_body, &head.name) && !field {
                let span = token_in_body.span;
                out.push(token(Token::LeftParen, "(", span));
                out.push(token(Token::Star, "*", span));
                out.push(identifier(&value, span));
                out.push(token(Token::RightParen, ")", span));
            } else {
                out.push(token_in_body.clone());
            }
        }
        out.push(token(Token::RightBrace, "}", tokens[close].span));
        i = close + 1;
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    /// The rewritten source of `source`, tokens joined by spaces.
    fn rewritten(source: &str) -> String {
        let tokens = crate::lexer::lex(source).expect("lexes");
        let texts: Vec<&str> = tokens
            .iter()
            .filter(|token| !matches!(token.kind, crate::lexer::Token::Eof))
            .map(|token| token.text.as_str())
            .collect();
        texts.join(" ")
    }

    #[test]
    fn shared_wraps_the_whole_type_name() {
        assert_eq!(
            rewritten("shared a::b::Thing<i64> item = make();"),
            "Mutex < a :: b :: Thing < i64 > > item = Mutex < a :: b :: Thing < i64 > > . new ( make ( ) ) ;"
        );
        // `shared` binds to the type name, so this is a pointer to a shared
        // i64, initialized with a pointer.
        assert_eq!(rewritten("shared i64* p = &total;"), "Mutex < i64 > * p = & total ;");
    }

    #[test]
    fn shared_without_a_declared_name_stays_a_name() {
        assert_eq!(rewritten("shared create() { }"), "shared create ( ) { }");
        assert_eq!(rewritten("shared value;"), "shared value ;");
        assert_eq!(rewritten("shared i64 count;"), "Mutex < i64 > count ;");
    }
}
