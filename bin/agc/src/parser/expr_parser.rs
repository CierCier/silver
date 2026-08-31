//! Recursive-descent expression parser (the "bootstrap parser" half of
//! the hybrid parser). Top-level item prediction is table-driven in
//! `prt_parser`; this module parses expression token ranges into AST
//! nodes with a simple cursor.

use crate::lexer::{LexToken, Span, Token};
use crate::parser::{ParseError, ast};

/// A callback the expression parser invokes when it needs to parse a block
/// of statements (currently only inside match-arm `{ ... }` bodies).  The
/// PRT parser supplies its own `parse_block_reduction`; when `None` (legacy
/// callers / unit tests) the block is emitted with an empty statement list
/// (preserving the old behaviour).
type BlockParser<'a> =
    Option<Box<dyn 'a + FnMut(&[LexToken], usize, usize) -> Result<ast::Block, ParseError>>>;

struct ExprCursor<'a> {
    tokens: &'a [LexToken],
    pos: usize,
    end: usize,
    block_parser: BlockParser<'a>,
}

impl<'a> ExprCursor<'a> {
    fn current(&self) -> Option<&'a LexToken> {
        if self.pos < self.end {
            self.tokens.get(self.pos)
        } else {
            None
        }
    }

    fn bump(&mut self) {
        self.pos += 1;
    }
}

fn parse_type_in_parens(tokens: &[LexToken], start: usize, end: usize) -> Option<ast::Type> {
    if start >= end {
        return None;
    }

    let mut cursor = start;
    let mut is_const = false;
    let mut type_volatile = false;
    loop {
        match tokens.get(cursor).map(|t| &t.kind) {
            Some(Token::Const) => {
                is_const = true;
                cursor += 1;
            }
            Some(Token::Volatile) => {
                type_volatile = true;
                cursor += 1;
            }
            _ => break,
        }
        if cursor >= end {
            return None;
        }
    }

    // Reference types: `&T` / `&mut T` / `&'a T` / `&'a mut T` inside casts.
    if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::BitwiseAnd)) {
        cursor += 1;
        let lifetime = if let Some(Token::Lifetime(lt_name)) = tokens.get(cursor).map(|t| &t.kind) {
            let lt_span = tokens[cursor].span;
            cursor += 1;
            Some(ast::Lifetime {
                name: lt_name.clone(),
                span: lt_span,
            })
        } else {
            None
        };
        let is_mutable = if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Mut)) {
            cursor += 1;
            true
        } else {
            false
        };
        let inner = parse_type_in_parens(tokens, cursor, end)?;
        let span = tokens[start].span.extend_to(&tokens[end - 1].span);
        return Some(ast::Type {
            kind: Box::new(ast::TypeKind::Reference(ast::ReferenceType {
                is_mutable,
                lifetime,
                inner: Box::new(inner),
            })),
            span,
        });
    }

    let base = match tokens.get(cursor)?.kind {
        Token::I8 => ast::TypeKind::Primitive(ast::PrimitiveType::I8),
        Token::I16 => ast::TypeKind::Primitive(ast::PrimitiveType::I16),
        Token::I32 => ast::TypeKind::Primitive(ast::PrimitiveType::I32),
        Token::I64 => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
        Token::I128 => ast::TypeKind::Primitive(ast::PrimitiveType::I128),
        Token::U8 => ast::TypeKind::Primitive(ast::PrimitiveType::U8),
        Token::U16 => ast::TypeKind::Primitive(ast::PrimitiveType::U16),
        Token::U32 => ast::TypeKind::Primitive(ast::PrimitiveType::U32),
        Token::U64 => ast::TypeKind::Primitive(ast::PrimitiveType::U64),
        Token::U128 => ast::TypeKind::Primitive(ast::PrimitiveType::U128),
        Token::F32 => ast::TypeKind::Primitive(ast::PrimitiveType::F32),
        Token::F64 => ast::TypeKind::Primitive(ast::PrimitiveType::F64),
        Token::F80 => ast::TypeKind::Primitive(ast::PrimitiveType::F80),
        Token::Bool => ast::TypeKind::Primitive(ast::PrimitiveType::Bool),
        Token::Str => ast::TypeKind::Primitive(ast::PrimitiveType::Str),
        Token::Char => ast::TypeKind::Primitive(ast::PrimitiveType::Char),
        Token::Void => ast::TypeKind::Primitive(ast::PrimitiveType::Void),
        Token::Identifier(ref name) => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: name.clone(),
                span: tokens[cursor].span,
            }],
            generics: None,
        }),
        Token::Vec => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "Vec".to_string(),
                span: tokens[cursor].span,
            }],
            generics: None,
        }),
        Token::Optional => ast::TypeKind::Named(ast::NamedType {
            path: vec![ast::Identifier {
                name: "Optional".to_string(),
                span: tokens[cursor].span,
            }],
            generics: None,
        }),
        _ => return None,
    };
    let from_identifier = matches!(tokens[cursor].kind, Token::Identifier(_));
    let mut ty = ast::Type {
        kind: Box::new(base),
        span: tokens[cursor].span,
    };
    cursor += 1;

    // Function-pointer cast target: `void(i64)*`, `(i32(i64, bool))*`, etc.
    // After the base (return) type, a `(` group of type-like tokens forms a
    // function type; the `*` loop below then wraps it in a pointer type.
    if cursor < end
        && matches!(tokens[cursor].kind, Token::LeftParen)
        && let Some(close) = find_matching_paren(tokens, cursor, end)
        && close > cursor + 1
        && tokens[(cursor + 1)..close]
            .iter()
            .all(|t| is_type_like(&t.kind))
    {
        let mut parsed_types = Vec::new();
        let mut arg_cursor = cursor + 1;
        let mut ok = true;
        while arg_cursor < close {
            if let Some((arg, next_arg)) = parse_paren_fn_arg(tokens, arg_cursor, close) {
                parsed_types.push(arg);
                arg_cursor = next_arg;
                if arg_cursor < close && matches!(tokens[arg_cursor].kind, Token::Comma) {
                    arg_cursor += 1;
                } else if arg_cursor < close {
                    ok = false;
                    break;
                }
            } else {
                ok = false;
                break;
            }
        }
        if ok {
            let start_span = ty.span.start;
            ty = ast::Type {
                kind: Box::new(ast::TypeKind::Function(ast::FunctionType {
                    parameters: parsed_types,
                    return_type: Box::new(ty),
                })),
                span: Span::new(start_span, tokens[close].span.end),
            };
            cursor = close + 1;
        }
    }

    let mut current_is_const = is_const;
    let mut has_pointer = false;
    while cursor < end {
        if !matches!(tokens[cursor].kind, Token::Star) {
            return None;
        }
        has_pointer = true;
        ty = ast::Type {
            kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                is_mutable: !current_is_const,
                is_volatile: type_volatile,
                inner: Box::new(ty),
            })),
            span: tokens[start].span.extend_to(&tokens[cursor].span),
        };
        current_is_const = false;
        cursor += 1;
    }

    // A bare identifier in parens is ambiguous between a cast type and
    // a parenthesized expression (e.g. `(a)` could be cast-to-type-a
    // or parenthesized-var-a). Only treat it as a cast type when it's
    // followed by `*` (pointer type, unambiguous) or when it's a
    // primitive/vector/optional keyword (unambiguous tokens).
    if !has_pointer && from_identifier {
        return None;
    }

    Some(ty)
}

/// Parse a single function-parameter type up to a top-level comma inside a
/// function-type cast target like `void(i64, bool)*`. `start`/`end` bound the
/// whole parenthesized parameter list; the returned `usize` is the index just
/// past the parsed parameter type (before any trailing comma).
fn parse_paren_fn_arg(tokens: &[LexToken], start: usize, end: usize) -> Option<(ast::Type, usize)> {
    let mut depth = 0usize;
    let mut i = start;
    while i < end {
        match tokens[i].kind {
            Token::Less | Token::LeftParen | Token::LeftBracket => depth += 1,
            Token::Greater | Token::RightParen | Token::RightBracket => {
                depth = depth.saturating_sub(1)
            }
            Token::Comma if depth == 0 => break,
            _ => {}
        }
        i += 1;
    }
    if i == start {
        return None;
    }
    let ty = parse_type_in_parens(tokens, start, i)?;
    Some((ty, i))
}

/// True when a token can appear inside a function-type cast target
/// (mirrors `PrdParser::is_type_like`).
fn is_type_like(token: &Token) -> bool {
    matches!(
        token,
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
            | Token::Identifier(_)
            | Token::Comma
            | Token::Star
            | Token::DoubleColon
            | Token::Less
            | Token::Greater
            | Token::LeftParen
            | Token::RightParen
            | Token::LeftBracket
            | Token::RightBracket
            | Token::IntLiteral(_)
            | Token::Const
            | Token::Mut
            | Token::Lifetime(_)
            | Token::BitwiseAnd
    )
}

/// Index of the matching closing paren for an opening paren at `start`.
fn find_matching_paren(tokens: &[LexToken], start: usize, end: usize) -> Option<usize> {
    if start >= end || !matches!(tokens[start].kind, Token::LeftParen) {
        return None;
    }
    let mut depth = 0usize;
    let mut i = start;
    while i < end {
        match tokens[i].kind {
            Token::LeftParen => depth += 1,
            Token::RightParen => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
        i += 1;
    }
    None
}

fn parse_simple_type_prefix(
    tokens: &[LexToken],
    start: usize,
    end: usize,
) -> Option<(ast::Type, usize)> {
    if start >= end {
        return None;
    }

    let mut cursor = start;
    let mut is_const = false;
    let mut type_volatile = false;
    loop {
        match tokens.get(cursor).map(|t| &t.kind) {
            Some(Token::Const) => {
                is_const = true;
                cursor += 1;
            }
            Some(Token::Volatile) => {
                type_volatile = true;
                cursor += 1;
            }
            _ => break,
        }
        if cursor >= end {
            return None;
        }
    }

    // Reference types: `&T` / `&mut T` (elided lifetime); nested recurse.
    if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::BitwiseAnd)) {
        cursor += 1;
        let is_mutable = if matches!(tokens.get(cursor).map(|t| &t.kind), Some(Token::Mut)) {
            cursor += 1;
            true
        } else {
            false
        };
        let (inner, next) = parse_simple_type_prefix(tokens, cursor, end)?;
        let ty = ast::Type {
            kind: Box::new(ast::TypeKind::Reference(ast::ReferenceType {
                is_mutable,
                lifetime: None,
                inner: Box::new(inner),
            })),
            span: tokens[start].span.extend_to(&tokens[next - 1].span),
        };
        return Some((ty, next));
    }

    let mut ty = {
        let mut path = Vec::new();
        let base_start = cursor;
        let first_name = match &tokens[cursor].kind {
            Token::Identifier(name) => Some(name.clone()),
            Token::Vec => Some("Vec".to_string()),
            Token::Optional => Some("Optional".to_string()),
            _ => None,
        };
        if let Some(name) = first_name {
            path.push(ast::Identifier {
                name,
                span: tokens[cursor].span,
            });
            cursor += 1;
            while cursor + 1 < end && matches!(tokens[cursor].kind, Token::DoubleColon) {
                cursor += 1;
                let seg = match &tokens[cursor].kind {
                    Token::Identifier(seg) => seg.clone(),
                    Token::Vec => "Vec".to_string(),
                    Token::Optional => "Optional".to_string(),
                    _ => return None,
                };
                path.push(ast::Identifier {
                    name: seg,
                    span: tokens[cursor].span,
                });
                cursor += 1;
            }
        }

        if !path.is_empty() {
            let mut generics = None;
            if cursor < end && matches!(tokens[cursor].kind, Token::Less) {
                let mut depth = 0usize;
                let mut close = cursor;
                while close < end {
                    match tokens[close].kind {
                        Token::Less => depth += 1,
                        Token::Greater => {
                            depth = depth.saturating_sub(1);
                            if depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                    close += 1;
                }
                if close >= end || !matches!(tokens[close].kind, Token::Greater) {
                    return None;
                }
                let mut args = Vec::new();
                let mut arg_cursor = cursor + 1;
                while arg_cursor < close {
                    let (arg, next_arg) = parse_simple_type_prefix(tokens, arg_cursor, close)?;
                    args.push(arg);
                    arg_cursor = next_arg;
                    if arg_cursor < close {
                        if !matches!(tokens[arg_cursor].kind, Token::Comma) {
                            return None;
                        }
                        arg_cursor += 1;
                    }
                }
                generics = Some(args);
                cursor = close + 1;
            }

            ast::Type {
                kind: Box::new(ast::TypeKind::Named(ast::NamedType { path, generics })),
                span: tokens[base_start].span.extend_to(&tokens[cursor - 1].span),
            }
        } else {
            let base = match tokens.get(cursor)?.kind {
                Token::I8 => ast::TypeKind::Primitive(ast::PrimitiveType::I8),
                Token::I16 => ast::TypeKind::Primitive(ast::PrimitiveType::I16),
                Token::I32 => ast::TypeKind::Primitive(ast::PrimitiveType::I32),
                Token::I64 => ast::TypeKind::Primitive(ast::PrimitiveType::I64),
                Token::I128 => ast::TypeKind::Primitive(ast::PrimitiveType::I128),
                Token::U8 => ast::TypeKind::Primitive(ast::PrimitiveType::U8),
                Token::U16 => ast::TypeKind::Primitive(ast::PrimitiveType::U16),
                Token::U32 => ast::TypeKind::Primitive(ast::PrimitiveType::U32),
                Token::U64 => ast::TypeKind::Primitive(ast::PrimitiveType::U64),
                Token::U128 => ast::TypeKind::Primitive(ast::PrimitiveType::U128),
                Token::F32 => ast::TypeKind::Primitive(ast::PrimitiveType::F32),
                Token::F64 => ast::TypeKind::Primitive(ast::PrimitiveType::F64),
                Token::F80 => ast::TypeKind::Primitive(ast::PrimitiveType::F80),
                Token::Bool => ast::TypeKind::Primitive(ast::PrimitiveType::Bool),
                Token::Str => ast::TypeKind::Primitive(ast::PrimitiveType::Str),
                Token::Char => ast::TypeKind::Primitive(ast::PrimitiveType::Char),
                Token::Void => ast::TypeKind::Primitive(ast::PrimitiveType::Void),
                _ => return None,
            };
            cursor += 1;
            ast::Type {
                kind: Box::new(base),
                span: tokens[cursor - 1].span,
            }
        }
    };

    while cursor < end && matches!(tokens[cursor].kind, Token::Star) {
        let is_mutable = !is_const;
        ty = ast::Type {
            kind: Box::new(ast::TypeKind::Pointer(ast::PointerType {
                is_mutable,
                is_volatile: type_volatile,
                inner: Box::new(ty),
            })),
            span: tokens[start].span.extend_to(&tokens[cursor].span),
        };
        cursor += 1;
    }

    Some((ty, cursor))
}

fn parse_named_type_expr_prefix(
    tokens: &[LexToken],
    start: usize,
    end: usize,
) -> Option<(ast::Type, usize)> {
    let (ty, next) = parse_simple_type_prefix(tokens, start, end)?;
    let ast::TypeKind::Named(named) = ty.kind.as_ref() else {
        return None;
    };
    if named.generics.is_some() || named.path.len() > 1 {
        Some((ty, next))
    } else {
        None
    }
}

fn find_matching_brace(tokens: &[LexToken], start: usize, end: usize) -> Option<usize> {
    if start >= end {
        return None;
    }
    if !matches!(tokens[start].kind, Token::LeftBrace) {
        return None;
    }
    let mut depth = 0usize;
    for (idx, token) in tokens.iter().enumerate().take(end).skip(start) {
        match token.kind {
            Token::LeftBrace => depth += 1,
            Token::RightBrace => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(idx);
                }
            }
            _ => {}
        }
    }
    None
}

fn parse_match_pattern(cursor: &mut ExprCursor<'_>) -> Result<ast::Pattern, ParseError> {
    let token = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
        message: "expected match pattern".to_string(),
        span: Span::default(),
    })?;
    match &token.kind {
        Token::IntLiteral(value) => {
            let span = token.span;
            cursor.bump();
            if matches!(
                cursor.current().map(|t| &t.kind),
                Some(Token::DotDot | Token::DotDotDot)
            ) {
                let op = cursor.current().map(|t| t.kind.clone()).unwrap();
                let inclusive = matches!(op, Token::DotDot);
                cursor.bump();
                let end_token = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                    message: "expected range end in match pattern".to_string(),
                    span,
                })?;
                let end_value = match &end_token.kind {
                    Token::IntLiteral(v) => *v,
                    _ => {
                        return Err(ParseError::InvalidSyntax {
                            message: "range patterns require integer literals".to_string(),
                            span: end_token.span,
                        });
                    }
                };
                let end_span = end_token.span;
                cursor.bump();
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Range {
                        start: ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(
                                *value,
                            ))),
                            span,
                        },
                        end: ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(
                                end_value,
                            ))),
                            span: end_span,
                        },
                        inclusive,
                    },
                    span: span.extend_to(&end_span),
                })
            } else {
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Literal(ast::Literal::Integer(*value)),
                    span,
                })
            }
        }
        Token::FloatLiteral(value) => {
            let span = token.span;
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Literal(ast::Literal::Float(*value)),
                span,
            })
        }
        Token::StringLiteral(value) => {
            let span = token.span;
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Literal(ast::Literal::String(value.clone())),
                span,
            })
        }
        Token::CharLiteral(value) => {
            let span = token.span;
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Literal(ast::Literal::Char(*value)),
                span,
            })
        }
        Token::True => {
            let span = token.span;
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Literal(ast::Literal::Bool(true)),
                span,
            })
        }
        Token::False => {
            let span = token.span;
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Literal(ast::Literal::Bool(false)),
                span,
            })
        }
        Token::Move => {
            // `move x` binding (ownership-extracting pattern).
            let span = token.span;
            cursor.bump();
            let Some(binding_token) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected identifier after 'move' in pattern".to_string(),
                    span,
                });
            };
            let Token::Identifier(binding_name) = &binding_token.kind else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected identifier after 'move' in pattern".to_string(),
                    span: binding_token.span,
                });
            };
            cursor.bump();
            Ok(ast::Pattern {
                kind: ast::PatternKind::Move(ast::Identifier {
                    name: binding_name.clone(),
                    span: binding_token.span,
                }),
                span: span.extend_to(&binding_token.span),
            })
        }
        Token::Identifier(name) => {
            let span = token.span;
            let name = name.clone();
            cursor.bump();
            if name == "_" {
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Wildcard,
                    span,
                })
            } else if name.starts_with(|c: char| c.is_uppercase())
                && matches!(cursor.current().map(|t| &t.kind), Some(Token::Dot))
            {
                // Enum type pattern: TypeName.Variant or TypeName.Variant(data)
                // (handles bare identifiers plus the dedicated type tokens
                // `Optional` / `Vec`, which lex as keywords).
                parse_enum_type_pattern(name, span, cursor)
            } else if name.starts_with(|c: char| c.is_uppercase()) {
                // Bare variant pattern: `Variant` or `Variant(data)`. The enum
                // type is inferred from the scrutinee during type checking; an
                // empty path marks this as infer-from-scrutinee.
                let data = if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftParen)) {
                    cursor.bump();
                    let mut data_patterns = vec![parse_match_pattern(cursor)?];
                    while matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                        cursor.bump();
                        data_patterns.push(parse_match_pattern(cursor)?);
                    }
                    if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ')' in enum variant pattern".to_string(),
                            span,
                        });
                    }
                    cursor.bump();
                    let data_pattern = if data_patterns.len() == 1 {
                        data_patterns.pop().unwrap()
                    } else {
                        ast::Pattern {
                            kind: ast::PatternKind::Tuple(data_patterns),
                            span: span.extend_to(&cursor.tokens[cursor.pos - 1].span),
                        }
                    };
                    Some(Box::new(data_pattern))
                } else {
                    None
                };
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Enum {
                        path: vec![],
                        variant: ast::Identifier {
                            name: name.clone(),
                            span,
                        },
                        data,
                    },
                    span: span
                        .with_end(cursor.current().map(|t| t.span.start).unwrap_or(span.start)),
                })
            } else {
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Identifier(ast::Identifier {
                        name: name.clone(),
                        span,
                    }),
                    span,
                })
            }
        }
        Token::Optional | Token::Vec => {
            let span = token.span;
            let name = match &token.kind {
                Token::Optional => "Optional".to_string(),
                Token::Vec => "Vec".to_string(),
                _ => unreachable!(),
            };
            cursor.bump();
            if matches!(cursor.current().map(|t| &t.kind), Some(Token::Dot)) {
                // Enum type pattern: Optional.Some(...) / Vec.Empty(...)
                parse_enum_type_pattern(name, span, cursor)
            } else {
                // A bare `Optional` / `Vec` type name used as a binding is
                // unusual; fall through to an identifier pattern.
                Ok(ast::Pattern {
                    kind: ast::PatternKind::Identifier(ast::Identifier {
                        name: name.clone(),
                        span,
                    }),
                    span,
                })
            }
        }
        _ => Err(ParseError::InvalidSyntax {
            message: "unsupported match pattern".to_string(),
            span: token.span,
        }),
    }
}

/// Parse an `EnumType.Variant` / `EnumType.Variant(data)` match pattern after
/// the enum type name has already been consumed. `cursor` sits on the `.`.
fn parse_enum_type_pattern(
    type_name: String,
    type_span: Span,
    cursor: &mut ExprCursor<'_>,
) -> Result<ast::Pattern, ParseError> {
    let path = vec![ast::Identifier {
        name: type_name,
        span: type_span,
    }];
    cursor.bump(); // consume dot
    let variant_token = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
        message: "expected variant name after '.' in enum pattern".to_string(),
        span: type_span,
    })?;
    let variant_name = match &variant_token.kind {
        Token::Identifier(v) => v.clone(),
        _ => {
            return Err(ParseError::InvalidSyntax {
                message: "expected variant name in enum pattern".to_string(),
                span: variant_token.span,
            });
        }
    };
    let variant_span = variant_token.span;
    cursor.bump();
    let data = if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftParen)) {
        cursor.bump();
        let mut data_patterns = vec![parse_match_pattern(cursor)?];
        // Multiple payload bindings: Variant(a, b, c)
        while matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
            cursor.bump();
            data_patterns.push(parse_match_pattern(cursor)?);
        }
        if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
            return Err(ParseError::InvalidSyntax {
                message: "expected ')' in enum variant pattern".to_string(),
                span: variant_span,
            });
        }
        cursor.bump();
        let data_pattern = if data_patterns.len() == 1 {
            data_patterns.pop().unwrap()
        } else {
            ast::Pattern {
                kind: ast::PatternKind::Tuple(data_patterns),
                span: type_span.extend_to(&variant_span),
            }
        };
        Some(Box::new(data_pattern))
    } else {
        None
    };
    Ok(ast::Pattern {
        kind: ast::PatternKind::Enum {
            path,
            variant: ast::Identifier {
                name: variant_name,
                span: variant_span,
            },
            data,
        },
        span: type_span.with_end(
            cursor
                .current()
                .map(|t| t.span.start)
                .unwrap_or(type_span.start),
        ),
    })
}

fn parse_primary(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let token = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
        message: "expected expression".to_string(),
        span: Span::default(),
    })?;

    let mut expr = match &token.kind {
        Token::Match => {
            let match_start = token.span.start;
            cursor.bump();
            let scrutinee = parse_assignment(cursor)?;
            let Some(lbrace) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '{' after match expression".to_string(),
                    span: scrutinee.span,
                });
            };
            if !matches!(lbrace.kind, Token::LeftBrace) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '{' after match expression".to_string(),
                    span: lbrace.span,
                });
            }
            cursor.bump();
            let mut arms = Vec::new();
            while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBrace)) {
                let pattern = parse_match_pattern(cursor)?;
                let guard = if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                    cursor.bump();
                    Some(parse_assignment(cursor)?)
                } else {
                    None
                };
                let Some(colon) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ':' after match pattern".to_string(),
                        span: pattern.span,
                    });
                };
                if !matches!(colon.kind, Token::Colon) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ':' after match pattern".to_string(),
                        span: colon.span,
                    });
                }
                cursor.bump();
                let body = if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftBrace)) {
                    let block_start = cursor.pos;
                    let Some(close) = find_matching_brace(cursor.tokens, block_start, cursor.end)
                    else {
                        return Err(ParseError::InvalidSyntax {
                            message: "unterminated match arm block".to_string(),
                            span: cursor.tokens[block_start].span,
                        });
                    };
                    cursor.pos = close + 1;
                    let block_span = cursor.tokens[block_start]
                        .span
                        .extend_to(&cursor.tokens[close].span);
                    let statements = if let Some(parse_block) = cursor.block_parser.as_mut() {
                        parse_block(cursor.tokens, block_start, close + 1)?.statements
                    } else {
                        Vec::new()
                    };
                    ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Block(ast::Block {
                            statements,
                            span: block_span,
                        })),
                        span: block_span,
                    }
                } else {
                    parse_assignment(cursor)?
                };
                let arm_span = pattern.span.extend_to(&body.span);
                arms.push(ast::MatchArm {
                    pattern,
                    guard,
                    body,
                    span: arm_span,
                });
                if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                    cursor.bump();
                }
            }
            let Some(rbrace) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated match expression".to_string(),
                    span: token.span.with_end(match_start),
                });
            };
            let span = token.span.with_end(rbrace.span.end);
            cursor.bump();
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Match {
                    expression: Box::new(scrutinee),
                    arms,
                }),
                span,
            });
        }
        Token::Identifier(name) => {
            if let Some((ty, next)) =
                parse_named_type_expr_prefix(cursor.tokens, cursor.pos, cursor.end)
            {
                cursor.pos = next;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::TypeName(ty.clone())),
                    span: ty.span,
                });
            }
            ast::Expression {
                kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                    name: name.clone(),
                    span: token.span,
                })),
                span: token.span,
            }
        }
        Token::Vec
        | Token::Optional
        | Token::I8
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
        | Token::Void => {
            if let Some((ty, next)) =
                parse_named_type_expr_prefix(cursor.tokens, cursor.pos, cursor.end)
            {
                cursor.pos = next;
                return Ok(ast::Expression {
                    kind: Box::new(ast::ExpressionKind::TypeName(ty.clone())),
                    span: ty.span,
                });
            }
            let name = token
                .kind
                .keyword_name()
                .unwrap_or_else(|| panic!("keyword token without keyword_name: {:?}", token.kind));
            ast::Expression {
                kind: Box::new(ast::ExpressionKind::Identifier(ast::Identifier {
                    name: name.to_string(),
                    span: token.span,
                })),
                span: token.span,
            }
        }
        Token::IntLiteral(value) => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Integer(*value))),
            span: token.span,
        },
        Token::FloatLiteral(value) => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Float(*value))),
            span: token.span,
        },
        Token::StringLiteral(value) => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::String(
                value.clone(),
            ))),
            span: token.span,
        },
        Token::CharLiteral(value) => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Char(*value))),
            span: token.span,
        },
        Token::True => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(true))),
            span: token.span,
        },
        Token::False => ast::Expression {
            kind: Box::new(ast::ExpressionKind::Literal(ast::Literal::Bool(false))),
            span: token.span,
        },
        Token::LeftParen => {
            let left_span = token.span;
            cursor.bump();
            let inner = parse_assignment(cursor)?;
            let Some(close) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ')'".to_string(),
                    span: left_span,
                });
            };
            if !matches!(close.kind, Token::RightParen) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ')'".to_string(),
                    span: close.span,
                });
            }
            let span = left_span.with_end(close.span.end);
            cursor.bump();
            return Ok(ast::Expression {
                kind: inner.kind,
                span,
            });
        }
        Token::Asm => {
            let asm_start = token.span.start;
            cursor.bump();
            let Some(open) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '(' after asm".to_string(),
                    span: token.span,
                });
            };
            if !matches!(open.kind, Token::LeftParen) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '(' after asm".to_string(),
                    span: open.span,
                });
            }
            cursor.bump();
            let mut code = String::new();
            let mut has_str = false;
            while let Some(tok) = cursor.current() {
                if let Token::StringLiteral(ref s) = tok.kind {
                    if has_str && !code.ends_with('\n') {
                        code.push('\n');
                    }
                    code.push_str(s);
                    has_str = true;
                    cursor.bump();
                } else {
                    break;
                }
            }
            if !has_str {
                return Err(ParseError::InvalidSyntax {
                    message: "asm expects a string literal".to_string(),
                    span: open.span,
                });
            }

            let mut inputs = Vec::new();
            let mut clobbers = Vec::new();
            // Optional input list: asm("code", [expr, expr, ...])
            if let Some(comma) = cursor.current()
                && matches!(comma.kind, Token::Comma)
            {
                cursor.bump();
                let Some(open_bracket) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected '[' after ',' in asm inputs".to_string(),
                        span: comma.span,
                    });
                };
                if !matches!(open_bracket.kind, Token::LeftBracket) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected '[' after ',' in asm inputs".to_string(),
                        span: open_bracket.span,
                    });
                }
                cursor.bump();
                // Parse comma-separated expression list
                while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBracket)) {
                    let expr = parse_assignment(cursor)?;
                    inputs.push(expr);
                    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                        cursor.bump();
                    } else {
                        break;
                    }
                }
                let Some(close_bracket) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated asm input list".to_string(),
                        span: token.span.with_end(asm_start),
                    });
                };
                if !matches!(close_bracket.kind, Token::RightBracket) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ']' after asm inputs".to_string(),
                        span: close_bracket.span,
                    });
                }
                cursor.bump();
            }

            // Optional clobber list: asm("code", [inputs], ["rbx", "rdx"])
            if let Some(comma) = cursor.current()
                && matches!(comma.kind, Token::Comma)
            {
                cursor.bump();
                let Some(open_bracket) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected '[' after ',' in asm clobbers".to_string(),
                        span: comma.span,
                    });
                };
                if !matches!(open_bracket.kind, Token::LeftBracket) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected '[' after ',' in asm clobbers".to_string(),
                        span: open_bracket.span,
                    });
                }
                cursor.bump();
                while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBracket)) {
                    let Some(clobber) = cursor.current() else {
                        break;
                    };
                    let Token::StringLiteral(name) = &clobber.kind else {
                        return Err(ParseError::InvalidSyntax {
                            message: "asm clobbers must be string literals".to_string(),
                            span: clobber.span,
                        });
                    };
                    clobbers.push(name.clone());
                    cursor.bump();
                    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                        cursor.bump();
                    } else {
                        break;
                    }
                }
                let Some(close_bracket) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated asm clobber list".to_string(),
                        span: token.span.with_end(asm_start),
                    });
                };
                if !matches!(close_bracket.kind, Token::RightBracket) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ']' after asm clobbers".to_string(),
                        span: close_bracket.span,
                    });
                }
                cursor.bump();
            }

            let Some(close) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ')' after asm string".to_string(),
                    span: open.span,
                });
            };
            if !matches!(close.kind, Token::RightParen) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ')' after asm string".to_string(),
                    span: close.span,
                });
            }
            let span = token.span.with_end(close.span.end);
            cursor.bump();
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Asm {
                    code: code.clone(),
                    inputs,
                    clobbers,
                }),
                span,
            });
        }
        Token::LeftBrace => {
            cursor.bump();
            let mut items = Vec::new();
            while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBrace)) {
                let Some(item_start) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated initializer".to_string(),
                        span: token.span,
                    });
                };
                match &item_start.kind {
                    Token::Dot => {
                        cursor.bump();

                        let Some(name_token) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected field name in initializer".to_string(),
                                span: item_start.span,
                            });
                        };
                        let Token::Identifier(field_name) = &name_token.kind else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected field name in initializer".to_string(),
                                span: name_token.span,
                            });
                        };
                        let field_ident = ast::Identifier {
                            name: field_name.clone(),
                            span: name_token.span,
                        };
                        cursor.bump();

                        let Some(assign) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer field".to_string(),
                                span: field_ident.span,
                            });
                        };
                        if !matches!(assign.kind, Token::Assign) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer field".to_string(),
                                span: assign.span,
                            });
                        }
                        cursor.bump();

                        let value = parse_assignment(cursor)?;
                        items.push(ast::InitializerItem::Field {
                            name: field_ident,
                            value,
                        });
                    }
                    Token::LeftBracket => {
                        cursor.bump();
                        let index = parse_assignment(cursor)?;
                        let Some(close) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in initializer index".to_string(),
                                span: item_start.span,
                            });
                        };
                        if !matches!(close.kind, Token::RightBracket) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in initializer index".to_string(),
                                span: close.span,
                            });
                        }
                        cursor.bump();

                        let Some(assign) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer index".to_string(),
                                span: index.span,
                            });
                        };
                        if !matches!(assign.kind, Token::Assign) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected '=' in initializer index".to_string(),
                                span: assign.span,
                            });
                        }
                        cursor.bump();

                        let value = parse_assignment(cursor)?;
                        items.push(ast::InitializerItem::Index { index, value });
                    }
                    _ => {
                        let value = parse_assignment(cursor)?;
                        items.push(ast::InitializerItem::Positional(value));
                    }
                }

                if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                    cursor.bump();
                } else {
                    break;
                }
            }

            let Some(close) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated initializer".to_string(),
                    span: token.span,
                });
            };
            if !matches!(close.kind, Token::RightBrace) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected '}' after initializer".to_string(),
                    span: close.span,
                });
            }
            let span = token.span.with_end(close.span.end);
            cursor.bump();
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Initializer { items }),
                span,
            });
        }
        Token::LeftBracket => {
            let bracket_start = token.span.start;
            cursor.bump();
            let mut elements = Vec::new();
            while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightBracket)) {
                let elem = parse_assignment(cursor)?;
                elements.push(elem);
                if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                    cursor.bump();
                } else {
                    break;
                }
            }
            let Some(close) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "unterminated array/tuple literal".to_string(),
                    span: token.span.with_end(bracket_start),
                });
            };
            if !matches!(close.kind, Token::RightBracket) {
                return Err(ParseError::InvalidSyntax {
                    message: "expected ']' after array/tuple literal".to_string(),
                    span: close.span,
                });
            }
            let span = token.span.with_end(close.span.end);
            cursor.bump();
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Array(elements)),
                span,
            });
        }
        Token::At => {
            let at_start = token.span.start;
            cursor.bump();
            let Some(name_token) = cursor.current() else {
                return Err(ParseError::InvalidSyntax {
                    message: "expected macro name after '@'".to_string(),
                    span: token.span.with_end(at_start),
                });
            };
            let name = match &name_token.kind {
                Token::Identifier(n) => n.clone(),
                _ => {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected identifier as macro name".to_string(),
                        span: name_token.span,
                    });
                }
            };
            let name_span = name_token.span;
            cursor.bump();
            let mut args = Vec::new();
            if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftParen)) {
                cursor.bump();
                while !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                    let arg = parse_assignment(cursor)?;
                    args.push(ast::MacroArg::Expression(arg));
                    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Comma)) {
                        cursor.bump();
                    } else {
                        break;
                    }
                }
                let Some(close) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "unterminated macro call".to_string(),
                        span: token.span.with_end(at_start),
                    });
                };
                if !matches!(close.kind, Token::RightParen) {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected ')' after macro arguments".to_string(),
                        span: close.span,
                    });
                }
                cursor.bump();
            }
            let span = token.span.with_end(
                cursor
                    .current()
                    .map(|t| t.span.end)
                    .unwrap_or(name_span.end),
            );
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::MacroCall {
                    name: ast::Identifier {
                        name,
                        span: name_span,
                    },
                    args,
                }),
                span,
            });
        }
        _ => {
            return Err(ParseError::InvalidSyntax {
                message: "unsupported primary expression in bootstrap parser".to_string(),
                span: token.span,
            });
        }
    };

    cursor.bump();
    // String literal concatenation: "A" "B" → "AB"
    if matches!(
        &*expr.kind,
        ast::ExpressionKind::Literal(ast::Literal::String(_))
    ) {
        while let Some(next) = cursor.current()
            && matches!(&next.kind, Token::StringLiteral(_))
        {
            if let Token::StringLiteral(part) = &next.kind {
                match &mut *expr.kind {
                    ast::ExpressionKind::Literal(ast::Literal::String(val)) => {
                        val.push_str(part);
                        expr.span.end = next.span.end;
                    }
                    _ => unreachable!(),
                }
            }
            cursor.bump();
        }
    }
    Ok(expr)
}

fn parse_postfix(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_primary(cursor)?;

    while let Some(token) = cursor.current() {
        match token.kind {
            Token::LeftParen => {
                let call_start = expr.span.start;
                cursor.bump();
                let mut arguments = Vec::new();
                if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                    loop {
                        let arg = parse_assignment(cursor)?;
                        arguments.push(arg);
                        match cursor.current().map(|t| &t.kind) {
                            Some(Token::Comma) => cursor.bump(),
                            Some(Token::RightParen) => break,
                            Some(_) => {
                                return Err(ParseError::InvalidSyntax {
                                    message: "expected ',' or ')' in call arguments".to_string(),
                                    span: cursor.current().unwrap().span,
                                });
                            }
                            None => {
                                return Err(ParseError::InvalidSyntax {
                                    message: "unterminated call expression".to_string(),
                                    span: expr.span.with_end(call_start),
                                });
                            }
                        }
                    }
                }

                let close = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                    message: "expected ')' in call expression".to_string(),
                    span: expr.span.with_end(call_start),
                })?;
                let call_span = expr.span.extend_to(&close.span);
                cursor.bump();

                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Call {
                        function: Box::new(expr),
                        arguments,
                    }),
                    span: call_span,
                };
            }
            Token::Dot => {
                cursor.bump();
                let Some(field_token) = cursor.current() else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected identifier after '.'".to_string(),
                        span: token.span,
                    });
                };
                let Token::Identifier(ref field_name) = field_token.kind else {
                    return Err(ParseError::InvalidSyntax {
                        message: "expected identifier after '.'".to_string(),
                        span: field_token.span,
                    });
                };
                let field_ident = ast::Identifier {
                    name: field_name.clone(),
                    span: field_token.span,
                };
                cursor.bump();

                if matches!(cursor.current().map(|t| &t.kind), Some(Token::LeftParen)) {
                    cursor.bump();
                    let mut arguments = Vec::new();
                    if !matches!(cursor.current().map(|t| &t.kind), Some(Token::RightParen)) {
                        loop {
                            arguments.push(parse_assignment(cursor)?);
                            match cursor.current().map(|t| &t.kind) {
                                Some(Token::Comma) => cursor.bump(),
                                Some(Token::RightParen) => break,
                                Some(_) => {
                                    return Err(ParseError::InvalidSyntax {
                                        message: "expected ',' or ')' in method arguments"
                                            .to_string(),
                                        span: cursor.current().unwrap().span,
                                    });
                                }
                                None => {
                                    return Err(ParseError::InvalidSyntax {
                                        message: "unterminated method call expression".to_string(),
                                        span: field_ident.span,
                                    });
                                }
                            }
                        }
                    }
                    let close = cursor.current().ok_or_else(|| ParseError::InvalidSyntax {
                        message: "expected ')' in method call".to_string(),
                        span: field_ident.span,
                    })?;
                    let span = expr.span.extend_to(&close.span);
                    cursor.bump();
                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::MethodCall {
                            receiver: Box::new(expr),
                            method: field_ident,
                            arguments,
                        }),
                        span,
                    };
                } else {
                    let span = expr.span.extend_to(&field_ident.span);
                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::FieldAccess {
                            object: Box::new(expr),
                            field: field_ident,
                        }),
                        span,
                    };
                }
            }
            Token::LeftBracket => {
                let open_span = token.span;
                cursor.bump();

                // Case 1: starts with Colon e.g. `[:end]`, `[:]`, `[::step]`, `[:end:step]`
                if matches!(cursor.current().map(|t| &t.kind), Some(Token::Colon)) {
                    cursor.bump(); // consume first ':'
                    let mut end = None;
                    let mut step = None;
                    if !matches!(
                        cursor.current().map(|t| &t.kind),
                        Some(Token::Colon) | Some(Token::RightBracket)
                    ) {
                        end = Some(Box::new(parse_assignment(cursor)?));
                    }
                    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Colon)) {
                        cursor.bump(); // consume second ':'
                        if !matches!(
                            cursor.current().map(|t| &t.kind),
                            Some(Token::RightBracket)
                        ) {
                            step = Some(Box::new(parse_assignment(cursor)?));
                        }
                    }
                    let Some(close) = cursor.current() else {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ']' in slice expression".to_string(),
                            span: open_span,
                        });
                    };
                    if !matches!(close.kind, Token::RightBracket) {
                        return Err(ParseError::InvalidSyntax {
                            message: "expected ']' in slice expression".to_string(),
                            span: close.span,
                        });
                    }
                    let span = expr.span.extend_to(&close.span);
                    cursor.bump();
                    expr = ast::Expression {
                        kind: Box::new(ast::ExpressionKind::Slice {
                            object: Box::new(expr),
                            start: None,
                            end,
                            step,
                        }),
                        span,
                    };
                } else {
                    // Case 2: starts with an expression `[start...]` or `[index]`
                    let first_expr = parse_assignment(cursor)?;
                    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Colon)) {
                        cursor.bump(); // consume first ':'
                        let mut end = None;
                        let mut step = None;
                        if !matches!(
                            cursor.current().map(|t| &t.kind),
                            Some(Token::Colon) | Some(Token::RightBracket)
                        ) {
                            end = Some(Box::new(parse_assignment(cursor)?));
                        }
                        if matches!(cursor.current().map(|t| &t.kind), Some(Token::Colon)) {
                            cursor.bump(); // consume second ':'
                            if !matches!(
                                cursor.current().map(|t| &t.kind),
                                Some(Token::RightBracket)
                            ) {
                                step = Some(Box::new(parse_assignment(cursor)?));
                            }
                        }
                        let Some(close) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in slice expression".to_string(),
                                span: open_span,
                            });
                        };
                        if !matches!(close.kind, Token::RightBracket) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in slice expression".to_string(),
                                span: close.span,
                            });
                        }
                        let span = expr.span.extend_to(&close.span);
                        cursor.bump();
                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Slice {
                                object: Box::new(expr),
                                start: Some(Box::new(first_expr)),
                                end,
                                step,
                            }),
                            span,
                        };
                    } else {
                        // Standard index expression
                        let Some(close) = cursor.current() else {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in index expression".to_string(),
                                span: open_span,
                            });
                        };
                        if !matches!(close.kind, Token::RightBracket) {
                            return Err(ParseError::InvalidSyntax {
                                message: "expected ']' in index expression".to_string(),
                                span: close.span,
                            });
                        }
                        let span = expr.span.extend_to(&close.span);
                        cursor.bump();
                        expr = ast::Expression {
                            kind: Box::new(ast::ExpressionKind::Index {
                                object: Box::new(expr),
                                index: Box::new(first_expr),
                            }),
                            span,
                        };
                    }
                }
            }
            Token::Increment => {
                let end_span = token.span.end;
                cursor.bump();
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Postfix {
                        operator: ast::UnaryOperator::Increment,
                        operand: Box::new(expr.clone()),
                    }),
                    span: expr.span.with_end(end_span),
                };
            }
            Token::Decrement => {
                let end_span = token.span.end;
                cursor.bump();
                expr = ast::Expression {
                    kind: Box::new(ast::ExpressionKind::Postfix {
                        operator: ast::UnaryOperator::Decrement,
                        operand: Box::new(expr.clone()),
                    }),
                    span: expr.span.with_end(end_span),
                };
            }
            _ => break,
        }
    }

    Ok(expr)
}

fn parse_unary(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let Some(token) = cursor.current() else {
        return Err(ParseError::InvalidSyntax {
            message: "expected expression".to_string(),
            span: Span::default(),
        });
    };

    let operator = match token.kind {
        Token::Minus => Some(ast::UnaryOperator::Minus),
        Token::Plus => Some(ast::UnaryOperator::Plus),
        Token::Star => Some(ast::UnaryOperator::Dereference),
        Token::Not => Some(ast::UnaryOperator::Not),
        Token::BitwiseNot => Some(ast::UnaryOperator::BitwiseNot),
        Token::Increment => Some(ast::UnaryOperator::Increment),
        Token::Decrement => Some(ast::UnaryOperator::Decrement),
        _ => None,
    };

    if let Some(operator) = operator {
        cursor.bump();
        let operand = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Unary {
                operator,
                operand: Box::new(operand.clone()),
            }),
            span: token.span.with_end(operand.span.end),
        });
    }

    if matches!(token.kind, Token::BitwiseAnd | Token::And) {
        cursor.bump();
        let is_mutable = if matches!(cursor.current().map(|t| &t.kind), Some(Token::Mut)) {
            cursor.bump();
            true
        } else {
            false
        };
        let inner = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Reference {
                is_mutable,
                expression: Box::new(inner.clone()),
            }),
            span: token.span.with_end(inner.span.end),
        });
    }

    if matches!(token.kind, Token::Move) {
        cursor.bump();
        let inner = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Move(Box::new(inner.clone()))),
            span: token.span.with_end(inner.span.end),
        });
    }

    if matches!(token.kind, Token::Launch) {
        cursor.bump();
        let call = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Launch(Box::new(call.clone()))),
            span: token.span.with_end(call.span.end),
        });
    }

    if matches!(token.kind, Token::Wait) {
        cursor.bump();
        let task = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Wait(Box::new(task.clone()))),
            span: token.span.with_end(task.span.end),
        });
    }

    if matches!(token.kind, Token::Comptime) {
        cursor.bump();
        let inner = parse_unary(cursor)?;
        return Ok(ast::Expression {
            kind: Box::new(ast::ExpressionKind::Comptime(Box::new(inner.clone()))),
            span: token.span.with_end(inner.span.end),
        });
    }

    if matches!(token.kind, Token::LeftParen) {
        // Find the matching closing paren (nesting-aware) so nested function
        // types like `(void(i64)*)expr` work; a naive first-`)` scan would
        // truncate at the inner `(i64)` group.
        let mut i = cursor.pos + 1;
        let mut depth = 1usize;
        let mut close_paren = None;
        while i < cursor.end {
            match cursor.tokens[i].kind {
                Token::LeftParen => depth += 1,
                Token::RightParen => {
                    depth -= 1;
                    if depth == 0 {
                        close_paren = Some(i);
                        break;
                    }
                }
                _ => {}
            }
            i += 1;
        }
        if let Some(end) = close_paren
            && end > cursor.pos + 1
            && let Some(target_type) = parse_type_in_parens(cursor.tokens, cursor.pos + 1, end)
        {
            cursor.pos = end + 1;
            let operand = parse_unary(cursor)?;
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Cast {
                    expression: Box::new(operand.clone()),
                    target_type: Box::new(target_type),
                }),
                span: token.span.with_end(operand.span.end),
            });
        }
    }

    parse_postfix(cursor)
}

fn parse_multiplicative(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_unary(cursor)?;
    while let Some(token) = cursor.current() {
        let operator = match token.kind {
            Token::Star => Some(ast::BinaryOperator::Multiply),
            Token::Slash => Some(ast::BinaryOperator::Divide),
            Token::Percent => Some(ast::BinaryOperator::Modulo),
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump();
        let rhs = parse_unary(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_additive(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_multiplicative(cursor)?;
    while let Some(token) = cursor.current() {
        let operator = match token.kind {
            Token::Plus => Some(ast::BinaryOperator::Add),
            Token::Minus => Some(ast::BinaryOperator::Subtract),
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump();
        let rhs = parse_multiplicative(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_shift(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_additive(cursor)?;
    while let Some(token) = cursor.current() {
        let operator = match token.kind {
            Token::Less
                if cursor.pos + 1 < cursor.end
                    && matches!(cursor.tokens[cursor.pos + 1].kind, Token::Less) =>
            {
                cursor.bump(); // consume first <
                Some(ast::BinaryOperator::LeftShift)
            }
            Token::Greater
                if cursor.pos + 1 < cursor.end
                    && matches!(cursor.tokens[cursor.pos + 1].kind, Token::Greater) =>
            {
                cursor.bump(); // consume first >
                Some(ast::BinaryOperator::RightShift)
            }
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump(); // consume second < or >
        let rhs = parse_additive(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_relational(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_shift(cursor)?;
    while let Some(token) = cursor.current() {
        let operator = match token.kind {
            Token::Less => Some(ast::BinaryOperator::Less),
            Token::Greater => Some(ast::BinaryOperator::Greater),
            Token::LessEqual => Some(ast::BinaryOperator::LessEqual),
            Token::GreaterEqual => Some(ast::BinaryOperator::GreaterEqual),
            Token::DotDot => Some(ast::BinaryOperator::Range),
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump();
        let rhs = if matches!(operator, ast::BinaryOperator::Range) {
            parse_shift(cursor)?
        } else {
            parse_additive(cursor)?
        };
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_equality(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_relational(cursor)?;
    while let Some(token) = cursor.current() {
        let operator = match token.kind {
            Token::Equal => Some(ast::BinaryOperator::Equal),
            Token::NotEqual => Some(ast::BinaryOperator::NotEqual),
            _ => None,
        };
        let Some(operator) = operator else { break };
        cursor.bump();
        let rhs = parse_relational(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_bitwise_and(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_equality(cursor)?;
    while let Some(token) = cursor.current() {
        if !matches!(token.kind, Token::BitwiseAnd) {
            break;
        }
        cursor.bump();
        let rhs = parse_equality(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator: ast::BinaryOperator::BitwiseAnd,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_bitwise_xor(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_bitwise_and(cursor)?;
    while let Some(token) = cursor.current() {
        if !matches!(token.kind, Token::BitwiseXor) {
            break;
        }
        cursor.bump();
        let rhs = parse_bitwise_and(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator: ast::BinaryOperator::BitwiseXor,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_bitwise_or(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_bitwise_xor(cursor)?;
    while let Some(token) = cursor.current() {
        if !matches!(token.kind, Token::BitwiseOr) {
            break;
        }
        cursor.bump();
        let rhs = parse_bitwise_xor(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator: ast::BinaryOperator::BitwiseOr,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_logical_and(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_bitwise_or(cursor)?;
    while let Some(token) = cursor.current() {
        if !matches!(token.kind, Token::And) {
            break;
        }
        cursor.bump();
        let rhs = parse_bitwise_or(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator: ast::BinaryOperator::LogicalAnd,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_logical_or(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let mut expr = parse_logical_and(cursor)?;
    while let Some(token) = cursor.current() {
        if !matches!(token.kind, Token::Or) {
            break;
        }
        cursor.bump();
        let rhs = parse_logical_and(cursor)?;
        let span = expr.span.extend_to(&rhs.span);
        expr = ast::Expression {
            kind: Box::new(ast::ExpressionKind::Binary {
                left: Box::new(expr),
                operator: ast::BinaryOperator::LogicalOr,
                right: Box::new(rhs),
            }),
            span,
        };
    }
    Ok(expr)
}

fn parse_assignment(cursor: &mut ExprCursor<'_>) -> Result<ast::Expression, ParseError> {
    let lhs = parse_logical_or(cursor)?;

    // Ternary / Unwrap-or:
    //   - `cond ? then : else` (Ternary: value-producing branch)
    //   - `value ? fallback`   (Unwrap-or: unwrap Optional, Result, or pointer)
    if matches!(cursor.current().map(|t| &t.kind), Some(Token::Question)) {
        cursor.bump();
        let then_or_fallback = parse_assignment(cursor)?;
        if matches!(cursor.current().map(|t| &t.kind), Some(Token::Colon)) {
            cursor.bump();
            let else_expr = parse_assignment(cursor)?;
            let span = lhs.span.extend_to(&else_expr.span);
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::Ternary {
                    condition: Box::new(lhs),
                    then_expr: Box::new(then_or_fallback),
                    else_expr: Box::new(else_expr),
                }),
                span,
            });
        } else {
            let span = lhs.span.extend_to(&then_or_fallback.span);
            return Ok(ast::Expression {
                kind: Box::new(ast::ExpressionKind::UnwrapOr {
                    value: Box::new(lhs),
                    fallback: Box::new(then_or_fallback),
                }),
                span,
            });
        }
    }

    let Some(token) = cursor.current() else {
        return Ok(lhs);
    };

    let operator = match token.kind {
        Token::Assign => Some(ast::BinaryOperator::Assign),
        Token::PlusAssign => Some(ast::BinaryOperator::AddAssign),
        Token::MinusAssign => Some(ast::BinaryOperator::SubtractAssign),
        Token::StarAssign => Some(ast::BinaryOperator::MultiplyAssign),
        Token::SlashAssign => Some(ast::BinaryOperator::DivideAssign),
        Token::PercentAssign => Some(ast::BinaryOperator::ModuloAssign),
        _ => None,
    };

    let Some(operator) = operator else {
        return Ok(lhs);
    };

    cursor.bump();
    let rhs = parse_assignment(cursor)?;
    let span = lhs.span.extend_to(&rhs.span);
    Ok(ast::Expression {
        kind: Box::new(ast::ExpressionKind::Binary {
            left: Box::new(lhs),
            operator,
            right: Box::new(rhs),
        }),
        span,
    })
}

/// Parse the token range `[start, end)` as an expression.
///
/// `block_parser` lets the caller (PRT parser) supply a function that parses
/// a `{ ... }` block of statements, which the expression parser needs when a
/// match arm body is a block.  Pass `None` for legacy callers.
pub(crate) fn parse_expression(
    tokens: &[LexToken],
    start: usize,
    end: usize,
    block_parser: BlockParser<'_>,
) -> Result<ast::Expression, ParseError> {
    let mut cursor = ExprCursor {
        tokens,
        pos: start,
        end,
        block_parser,
    };
    let expr = parse_assignment(&mut cursor)?;
    if cursor.pos != end {
        let span = cursor
            .current()
            .map(|t| t.span)
            .unwrap_or_else(|| expr.span);
        return Err(ParseError::InvalidSyntax {
            message: "unsupported expression form in bootstrap parser".to_string(),
            span,
        });
    }
    Ok(expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::prt_parser::PRT_Parser;

    #[test]
    fn parses_function_pointer_type_and_cast() {
        // The fn-pointer type `void(i64)` in a cast `(void(i64))worker` must
        // parse as a Function type — including the cast of a plain identifier
        // (no inner parens), a nested-args case, and a trailing `*` (pointer
        // to fn-pointer).
        let source = r#"
            void worker(i64 x) { }
            i64 main() {
                void(i64) fn = (void(i64))worker;
                void(i64, i64) fn2 = (void(i64, i64))worker;
                void(i64)* fn3 = (void(i64)*)fn;
                fn(7);
                return 0;
            }
        "#;
        let tokens = lex(source).expect("lex failed");
        let mut parser = PRT_Parser::new(None);
        let program = parser.parse_program(&tokens).expect("parse failed");
        assert_eq!(program.items.len(), 2);

        let ast::ItemKind::Function(main) = &program.items[1].kind else {
            panic!("expected function item");
        };
        let stmts = &main.body.statements;
        assert_eq!(stmts.len(), 5);

        // First let: declared type is Function([I64] -> Void).
        let ast::StatementKind::Let(first) = &stmts[0].kind else {
            panic!("expected let statement");
        };
        let declared = first.type_annotation.as_ref().expect("type annotation");
        let ast::TypeKind::Function(fnty) = &*declared.kind else {
            panic!("expected function type annotation");
        };
        assert_eq!(fnty.parameters.len(), 1);
        assert!(matches!(
            &*fnty.parameters[0].kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::I64)
        ));
        assert!(matches!(
            &*fnty.return_type.kind,
            ast::TypeKind::Primitive(ast::PrimitiveType::Void)
        ));

        // The initializer of the first let is the cast; its target type must
        // be the same Function type.
        let init = first.initializer.as_ref().expect("initializer");
        let ast::ExpressionKind::Cast { target_type, .. } = &*init.kind else {
            panic!("expected cast initializer");
        };
        assert!(matches!(&*target_type.kind, ast::TypeKind::Function(_)));

        // Second let: multi-argument function type via the comma-aware split.
        let ast::StatementKind::Let(second) = &stmts[1].kind else {
            panic!("expected let statement");
        };
        let declared2 = second.type_annotation.as_ref().expect("type annotation");
        let ast::TypeKind::Function(fnty2) = &*declared2.kind else {
            panic!("expected function type");
        };
        assert_eq!(fnty2.parameters.len(), 2);

        // Third let: `void(i64)*` is a pointer to the function type — the `*`
        // loop must still apply after the paren group.
        let ast::StatementKind::Let(third) = &stmts[2].kind else {
            panic!("expected let statement");
        };
        let declared3 = third.type_annotation.as_ref().expect("type annotation");
        assert!(matches!(&*declared3.kind, ast::TypeKind::Pointer(_)));
        let ast::TypeKind::Pointer(ptr) = &*declared3.kind else {
            panic!("expected pointer type");
        };
        assert!(matches!(&*ptr.inner.kind, ast::TypeKind::Function(_)));
    }

    #[test]
    fn parses_launch_and_wait_expressions() {
        // `launch f(args...)` wraps the call; `wait t` wraps its Task
        // operand. Both are prefix expressions binding tighter than the
        // surrounding statement.
        let source = r#"
            i64 work(i64 x) { return x + 1; }
            i64 main() {
                Task<i64> t = launch work(40);
                defer wait t;
                return 0;
            }
        "#;
        let tokens = lex(source).expect("lex failed");
        let mut parser = PRT_Parser::new(None);
        let program = parser.parse_program(&tokens).expect("parse failed");
        assert_eq!(program.items.len(), 2);

        let ast::ItemKind::Function(main) = &program.items[1].kind else {
            panic!("expected function item");
        };
        let stmts = &main.body.statements;
        assert_eq!(stmts.len(), 3);

        // `Task<i64> t = launch work(40);` — the initializer is Launch
        // wrapping a Call whose callee is the bare identifier `work`.
        let ast::StatementKind::Let(first) = &stmts[0].kind else {
            panic!("expected let statement");
        };
        let declared = first.type_annotation.as_ref().expect("type annotation");
        let ast::TypeKind::Named(named) = &*declared.kind else {
            panic!("expected named type annotation");
        };
        assert_eq!(named.path.len(), 1);
        assert_eq!(named.path[0].name, "Task");
        assert_eq!(named.generics.as_ref().expect("generics").len(), 1);

        let init = first.initializer.as_ref().expect("initializer");
        let ast::ExpressionKind::Launch(call) = &*init.kind else {
            panic!("expected launch initializer");
        };
        let ast::ExpressionKind::Call {
            function,
            arguments,
        } = &*call.kind
        else {
            panic!("expected call inside launch");
        };
        let ast::ExpressionKind::Identifier(callee) = &*function.kind else {
            panic!("expected identifier callee");
        };
        assert_eq!(callee.name, "work");
        assert_eq!(arguments.len(), 1);

        // `defer wait t;` — the deferred statement's expression is Wait.
        let ast::StatementKind::Defer(deferred) = &stmts[1].kind else {
            panic!("expected defer statement");
        };
        let ast::StatementKind::Expression(wait_expr) = &deferred.kind else {
            panic!("expected expression statement inside defer");
        };
        let ast::ExpressionKind::Wait(task) = &*wait_expr.kind else {
            panic!("expected wait expression");
        };
        let ast::ExpressionKind::Identifier(task_id) = &*task.kind else {
            panic!("expected task identifier");
        };
        assert_eq!(task_id.name, "t");
    }
}
