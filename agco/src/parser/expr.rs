use crate::ast::Expr;
use crate::lexer::Token;
use crate::parser::ty::type_parser;
use chumsky::prelude::*;

#[derive(Clone)]
enum PostfixOp {
    Call(Vec<Expr>),
    Index(Box<Expr>),
    Member(String, bool), // name, is_ptr
    PostInc,
    PostDec,
    Generics(Vec<crate::ast::TypeName>),
}

pub fn expr_parser<'a>() -> impl Parser<'a, &'a [Token], Expr, extra::Err<Simple<'a, Token>>> + Clone
{
    recursive(|expr| {
        // InitList: { item, item }
        // item: ( [designator] = )? value
        let init_item = (just(Token::LBracket)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::RBracket))
            .then_ignore(just(Token::Assign))
            .map(|e| Some(Box::new(e))))
        .or_not()
        .then(expr.clone())
        .map(|(designator, value)| crate::ast::InitItem {
            designator: designator.flatten(),
            value: Box::new(value),
        });

        let init_list = init_item
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Expr::InitList);

        let val = select! {
            Token::Integer(i) => Expr::Int(i as u64),
            Token::Identifier(s) => Expr::Ident(s),
            Token::KwTrue => Expr::Bool(true),
            Token::KwFalse => Expr::Bool(false),
            Token::String(s) => Expr::Str(s),
            Token::CharLiteral(c) => Expr::Char(c),
            Token::Float(f) => Expr::Float(f),
        };

        // Parenthesized expression
        let atom = val
            .or(type_parser().map(|ty| {
                let primitives = [
                    "void", "bool", "i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32",
                    "f64", "char", "str",
                ];
                if ty.pointer_depth == 0
                    && ty.array_dims.is_empty()
                    && ty.generic_args.is_empty()
                    && !primitives.contains(&ty.name.as_str())
                {
                    Expr::Ident(ty.name)
                } else {
                    Expr::Type(ty)
                }
            }))
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .or(init_list);

        let postfix = atom.clone().foldl(
            choice((
                // Call: (args)
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen))
                    .map(PostfixOp::Call),
                // Index: [index]
                expr.clone()
                    .delimited_by(just(Token::LBracket), just(Token::RBracket))
                    .map(|idx| PostfixOp::Index(Box::new(idx))),
                // Member: .ident
                just(Token::Dot)
                    .ignore_then(select! {
                        Token::Identifier(s) => s,
                        Token::KwNew => "new".to_string()
                    })
                    .map(|s| PostfixOp::Member(s, false)),
                // Arrow: -> ident
                just(Token::Arrow)
                    .ignore_then(select! { Token::Identifier(s) => s })
                    .map(|s| PostfixOp::Member(s, true)),
                // PostInc: ++
                just(Token::PlusPlus).to(PostfixOp::PostInc),
                // PostDec: --
                just(Token::MinusMinus).to(PostfixOp::PostDec),
                // Generics: < T, U >
                // NOTE: This can be ambiguous with LessThan.
                // We rely on the parser failing if it's not a valid type list ended by >.
                // Ideally we need backtracking here if it fails.
                just(Token::Lt)
                    .ignore_then(
                        type_parser()
                            .separated_by(just(Token::Comma))
                            .collect::<Vec<_>>(),
                    )
                    .then_ignore(just(Token::Gt))
                    .map(PostfixOp::Generics),
            ))
            .repeated(),
            |base, op| match op {
                PostfixOp::Call(args) => {
                    if let Expr::Ident(callee) = base {
                        Expr::Call {
                            callee,
                            args,
                            generic_args: vec![],
                        }
                    } else if let Expr::Member { base, member, ptr } = base {
                        Expr::MethodCall {
                            base,
                            method: member,
                            args,
                            ptr,
                        }
                    } else if let Expr::Type(ty) = base {
                        // Static method call: Type.method()
                        // Wait, Expr::Type doesn't have method.
                        // But Member on Type creates Expr::Member?
                        // Then MethodCall on Member works?
                        // Yes, MethodCall takes base expression.
                        panic!("Invalid callee: {:?} - expected Identifier or Member", ty)
                    } else {
                        // Check if base is member access on type
                        panic!("Invalid callee: {:?} - expected Identifier or Member", base)
                    }
                }
                PostfixOp::Index(index) => Expr::Index {
                    base: Box::new(base),
                    index,
                },
                PostfixOp::Member(member, ptr) => Expr::Member {
                    base: Box::new(base),
                    member,
                    ptr,
                },
                PostfixOp::PostInc => Expr::Unary {
                    op: Token::PlusPlus,
                    rhs: Box::new(base),
                },
                PostfixOp::PostDec => Expr::Unary {
                    op: Token::MinusMinus,
                    rhs: Box::new(base),
                },
                PostfixOp::Generics(args) => {
                    if let Expr::Ident(name) = base {
                        // Convert Ident to Type
                        Expr::Type(crate::ast::TypeName {
                            name,
                            pointer_depth: 0,
                            array_dims: vec![],
                            generic_args: args,
                        })
                    } else {
                        // Error: Generics on non-ident?
                        panic!("Generics applied to invalid base: {:?}", base)
                    }
                }
            },
        );

        // Comptime: comptime expr
        let comptime = just(Token::KwComptime)
            .ignore_then(expr.clone())
            .map(|e| Expr::Comptime(Box::new(e)));

        // Cast: (Type) expr
        // To avoid ambiguity with (Expr), we rely on backtracking or structure.
        // We'll place it in `unary` level as a prefix.
        // But (Type) starts with `(`.

        let cast = just(Token::LParen)
            .ignore_then(type_parser())
            .then_ignore(just(Token::RParen))
            .then(expr.clone()) // Parse the transformed unary (handle right associativity or just next unary)
            .map(|(target, expr)| Expr::Cast {
                target,
                expr: Box::new(expr),
            });

        // Prefix: -x, !x, *x, &x, (Type)x, comptime x (unary priority?)
        let unary_op = choice((
            just(Token::Minus)
                .to(Token::Minus)
                .map(|op| (op, false, false)), // op, is_deref, is_addr
            just(Token::Bang)
                .to(Token::Bang)
                .map(|op| (op, false, false)),
            just(Token::Tilde)
                .to(Token::Tilde)
                .map(|op| (op, false, false)),
            just(Token::Star)
                .to(Token::Star)
                .map(|op| (op, true, false)), // Deref
            just(Token::Amp).to(Token::Amp).map(|op| (op, false, true)), // AddressOf
        ))
        .then(postfix.clone())
        .map(|((op, is_deref, is_addr), rhs)| {
            if is_deref {
                Expr::Deref(Box::new(rhs))
            } else if is_addr {
                Expr::AddressOf(Box::new(rhs))
            } else {
                Expr::Unary {
                    op,
                    rhs: Box::new(rhs),
                }
            }
        });

        let unary = choice((
            comptime, cast, // Try cast. If it fails (e.g. (Expr)), it backtracks?
            // NOTE: (Type) vs (Expr).
            // If input is (x) where x is ident.
            // cast parser: matches (, matches Ty(x), matches ). Then expects unary.
            // unary matches... end of expr?
            // If (x) is followed by +, cast expects unary.
            // If cast consumes (x), remaining is +. + is not unary.
            // So cast fails.
            // Backtracks to unary_op? No.
            // Backtracks to postfix?
            // postfix -> atom -> (expr).
            // (x) matches (expr)!
            // But we need `cast` to fail if it's not a cast.
            // This relies on `type_parser` only matching actual types or `cast` logic checking follow-up?
            // Ambiguity is real for `(ident) ...`.
            // For now, let's try assuming `cast` has priority and standard backtracking handles it.
            unary_op, postfix,
        ));

        // Product: * / %
        let product = unary.clone().foldl(
            choice((
                just(Token::Star).to(Token::Star),
                just(Token::Slash).to(Token::Slash),
                just(Token::Percent).to(Token::Percent),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        // Sum: + -
        let sum = product.clone().foldl(
            choice((
                just(Token::Plus).to(Token::Plus),
                just(Token::Minus).to(Token::Minus),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        // Comparison: < > <= >=
        let comparison = sum.clone().foldl(
            choice((
                just(Token::Lt).to(Token::Lt),
                just(Token::Gt).to(Token::Gt),
                just(Token::Le).to(Token::Le),
                just(Token::Ge).to(Token::Ge),
            ))
            .then(sum)
            .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        // Equalit: == !=
        let equality = comparison.clone().foldl(
            choice((just(Token::Eq).to(Token::Eq), just(Token::Ne).to(Token::Ne)))
                .then(comparison)
                .repeated(),
            |lhs, (op, rhs)| Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        );

        // Assignment: = += -= *= /= %=
        // Right-associative? a = b = c -> a = (b = c)
        // Recursive foldr? Or just `then(assign)`?
        // assignment = equality then (op then assignment)?
        // type: equality -> Expr.
        // If we use recursion: equality.then(op.then(expr).or_not())?
        // recursive(|assign| { equality.then(op.then(assign).or_not()) ... })
        // But `expr` is already recursive.
        // Let's use `equality` as base.

        let assignment = equality
            .clone()
            .then(
                choice((
                    just(Token::Assign).to(Token::Assign),
                    just(Token::PlusAssign).to(Token::PlusAssign),
                    just(Token::MinusAssign).to(Token::MinusAssign),
                    just(Token::StarAssign).to(Token::StarAssign),
                    just(Token::SlashAssign).to(Token::SlashAssign),
                    just(Token::PercentAssign).to(Token::PercentAssign),
                ))
                .then(expr) // Recursive call to top-level expr
                .or_not(),
            )
            .map(|(lhs, rhs)| {
                if let Some((op, rhs)) = rhs {
                    Expr::Assign {
                        op,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    }
                } else {
                    lhs
                }
            });

        assignment
    })
}
