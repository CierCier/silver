use crate::ast::{Declarator, Stmt, StmtDecl};
use crate::lexer::Token;
use crate::parser::expr::expr_parser;
use crate::parser::ty::type_parser;
use chumsky::prelude::*;

pub fn stmt_parser<'a>() -> impl Parser<'a, &'a [Token], Stmt, extra::Err<Simple<'a, Token>>> + Clone
{
    recursive(|stmt| {
        let block = stmt
            .clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map(Stmt::Block);

        let expr_stmt = expr_parser()
            .then_ignore(just(Token::Semicolon))
            .map(|e| Stmt::Expr(Box::new(e)));

        let return_stmt = just(Token::KwReturn)
            .ignore_then(expr_parser().or_not())
            .then_ignore(just(Token::Semicolon))
            .map(|e| Stmt::Return(e.map(Box::new)));

        let break_stmt = just(Token::KwBreak)
            .then_ignore(just(Token::Semicolon))
            .to(Stmt::Break);

        let continue_stmt = just(Token::KwContinue)
            .then_ignore(just(Token::Semicolon))
            .to(Stmt::Continue);

        // Var Decl: Type ident (= expr)?;
        // StmtDecl { ty, declarators: vec![Declarator { name, init }] }
        // C-style: int x = 5, y = 6;
        let decl_stmt = type_parser()
            .then(
                select! { Token::Identifier(name) => name }
                    .then(
                        just(Token::LBracket)
                            .ignore_then(select! { Token::Integer(i) => i as u64 }.or_not())
                            .then_ignore(just(Token::RBracket))
                            .repeated()
                            .collect::<Vec<_>>(),
                    )
                    .then(just(Token::Assign).ignore_then(expr_parser()).or_not())
                    .map(|((name, array_dims), init)| Declarator {
                        name,
                        array_dims,
                        init: init.map(Box::new),
                    })
                    .separated_by(just(Token::Comma))
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Semicolon))
            .map(|(ty, declarators)| {
                Stmt::Decl(StmtDecl {
                    ty,
                    declarators,
                    is_const: false, // TODO: handle const
                })
            });

        // If statement
        let if_stmt = just(Token::KwIf)
            .ignore_then(expr_parser()) // Condition
            .then(stmt.clone()) // Then branch (single stmt, likely a block)
            .then(just(Token::KwElse).ignore_then(stmt.clone()).or_not()) // Else branch
            .map(|((cond, then_branch), else_branch)| Stmt::If {
                cond: Box::new(cond),
                then_branch: Box::new(then_branch),
                else_branch: else_branch.map(Box::new),
            });

        // While statement
        let while_stmt = just(Token::KwWhile)
            .ignore_then(expr_parser())
            .then(stmt.clone())
            .map(|(cond, body)| Stmt::While {
                cond: Box::new(cond),
                body: Box::new(body),
            });

        // For statement
        let for_stmt = just(Token::KwFor)
            .ignore_then(just(Token::LParen))
            .ignore_then(choice((
                decl_stmt.clone().map(|s| Some(Box::new(s))),
                expr_stmt.clone().map(|s| Some(Box::new(s))),
                just(Token::Semicolon).to(None),
            )))
            .then(expr_parser().or_not().then_ignore(just(Token::Semicolon))) // Cond
            .then(expr_parser().or_not()) // Iter
            .then_ignore(just(Token::RParen))
            .then(stmt.clone())
            .map(|(((init, cond), iter), body)| Stmt::For {
                init,
                cond: cond.map(Box::new),
                iter: iter.map(Box::new),
                body: Box::new(body),
            });

        choice((
            block,
            return_stmt,
            break_stmt,
            continue_stmt,
            if_stmt,
            while_stmt,
            for_stmt,
            decl_stmt, // Try decl first? Ambiguity with expr stmt?
            // Types start with keywords or identifiers. Exprs start with identifiers...
            // If we check decl first, it consumes Type.
            // Need to be careful with ambiguity.
            // For now, assume Decl is distinguishable or prioritized.
            // Actually, `type_parser` might match an identifier which could be start of expression.
            // But `expr_parser` also matches identifier.
            // This is the classic "Statement vs Declaration" ambiguity.
            // C solves this by requiring types to be known or using syntax.
            // Silver: `int x` (primitive type). `MyStruct x` (Ident type).
            // `x = 5` (Expr).
            // `x * y` (Expr).
            // `x * y;` -> ExprStmt.
            // `x * y;` -> `type_parser` consumes `x`, expects `*` (pointer? no, pointer is `*T`).
            // `type_parser` for `x` (Ident) succeeds. Then expects identifier (declarator name).
            // If input is `x * y;`: `ty` parses `x`. Next token is `*`. `Identifier` expected. Fail.
            // So `decl_stmt` fails. Backtracks. `expr_stmt` tries. `x * y` parsed as expr. Succeeds.
            expr_stmt,
        ))
    })
}
