use crate::ast::{Decl, Param, StructField};
use crate::lexer::Token;
use crate::parser::stmt::stmt_parser;
use crate::parser::ty::type_parser;
use chumsky::prelude::*;

pub fn decl_parser<'a>() -> impl Parser<'a, &'a [Token], Decl, extra::Err<Simple<'a, Token>>> + Clone
{
    let type_parser = type_parser();
    let stmt_parser = stmt_parser();

    let import_decl = just(Token::KwImport)
        .ignore_then(
            select! { Token::Identifier(s) => s }
                .separated_by(just(Token::Dot))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Semicolon))
        .map(|path| Decl::Import { path });

    // Func: fn ident(params) -> ret block
    // Silver syntax: type ident(params) block
    // e.g. void main() { ... }
    // Or rust-style?
    // Looking at examples/optionals.ag: `struct Rect { ... }`, `int main() { ... }`
    // So C-style type-first signatures.

    let generics = just(Token::Lt)
        .ignore_then(
            select! { Token::Identifier(s) => s }
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Gt))
        .or_not()
        .map(|g| g.unwrap_or_default());

    let param = type_parser
        .clone()
        .then(select! { Token::Identifier(s) => s })
        .map(|(ty, name)| Param { ty, name });

    let func_decl = type_parser
        .clone()
        .then(select! {
            Token::Identifier(s) => s,
            Token::KwNew => "new".to_string()
        })
        .then(generics.clone()) // Function generics
        .then(
            param
                .clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(
            stmt_parser
                .clone()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(Some)
                .or(just(Token::Semicolon).to(None)), // Extern/Prototype
        )
        .map(|((((ret, name), generic_params), params), body)| {
            let is_extern = body.is_none();
            Decl::Func {
                ret,
                name,
                params,
                body,
                is_extern,
                is_variadic: false,
                generic_params,
            }
        });

    let struct_decl = just(Token::KwStruct)
        .ignore_then(select! { Token::Identifier(s) => s })
        .then(generics.clone()) // Struct generics
        .then(
            type_parser
                .clone()
                .then(
                    select! { Token::Identifier(s) => s }
                        .separated_by(just(Token::Comma))
                        .collect::<Vec<_>>(),
                )
                .then_ignore(just(Token::Semicolon))
                .map(|(ty, names)| StructField { ty, names })
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|((name, generic_params), fields)| Decl::Struct {
            name,
            fields,
            generic_params,
            attributes: vec![],
        });

    // Impl: impl T { methods... }
    // Or impl<T> Struct<T> { ... } ?
    // Example: impl Dim<T> { ... }
    // So: impl Type { ... }
    // But `type_parser` parses `Dim<T>`.
    // Inside impl block: functions (methods).
    // Note: methods look like functions but inside impl.
    // We can reuse func_decl but wrapped in Impl.
    // Wait, Decl::Func is used for methods too?
    // AST has `Decl::Impl { ty, methods }`.
    // Methods are `Decl`s.

    // We need recursive `decl_parser` for methods inside impl?
    // Or just `func_decl`?
    // `decl_parser` includes `impl_decl`. `impl` inside `impl` is usually not allowed.
    // But `struct_decl` inside `impl`?
    // Let's assume only functions are allowed in impl for now.

    let cast_decl = just(Token::KwCast)
        .ignore_then(type_parser.clone())
        .then(
            param
                .clone()
                .separated_by(just(Token::Comma))
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen)),
        )
        .then(
            stmt_parser
                .clone()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .map(Some)
                .or(just(Token::Semicolon).to(None)),
        )
        .map(|((target, params), body)| Decl::Cast {
            target,
            params,
            body,
            is_implicit: false, // Default to explicit for now
        });

    let impl_decl = just(Token::KwImpl)
        .ignore_then(type_parser.clone())
        .then(
            choice((func_decl.clone(), cast_decl.clone()))
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|(ty, methods)| Decl::Impl { ty, methods });

    let enum_decl = just(Token::KwEnum)
        .ignore_then(select! { Token::Identifier(s) => s })
        .then(
            select! { Token::Identifier(s) => s }
                .then(
                    just(Token::Assign)
                        .ignore_then(select! { Token::Integer(i) => i as u64 })
                        .or_not(),
                )
                .then_ignore(just(Token::Semicolon))
                .map(|(name, value)| crate::ast::EnumItem { name, value })
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace)),
        )
        .map(|(name, items)| Decl::Enum { name, items });

    choice((
        import_decl,
        func_decl,
        struct_decl,
        impl_decl,
        enum_decl,
        cast_decl,
    ))
}
