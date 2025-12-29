use crate::ast::TypeName;
use crate::lexer::Token;
use chumsky::prelude::*;

pub fn type_parser<'a>()
-> impl Parser<'a, &'a [Token], TypeName, extra::Err<Simple<'a, Token>>> + Clone {
    recursive(|ty| {
        // Base type: Identifier or Primitive Keyword
        let base = select! {
            Token::Identifier(name) => name,
            Token::KwVoid => "void".to_string(),
            Token::KwBool => "bool".to_string(),
            Token::KwI8 => "i8".to_string(),
            Token::KwI16 => "i16".to_string(),
            Token::KwI32 => "i32".to_string(),
            Token::KwI64 => "i64".to_string(),
            Token::KwU8 => "u8".to_string(),
            Token::KwU16 => "u16".to_string(),
            Token::KwU32 => "u32".to_string(),
            Token::KwU64 => "u64".to_string(),
            Token::KwF32 => "f32".to_string(),
            Token::KwF64 => "f64".to_string(),
            Token::KwChar => "char".to_string(),
            Token::KwStr => "str".to_string(),
        };

        // Generics: <T, U>
        let generics = ty
            .clone()
            .separated_by(just(Token::Comma))
            .collect::<Vec<_>>()
            .delimited_by(just(Token::Lt), just(Token::Gt))
            .or_not()
            .map(|g| g.unwrap_or_default());

        // Pointers: *T (prefix)
        let stars = just(Token::Star)
            .repeated()
            .collect::<Vec<_>>()
            .map(|s| s.len() as u32);

        // Array dims: [size] (suffix)
        // [10] is Option<u64>(10). [] is Option<u64>(None).
        // For simple parsing, size is just an integer literal for now.
        // TODO: Full generic expressions in array size?
        let array_dim = just(Token::LBracket)
            .ignore_then(select! { Token::Integer(i) => i as u64 }.or_not())
            .then_ignore(just(Token::RBracket));

        let arrays = array_dim.repeated().collect::<Vec<_>>();

        // Combine: base -> generics -> stars -> arrays
        base.then(generics)
            .then(stars)
            .then(arrays)
            .map(|(((name, args), depth), dims)| TypeName {
                name,
                pointer_depth: depth,
                generic_args: args,
                array_dims: dims,
            })
    })
}
