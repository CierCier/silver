pub mod decl;
pub mod expr;
pub mod stmt;
pub mod ty;

use crate::ast::Program;
use crate::lexer::Token;
use chumsky::prelude::*;

use crate::parser::decl::decl_parser;

pub fn parser<'a>() -> impl Parser<'a, &'a [Token], Program, extra::Err<Simple<'a, Token>>> {
    decl_parser()
        .repeated()
        .collect::<Vec<_>>()
        .then_ignore(end())
        .map(|decls| Program { decls })
}

#[cfg(test)]
mod tests;
