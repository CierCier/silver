pub mod ast;
pub mod error;
<<<<<<< HEAD
pub mod graph;
=======
pub mod prt_parser;
>>>>>>> cc823df (shift to LL3)

pub use ast::*;
pub use error::{ParseError, ParseResult};

use crate::lexer::{LexToken, Span};

pub struct Parser {
    tokens: Vec<LexToken>,
    source_name: Option<String>,
}

impl Parser {
    pub fn new(tokens: Vec<LexToken>) -> Self {
        Self {
            tokens,
            source_name: None,
        }
    }

    pub fn new_with_source(tokens: Vec<LexToken>, source_name: impl Into<String>) -> Self {
        Self {
            tokens,
            source_name: Some(source_name.into()),
        }
    }

    pub fn parse_program(&mut self) -> (Program, Vec<ParseError>) {
<<<<<<< HEAD
        let mut parser = graph::GraphParser::new(self.source_name.clone());
=======
        let mut parser = prt_parser::PRT_Parser::new(self.source_name.clone());
>>>>>>> cc823df (shift to LL3)
        match parser.parse_program(&self.tokens) {
            Ok(program) => (program, Vec::new()),
            Err(error) => {
                let fallback_span = self
                    .tokens
                    .last()
                    .map(|token| token.span.clone())
                    .unwrap_or(Span { start: 0, end: 0 });
                (
                    Program {
                        items: Vec::new(),
                        span: fallback_span,
                    },
                    vec![error],
                )
            }
        }
    }
}
