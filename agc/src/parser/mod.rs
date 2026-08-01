pub mod ast;
pub mod error;
pub mod import_hook;
pub mod prt_parser;

pub use ast::*;
pub use error::ParseError;
pub use import_hook::FileImportResolverHook;

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
        // Comments are captured by the lexer as tokens; pull them out of the
        // stream (the PRT and statement parsers never see them) and attach
        // them to the program as CommentItem nodes, in source order.
        let mut comments: Vec<ast::CommentItem> = Vec::new();
        let code_tokens: Vec<LexToken> = self
            .tokens
            .iter()
            .filter(|token| match &token.kind {
                crate::lexer::Token::Comment { kind, text } => {
                    comments.push(ast::CommentItem {
                        kind: *kind,
                        text: text.clone(),
                        span: token.span.clone(),
                    });
                    false
                }
                _ => true,
            })
            .cloned()
            .collect();

        let mut parser = prt_parser::PRT_Parser::new(self.source_name.clone());
        match parser.parse_program(&code_tokens) {
            Ok(mut program) => {
                program.comments = comments;
                (program, Vec::new())
            }
            Err(error) => {
                let fallback_span = self
                    .tokens
                    .last()
                    .map(|token| token.span.clone())
                    .unwrap_or(Span::default());
                (
                    Program {
                        attributes: Vec::new(),
                        items: Vec::new(),
                        comments,
                        span: fallback_span,
                    },
                    vec![error],
                )
            }
        }
    }
}
