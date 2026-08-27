//! Lowers an Elise `SourceGraph` into a strongly-typed Silver `ast::Program`.

use crate::lexer::{LexToken, Span};
use crate::parser::ast::*;
use crate::parser::prt_parser::PRT_Parser;
use elise_core::SourceGraph;

/// Lowers a `SourceGraph` produced by `parse_ag` into a Silver `ast::Program`.
pub fn lower_source_graph(graph: &SourceGraph, file_id: usize) -> Program {
    let src = graph.text();
    let root = graph.root();
    let mut comments = Vec::new();

    let root_span = Span::new(root.span().0, root.span().1);

    let all_tokens = match crate::lexer::lex_with_source(src, file_id as u32) {
        Ok(toks) => toks,
        Err(_) => Vec::new(),
    };

    let code_tokens: Vec<LexToken> = all_tokens
        .into_iter()
        .filter(|token| match &token.kind {
            crate::lexer::Token::Comment { kind, text } => {
                comments.push(CommentItem {
                    kind: *kind,
                    text: text.clone(),
                    span: token.span,
                });
                false
            }
            crate::lexer::Token::Eof => false,
            _ => true,
        })
        .collect();

    let mut parser = PRT_Parser::new(None);
    match parser.parse_program(&code_tokens) {
        Ok(mut prog) => {
            prog.comments = comments;
            prog
        }
        Err(_) => Program {
            attributes: Vec::new(),
            items: Vec::new(),
            comments,
            span: root_span,
        },
    }
}
