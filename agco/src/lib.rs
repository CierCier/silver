pub mod ast;
pub mod codegen;
pub mod lexer;
pub mod parser;

use chumsky::Parser;
use lexer::Token;
use logos::Logos;

pub fn compile(code: &str, verbose: bool) -> Result<ast::Program, String> {
    // Lexing
    let mut tokens = Vec::new();
    for (token, span) in Token::lexer(code).spanned() {
        match token {
            Ok(tok) => tokens.push(tok),
            Err(_) => {
                return Err(format!(
                    "Lexing Error at span {:?}: {:?}",
                    span,
                    &code[span.clone()]
                ));
            }
        }
    }

    if verbose {
        println!("Tokens: {:?}", tokens);
    }

    // Parsing
    let parser = parser::parser();
    let result = parser.parse(&tokens);

    match result.into_result() {
        Ok(program) => Ok(program),
        Err(errs) => {
            let mut errors = String::from("Parse Errors:\n");
            for err in errs {
                errors.push_str(&format!("{:?}\n", err));
            }
            Err(errors)
        }
    }
}
