use crate::lexer::Token;
use crate::parser::parser;
use chumsky::Parser;
use logos::Logos;

fn lex(code: &str) -> Vec<Token> {
    Token::lexer(code)
        .map(|res| res.expect("Lexing error"))
        .collect()
}

#[test]
fn test_func_decl() {
    let code = "void main() { return 0; }";
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}

#[test]
fn test_struct_decl() {
    // Valid: semicolons separate fields
    let code = "struct Point { i32 x; i32 y; }";
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}

#[test]
fn test_control_flow() {
    let code = "
    void test() {
        if (true) {
            return 1;
        } else {
            return 0;
        }
        while (x < 10) {
            x = x + 1;
        }
    }
    ";
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}

#[test]
fn test_precedence() {
    let code = "void f() { return 1 + 2 * 3 == 7; }";
    // (1 + (2 * 3)) == 7
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}

#[test]
fn test_assign() {
    let code = "void f() { x = 5; y += 10; }";
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}

#[test]
fn test_loop() {
    let code = "void f() { for (i32 i=0; i<10; i++) { } }";
    let tokens = lex(code);
    let parser = parser();
    let result = parser.parse(&tokens);
    assert!(result.into_result().is_ok());
}
