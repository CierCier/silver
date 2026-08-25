fn main() {
    let cases = [
        "/*/**/*/",
        "/**///\n/////\n/*/**/*/",
        "a /* x /* y */ z */ b",
        "let lt = 'a;",
        "c64 z = 3.5i; c64 w = 5i;",
    ];
    let mut spec = agc::grammar::SilverLexSpec::new();
    for case in cases {
        match agc::lexer::lex_with_source(case, 0) {
            Ok(tokens) => {
                println!("{case:?} -> {} legacy tokens", tokens.len());
                for t in &tokens {
                    println!("  {:?} {:?}", t.text, t.span);
                }
            }
            Err(e) => println!("{case:?} -> LEGACY ERR {e:?}"),
        }
        match elise_lex::scan(&mut spec, case) {
            Ok(buf) => {
                println!(
                    "  elise -> {} tokens, {} trivia",
                    buf.len(),
                    buf.trivia().len()
                );
                for t in buf.trivia() {
                    println!("  trivia {:?}", t);
                }
            }
            Err(e) => println!("  elise ERR {e:?}"),
        }
    }
}
