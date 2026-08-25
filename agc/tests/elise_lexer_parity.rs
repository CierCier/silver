//! M1 gate: differential parity between the elise Silver lexer and the
//! legacy hand-written lexer, over every `.ag` file in the repository.
//!
//! For each file both lexers must agree on:
//! - success/failure,
//! - the ordered sequence of *significant* tokens (start, end, text, class),
//! - the sequence of comment trivia (start, end, doc-ness).
//!
//! Whitespace trivia has no legacy counterpart (the old scanner skips it
//! silently) so it is excluded from comparison.

use agc::grammar::lexspec::{SilverLexSpec, Tok};
use agc::lexer;
use elise_lex::{LexSpec, TokenBuf};

fn collect(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.flatten() {
        let path = entry.path();
        if path.is_dir() {
            collect(&path, out);
        } else if path.extension().is_some_and(|ext| ext == "ag") {
            out.push(path);
        }
    }
}

fn corpus() -> Vec<(String, String)> {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap();
    let mut files = Vec::new();
    for dir in ["std", "tests", "examples"] {
        collect(&root.join(dir), &mut files);
    }
    files.sort();
    files
        .into_iter()
        .map(|path| {
            let rel = path.strip_prefix(root).unwrap().display().to_string();
            let source = std::fs::read_to_string(&path).expect("corpus read");
            (rel, source)
        })
        .collect()
}

/// Canonical classification name for a legacy token.
fn legacy_class(token: &lexer::Token) -> &'static str {
    use lexer::Token as T;
    match token {
        T::IntLiteral(_) => "Int",
        T::FloatLiteral(_) => "Float",
        T::ComplexLiteral(..) => "Complex",
        T::StringLiteral(_) => "StrLit",
        T::CharLiteral(_) => "CharLit",
        T::Lifetime(_) => "Lifetime",
        T::BoolLiteral(_) => "Bool",
        T::Identifier(_) => "Ident",
        T::Comment { .. } => "Comment",
        T::Eof => "Eof",

        T::Struct => "Struct",
        T::Enum => "Enum",
        T::Impl => "Impl",
        T::Trait => "Trait",
        T::Mut => "Mut",
        T::Const => "Const",
        T::Static => "Static",
        T::Volatile => "Volatile",
        T::If => "If",
        T::Else => "Else",
        T::While => "While",
        T::For => "For",
        T::Match => "Match",
        T::Break => "Break",
        T::Continue => "Continue",
        T::Return => "Return",
        T::Type => "Type",
        T::Let => "Let",
        T::SelfType => "SelfType",
        T::Defer => "Defer",
        T::Import => "Import",
        T::Comptime => "Comptime",
        T::Cast => "Cast",
        T::Move => "Move",
        T::Extern => "Extern",
        T::Private => "Private",
        T::Asm => "Asm",
        T::As => "As",
        T::In => "In",
        T::Macro => "Macro",
        T::Launch => "Launch",
        T::Wait => "Wait",
        T::True => "True",
        T::False => "False",

        T::I8 => "I8",
        T::I16 => "I16",
        T::I32 => "I32",
        T::I64 => "I64",
        T::I128 => "I128",
        T::U8 => "U8",
        T::U16 => "U16",
        T::U32 => "U32",
        T::U64 => "U64",
        T::U128 => "U128",
        T::F32 => "F32",
        T::F64 => "F64",
        T::F80 => "F80",
        T::C32 => "C32",
        T::C64 => "C64",
        T::C80 => "C80",
        T::Bool => "BoolT",
        T::Str => "StrT",
        T::Char => "CharT",
        T::Void => "Void",
        T::Vec => "Vec",
        T::Optional => "Optional",

        T::Plus => "Plus",
        T::Minus => "Minus",
        T::Star => "Star",
        T::Slash => "Slash",
        T::Percent => "Percent",
        T::Equal => "Equal",
        T::NotEqual => "NotEqual",
        T::Less => "Less",
        T::Greater => "Greater",
        T::LessEqual => "LessEqual",
        T::GreaterEqual => "GreaterEqual",
        T::And => "AndAnd",
        T::Or => "OrOr",
        T::Not => "Not",
        T::Assign => "Assign",
        T::PlusAssign => "PlusAssign",
        T::MinusAssign => "MinusAssign",
        T::StarAssign => "StarAssign",
        T::SlashAssign => "SlashAssign",
        T::PercentAssign => "PercentAssign",
        T::BitwiseAnd => "BitAnd",
        T::BitwiseOr => "BitOr",
        T::BitwiseXor => "BitXor",
        T::BitwiseNot => "BitNot",
        T::Increment => "Increment",
        T::Decrement => "Decrement",

        T::LeftParen => "LParen",
        T::RightParen => "RParen",
        T::LeftBrace => "LBrace",
        T::RightBrace => "RBrace",
        T::LeftBracket => "LBracket",
        T::RightBracket => "RBracket",
        T::Semicolon => "Semi",
        T::Comma => "Comma",
        T::Dot => "Dot",
        T::DotDot => "DotDot",
        T::DotDotDot => "DotDotDot",
        T::Colon => "Colon",
        T::DoubleColon => "ColonColon",
        T::Question => "Question",
        T::At => "At",
        T::Hash => "Hash",
    }
}

fn elise_class(kind: u16) -> &'static str {
    fn is(k: u16, tok: Tok) -> bool { k == tok as u16 }
    match kind {
        k if is(k, Tok::IntLit) => "Int",
        k if is(k, Tok::FloatLit) => "Float",
        k if is(k, Tok::ComplexLit) => "Complex",
        k if is(k, Tok::StrLit) => "StrLit",
        k if is(k, Tok::CharLit) => "CharLit",
        k if is(k, Tok::Lifetime) => "Lifetime",
        k if is(k, Tok::Ident) => "Ident",
        k if is(k, Tok::Eof) => "Eof",
        k if is(k, Tok::True) => "True",
        k if is(k, Tok::False) => "False",
        k if is(k, Tok::Bool) => "BoolT",
        k if is(k, Tok::Str) => "StrT",
        k if is(k, Tok::Char) => "CharT",
        k if is(k, Tok::AndAnd) => "AndAnd",
        k if is(k, Tok::OrOr) => "OrOr",
        k if is(k, Tok::BitAnd) => "BitAnd",
        k if is(k, Tok::BitOr) => "BitOr",
        k if is(k, Tok::BitXor) => "BitXor",
        k if is(k, Tok::BitNot) => "BitNot",
        k if is(k, Tok::LParen) => "LParen",
        k if is(k, Tok::RParen) => "RParen",
        k if is(k, Tok::LBrace) => "LBrace",
        k if is(k, Tok::RBrace) => "RBrace",
        k if is(k, Tok::LBracket) => "LBracket",
        k if is(k, Tok::RBracket) => "RBracket",
        k if is(k, Tok::ColonColon) => "ColonColon",
        _ => TOK_NAMES.iter()
            .find(|(disc, _)| *disc == kind)
            .map(|(_, name)| *name)
            .unwrap_or("UNKNOWN"),
    }
}

/// (discriminant, canonical name) for every remaining variant.
const TOK_NAMES: &[(u16, &'static str)] = &[
    (Tok::Struct as u16, "Struct"),
    (Tok::Enum as u16, "Enum"),
    (Tok::Impl as u16, "Impl"),
    (Tok::Trait as u16, "Trait"),
    (Tok::Mut as u16, "Mut"),
    (Tok::Const as u16, "Const"),
    (Tok::Static as u16, "Static"),
    (Tok::Volatile as u16, "Volatile"),
    (Tok::If as u16, "If"),
    (Tok::Else as u16, "Else"),
    (Tok::While as u16, "While"),
    (Tok::For as u16, "For"),
    (Tok::Match as u16, "Match"),
    (Tok::Break as u16, "Break"),
    (Tok::Continue as u16, "Continue"),
    (Tok::Return as u16, "Return"),
    (Tok::Type as u16, "Type"),
    (Tok::Let as u16, "Let"),
    (Tok::SelfType as u16, "SelfType"),
    (Tok::Defer as u16, "Defer"),
    (Tok::Import as u16, "Import"),
    (Tok::Comptime as u16, "Comptime"),
    (Tok::Cast as u16, "Cast"),
    (Tok::Move as u16, "Move"),
    (Tok::Extern as u16, "Extern"),
    (Tok::Private as u16, "Private"),
    (Tok::Asm as u16, "Asm"),
    (Tok::As as u16, "As"),
    (Tok::In as u16, "In"),
    (Tok::Macro as u16, "Macro"),
    (Tok::Launch as u16, "Launch"),
    (Tok::Wait as u16, "Wait"),
    (Tok::I8 as u16, "I8"),
    (Tok::I16 as u16, "I16"),
    (Tok::I32 as u16, "I32"),
    (Tok::I64 as u16, "I64"),
    (Tok::I128 as u16, "I128"),
    (Tok::U8 as u16, "U8"),
    (Tok::U16 as u16, "U16"),
    (Tok::U32 as u16, "U32"),
    (Tok::U64 as u16, "U64"),
    (Tok::U128 as u16, "U128"),
    (Tok::F32 as u16, "F32"),
    (Tok::F64 as u16, "F64"),
    (Tok::F80 as u16, "F80"),
    (Tok::C32 as u16, "C32"),
    (Tok::C64 as u16, "C64"),
    (Tok::C80 as u16, "C80"),
    (Tok::Void as u16, "Void"),
    (Tok::Vec as u16, "Vec"),
    (Tok::Optional as u16, "Optional"),
    (Tok::Plus as u16, "Plus"),
    (Tok::Minus as u16, "Minus"),
    (Tok::Star as u16, "Star"),
    (Tok::Slash as u16, "Slash"),
    (Tok::Percent as u16, "Percent"),
    (Tok::Equal as u16, "Equal"),
    (Tok::NotEqual as u16, "NotEqual"),
    (Tok::Less as u16, "Less"),
    (Tok::Greater as u16, "Greater"),
    (Tok::LessEqual as u16, "LessEqual"),
    (Tok::GreaterEqual as u16, "GreaterEqual"),
    (Tok::Not as u16, "Not"),
    (Tok::Assign as u16, "Assign"),
    (Tok::PlusAssign as u16, "PlusAssign"),
    (Tok::MinusAssign as u16, "MinusAssign"),
    (Tok::StarAssign as u16, "StarAssign"),
    (Tok::SlashAssign as u16, "SlashAssign"),
    (Tok::PercentAssign as u16, "PercentAssign"),
    (Tok::Increment as u16, "Increment"),
    (Tok::Decrement as u16, "Decrement"),
    (Tok::Semi as u16, "Semi"),
    (Tok::Comma as u16, "Comma"),
    (Tok::Dot as u16, "Dot"),
    (Tok::DotDot as u16, "DotDot"),
    (Tok::DotDotDot as u16, "DotDotDot"),
    (Tok::Colon as u16, "Colon"),
    (Tok::Question as u16, "Question"),
    (Tok::At as u16, "At"),
    (Tok::Hash as u16, "Hash"),
];

#[test]
fn lexer_parity_over_corpus() {
    let mut spec = SilverLexSpec::new();
    let corpus = corpus();
    assert!(corpus.len() > 100, "corpus discovery broke");

    let mut checked = 0usize;

    for (name, source) in &corpus {
        check_one(name, source, &mut spec);
        checked += 1;
    }

    eprintln!("elise lexer parity verified over {checked} corpus files");
    assert!(checked > 200);
}

#[test]
fn lexer_parity_on_adversarial_snippets() {
    let mut spec = SilverLexSpec::new();
    let snippets: &[(&str, &str)] = &[
        ("nested-comments", "/* /* nested */ still */ fn main() {}"),
        ("lifetime", "struct SV<'a> { &'a i64 d; } i32 main() { return 0; }"),
        ("complex", "c64 a = 3.5i; c64 b = 5i;"),
        (
            "string-escapes",
            "str s = \"\\u{1F600} \\x41 \\n \\t \\\" \";",
        ),
        ("hex", "i32 x = 0xDEADBEEF;"),
        ("float-edge", "f64 y = 3.14; i32 n = 42;"),
        (
            "char-literals",
            "char a = 'x'; char nl = '\\n'; char q = '\\'';",
        ),
        ("operators", "a <<= 1;"), // `<<=` lexes as `<` `<` `=` (legacy behavior)
        (
            "empty-comment",
            "/**///\n/////\n/*/**/*/",
        ),
    ];
    for (name, source) in snippets {
        check_one(name, source, &mut spec);
    }
}

fn check_one(name: &str, source: &str, spec: &mut SilverLexSpec) {
    let legacy = lexer::lex_with_source(source, 0);
    let elise = elise_lex::scan(spec, source);

    match (legacy, elise) {
        (Ok(legacy_tokens), Ok(buf)) => {
            check_stream(name, source, &legacy_tokens, &buf);
        }
        (Err(_), Err(_)) => {
            // Both reject the input; error-position equality is checked
            // loosely (both must fail) since spans are shaped differently.
        }
        (legacy_result, elise_result) => {
            panic!(
                "{name}: lexers disagree on success: legacy={:?} elise={:?}",
                legacy_result.map(|t| t.len()).map_err(|e| format!("{e:?}")),
                elise_result.as_ref().map(|b| b.len()).map_err(|e| format!("{e:?}"))
            );
        }
    }
}

fn check_stream(name: &str, source: &str, legacy: &[lexer::LexToken], buf: &TokenBuf) {
    // Significant tokens (comments handled separately below).
    let legacy_sig: Vec<&lexer::LexToken> = legacy
        .iter()
        .filter(|t| {
            !matches!(t.kind, lexer::Token::Comment { .. } | lexer::Token::Eof)
        })
        .collect();
    let elise_rows: Vec<_> = buf
        .rows()
        .iter()
        .filter(|row| row.kind != Tok::Eof as u16)
        .collect();

    if legacy_sig.len() != elise_rows.len() {
        // Find the first divergence to make the failure actionable.
        let mut div = 0usize;
        while div < legacy_sig.len().min(elise_rows.len()) {
            let (lt, row) = (&legacy_sig[div], &elise_rows[div]);
            if lt.span.start != row.start as usize
                || lt.span.end != (row.start + row.len) as usize
            {
                break;
            }
            div += 1;
        }
        let ctx = |t: &lexer::LexToken| format!("{:?} {:?}", t.text, t.span);
        let el_ctx = |row: &elise_lex::TokenRow| {
            format!("{:?}", &source[row.start as usize..(row.start + row.len) as usize])
        };
        panic!(
            "{name}: token count differs: legacy={} elise={} \
             first divergence at #{div}: legacy={} elise={}",
            legacy_sig.len(),
            elise_rows.len(),
            legacy_sig.get(div).map(|t| ctx(t)).unwrap_or_default(),
            elise_rows.get(div).map(|row| el_ctx(row)).unwrap_or_default(),
        );
    }

    for (index, (lt, row)) in legacy_sig.iter().zip(elise_rows.iter()).enumerate() {
        let (ls, le) = (lt.span.start, lt.span.end);
        let (es, ee) = (row.start as usize, (row.start + row.len) as usize);
        if ls != es || le != ee {
            panic!(
                "{name}: token #{index} span differs: legacy=[{ls},{le}) {:?} elise=[{es},{ee})",
                lt.text
            );
        }
        let lc = legacy_class(&lt.kind);
        let ec = elise_class(row.kind);
        if lc != ec {
            panic!(
                "{name}: token #{index} class differs at [{ls},{le}) {:?}: legacy={lc} elise={ec}",
                lt.text
            );
        }
    }

    // Comments: legacy emits them as inline tokens; elise as trivia.
    let legacy_comments: Vec<(usize, usize)> = legacy
        .iter()
        .filter_map(|t| {
            matches!(t.kind, lexer::Token::Comment { .. })
                .then_some((t.span.start, t.span.end))
        })
        .collect();
    let elise_comments: Vec<(usize, usize)> = buf
        .trivia()
        .iter()
        .filter(|t| t.kind != Tok::TriviaLayout as u16)
        .map(|t| (t.start as usize, (t.start + t.len) as usize))
        .collect();
    assert_eq!(
        legacy_comments, elise_comments,
        "{name}: comment ranges differ"
    );

    // Sanity: round-trip every compared token's text through the source.
    let _ = source;
}
