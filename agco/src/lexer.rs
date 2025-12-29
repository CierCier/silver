use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(skip r"[ \t\n\f\r]+")] // Skip whitespace
#[logos(skip r"//.*")] // Skip single-line comments
#[logos(skip r"/\*([^*]|\*+[^*/])*\*+/")] // Skip multi-line comments
pub enum Token {
    // Keywords
    #[token("import")]
    KwImport,
    #[token("struct")]
    KwStruct,
    #[token("enum")]
    KwEnum,
    #[token("extern")]
    KwExtern,
    #[token("static")]
    KwStatic,
    #[token("link")]
    KwLink,
    #[token("switch")]
    KwSwitch,
    #[token("case")]
    KwCase,
    #[token("default")]
    KwDefault,
    #[token("impl")]
    KwImpl,
    #[token("comptime")]
    KwComptime,
    #[token("const")]
    KwConst,
    #[token("return")]
    KwReturn,
    #[token("for")]
    KwFor,
    #[token("while")]
    KwWhile,
    #[token("if")]
    KwIf,
    #[token("else")]
    KwElse,
    #[token("break")]
    KwBreak,
    #[token("continue")]
    KwContinue,
    #[token("asm")]
    KwAsm,
    #[token("as")]
    KwAs,
    #[token("cast")]
    KwCast,
    #[token("implicit")]
    KwImplicit,
    #[token("trait")]
    KwTrait,
    #[token("new")]
    KwNew,
    #[token("drop")]
    KwDrop,
    #[token("alloc")]
    KwAlloc,
    #[token("free")]
    KwFree,
    #[token("true")]
    KwTrue,
    #[token("false")]
    KwFalse,

    // Primitive Types
    #[token("void")]
    KwVoid,
    #[token("bool")]
    KwBool,
    #[token("i8")]
    KwI8,
    #[token("i16")]
    KwI16,
    #[token("i32")]
    KwI32,
    #[token("i64")]
    KwI64,
    #[token("u8")]
    KwU8,
    #[token("u16")]
    KwU16,
    #[token("u32")]
    KwU32,
    #[token("u64")]
    KwU64,
    #[token("f32")]
    KwF32,
    #[token("f64")]
    KwF64,
    #[token("char")]
    KwChar,
    #[token("str")]
    KwStr,

    // Identifiers and Literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Identifier(String),

    #[regex(r"[0-9]+", |lex| lex.slice().parse().ok())]
    Integer(i64),

    #[regex(r"[0-9]+\.[0-9]+", |lex| lex.slice().parse().ok())]
    Float(f64),

    // String literal: simplified for now, advanced escaping later
    #[regex(r#""([^"\\]|\\[\s\S])*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string() // Strip quotes
    })]
    String(String),

    // Char literal
    #[regex(r"'([^'\\]|\\.)'", |lex| {
        let s = lex.slice();
        // Naive char parsing
        s.chars().nth(1).unwrap()
    })]
    CharLiteral(char),

    // Punctuation
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("...")]
    DotDotDot,
    #[token("?")]
    Question,
    #[token("@")]
    At,
    #[token("->")]
    Arrow,

    // Operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("++")]
    PlusPlus,
    #[token("--")]
    MinusMinus,

    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,

    #[token("=")]
    Assign,
    #[token("+=")]
    PlusAssign,
    #[token("-=")]
    MinusAssign,
    #[token("*=")]
    StarAssign,
    #[token("/=")]
    SlashAssign,
    #[token("%=")]
    PercentAssign,
    #[token("&=")]
    AmpAssign, // Not in C++ enum but logical
    #[token("|=")]
    PipeAssign, // Not in C++ enum but logical
    #[token("^=")]
    CaretAssign, // Not in C++ enum but logical

    #[token("<<")]
    Shl,
    #[token(">>")]
    Shr,
    #[token("<<=")]
    ShlAssign,
    #[token(">>=")]
    ShrAssign,

    #[token("==")]
    Eq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,

    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
