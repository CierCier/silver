use std::fmt;

/// Kinds of comments captured by the lexer (previously discarded).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CommentKind {
    /// `// ...`
    Line,
    /// `/// ...` — documentation comment
    DocLine,
    /// `/* ... */`
    Block,
    /// `/** ... */` — documentation comment
    DocBlock,
}

impl CommentKind {
    pub fn is_doc(&self) -> bool {
        matches!(self, CommentKind::DocLine | CommentKind::DocBlock)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    IntLiteral(i128),
    FloatLiteral(f64),
    ComplexLiteral(f64, f64), // (real, imaginary)
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),

    // Keywords
    Struct,
    Enum,
    Impl,
    Trait,
    // (Fn removed — trait methods use C-style syntax)
    Mut,
    Const,
    Static,
    Volatile,
    If,
    Else,
    While,
    For,
    Match,
    Break,
    Continue,
    Return,
    Type,
    SelfType,
    Defer,
    Import,
    Comptime,
    Cast,
    Move,
    Extern,
    // (Pub removed — items public by default, Private to opt out)
    Private,
    Asm,
    In,
    True,
    False,
    // Spawn/join keywords (fearless concurrency): `launch` starts a detached
    // OS thread running the wrapped call; `wait` joins a Task and reads its
    // result.
    Launch,
    Wait,

    // Integer Types
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,

    // Floating Point Types
    F32,
    F64,
    F80,

    // Complex Types
    C32,
    C64,
    C80, // Complex32, Complex64, Complex80

    // Other Types
    Bool,
    Str,
    Char,
    Void,
    Vec,
    Optional,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    And,
    Or,
    Not,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    Increment,
    Decrement,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Semicolon,
    Comma,
    Dot,
    DotDot,
    DotDotDot,
    Colon,
    DoubleColon,
    Question,
    At,
    Hash,

    // Keywords
    Macro,

    // Special
    Identifier(String),
    /// A captured comment (`//`, `///`, `/* */`, `/** */`) with its content.
    /// Previously discarded by the lexer; now surfaced so the parser can
    /// attach them to the AST and dispatch them (LLVM IR comments, LSP docs).
    Comment {
        kind: CommentKind,
        text: String,
    },
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LexError {
    UnexpectedChar {
        found: char,
        span: (usize, usize),
    },
    UnexpectedEof {
        span: (usize, usize),
    },
    InvalidNumber {
        span: (usize, usize),
        message: String,
    },
    InvalidString {
        span: (usize, usize),
        message: String,
    },
    InvalidChar {
        span: (usize, usize),
        message: String,
    },
}

// Compatibility types for the existing main.rs interface
#[derive(Debug, Clone)]
pub struct LexToken {
    pub kind: Token,
    pub span: Span,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    /// Source file id (index into the process-wide source registry); 0 means
    /// unregistered (e.g. synthetic spans, in-memory lexing in tests).
    pub file: u32,
    /// 1-based line/column of the span start; 0 means unknown.
    pub start_line: u32,
    pub start_col: u32,
    /// 1-based line/column of the span end; 0 means unknown.
    pub end_line: u32,
    pub end_col: u32,
}

impl Span {
    /// Synthetic span with no source location (file 0, unknown line/col).
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            ..Self::default()
        }
    }

    /// Span running from `self.start` through `other.end`, carrying `self`'s
    /// file and start location plus `other`'s end location. Used to combine
    /// two token spans when building AST nodes.
    pub fn extend_to(&self, other: &Span) -> Self {
        Self {
            end: other.end,
            end_line: other.end_line,
            end_col: other.end_col,
            ..*self
        }
    }

    /// Keep the start location and file, but set a new end byte offset.
    pub fn with_end(&self, end: usize) -> Self {
        Self {
            end,
            end_line: 0,
            end_col: 0,
            ..*self
        }
    }

    /// True when the span carries no usable source location.
    pub fn is_synthetic(&self) -> bool {
        self.file == 0 || self.start_line == 0
    }

    /// Human-readable `path:line:col` for messages rendered without carets.
    pub fn display_location(&self) -> String {
        if self.is_synthetic() {
            return String::from("unknown location");
        }
        let path = source_file(self.file)
            .map(|f| f.path)
            .unwrap_or_else(|| format!("file#{}", self.file));
        format!("{}:{}:{}", path, self.start_line, self.start_col)
    }
}

#[derive(Debug, Clone)]
pub struct LexErrorCompat {
    pub kind: LexError,
    pub span: Span,
}

// Public interface function expected by main.rs
pub fn lex(input: &str) -> Result<Vec<LexToken>, Vec<LexErrorCompat>> {
    lex_with_source(input, 0)
}

/// Lex a source file with an explicit file id (from the source registry).
pub fn lex_with_source(input: &str, file: u32) -> Result<Vec<LexToken>, Vec<LexErrorCompat>> {
    let mut lexer = Lexer::with_source(input.to_string(), file);
    match lexer.tokenize_with_spans() {
        Ok(tokens) => Ok(tokens),
        Err(error) => {
            let byte_span = match error {
                LexError::UnexpectedChar { span, .. }
                | LexError::UnexpectedEof { span }
                | LexError::InvalidNumber { span, .. }
                | LexError::InvalidString { span, .. }
                | LexError::InvalidChar { span, .. } => span,
            };
            let (start_line, start_col) = line_col_at(input, byte_span.0);
            let (end_line, end_col) = line_col_at(input, byte_span.1);
            Err(vec![LexErrorCompat {
                kind: error.clone(),
                span: Span {
                    start: byte_span.0,
                    end: byte_span.1,
                    file,
                    start_line,
                    start_col,
                    end_line,
                    end_col,
                },
            }])
        }
    }
}

/// Compute 1-based (line, column) for a byte offset in `text`.
pub fn line_col_at(text: &str, offset: usize) -> (u32, u32) {
    let mut line = 1u32;
    let mut col = 1u32;
    for ch in text[..offset.min(text.len())].chars() {
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

/// A registered source file: path + full text, indexed by file id.
#[derive(Debug, Clone)]
pub struct SourceFile {
    pub path: String,
    pub text: String,
}

thread_local! {
    static SOURCE_REGISTRY: std::cell::RefCell<Vec<SourceFile>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Register a source file's path and text, returning its file id (1-based;
/// id 0 is reserved for synthetic/unregistered spans). Re-registering the
/// same path returns the existing id.
pub fn register_source(path: &str, text: &str) -> u32 {
    SOURCE_REGISTRY.with(|registry| {
        let mut files = registry.borrow_mut();
        if let Some((idx, _)) = files.iter().enumerate().find(|(_, f)| f.path == path) {
            return idx as u32 + 1;
        }
        files.push(SourceFile {
            path: path.to_string(),
            text: text.to_string(),
        });
        files.len() as u32
    })
}

/// Look up a registered source file by id (0 = unregistered).
pub fn source_file(file: u32) -> Option<SourceFile> {
    if file == 0 {
        return None;
    }
    SOURCE_REGISTRY.with(|registry| registry.borrow().get(file as usize - 1).cloned())
}

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    line: usize,
    column: usize,
    file: u32,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self::with_source(input, 0)
    }

    pub fn with_source(input: String, file: u32) -> Self {
        Self {
            input,
            position: 0,
            line: 1,
            column: 1,
            file,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            if self.starts_comment() {
                let (kind, text) = self.next_comment()?;
                tokens.push(Token::Comment { kind, text });
                continue;
            }

            let token = self.next_token()?;
            tokens.push(token);
        }

        tokens.push(Token::Eof);
        Ok(tokens)
    }

    pub fn tokenize_with_spans(&mut self) -> Result<Vec<LexToken>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            if self.starts_comment() {
                let start = self.position;
                let start_line = self.line as u32;
                let start_col = self.column as u32;
                let (kind, text) = self.next_comment()?;
                let end = self.position;
                let end_line = self.line as u32;
                let end_col = self.column as u32;
                tokens.push(LexToken {
                    kind: Token::Comment {
                        kind,
                        text: text.clone(),
                    },
                    span: Span {
                        start,
                        end,
                        file: self.file,
                        start_line,
                        start_col,
                        end_line,
                        end_col,
                    },
                    text,
                });
                continue;
            }

            let start = self.position;
            let start_line = self.line as u32;
            let start_col = self.column as u32;
            let token = self.next_token()?;
            let end = self.position;
            let end_line = self.line as u32;
            let end_col = self.column as u32;
            let text = self.input.get(start..end).unwrap_or("").to_string();

            tokens.push(LexToken {
                kind: token,
                span: Span {
                    start,
                    end,
                    file: self.file,
                    start_line,
                    start_col,
                    end_line,
                    end_col,
                },
                text,
            });
        }

        let end = self.position;
        let eof_line = self.line as u32;
        let eof_col = self.column as u32;
        tokens.push(LexToken {
            kind: Token::Eof,
            span: Span {
                start: end,
                end,
                file: self.file,
                start_line: eof_line,
                start_col: eof_col,
                end_line: eof_line,
                end_col: eof_col,
            },
            text: String::new(),
        });

        Ok(tokens)
    }

    fn next_token(&mut self) -> Result<Token, LexError> {
        let start_pos = self.position;
        let ch = self.advance();

        match ch {
            // Single character tokens
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            '[' => Ok(Token::LeftBracket),
            ']' => Ok(Token::RightBracket),
            ';' => Ok(Token::Semicolon),
            ',' => Ok(Token::Comma),
            '.' => {
                if self.match_char('.') {
                    if self.match_char('.') {
                        Ok(Token::DotDotDot)
                    } else {
                        Ok(Token::DotDot)
                    }
                } else {
                    Ok(Token::Dot)
                }
            }
            '?' => Ok(Token::Question),
            '@' => Ok(Token::At),
            '#' => Ok(Token::Hash),
            '~' => Ok(Token::BitwiseNot),

            // Operators that might be compound
            '+' => {
                if self.match_char('+') {
                    Ok(Token::Increment)
                } else if self.match_char('=') {
                    Ok(Token::PlusAssign)
                } else {
                    Ok(Token::Plus)
                }
            }
            '-' => {
                if self.match_char('-') {
                    Ok(Token::Decrement)
                } else if self.match_char('=') {
                    Ok(Token::MinusAssign)
                } else {
                    Ok(Token::Minus)
                }
            }
            '*' => {
                if self.match_char('=') {
                    Ok(Token::StarAssign)
                } else {
                    Ok(Token::Star)
                }
            }
            '/' => {
                if self.match_char('=') {
                    Ok(Token::SlashAssign)
                } else {
                    Ok(Token::Slash)
                }
            }
            '%' => {
                if self.match_char('=') {
                    Ok(Token::PercentAssign)
                } else {
                    Ok(Token::Percent)
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(Token::Equal)
                } else {
                    Ok(Token::Assign)
                }
            }
            '!' => {
                if self.match_char('=') {
                    Ok(Token::NotEqual)
                } else {
                    Ok(Token::Not)
                }
            }
            '<' => {
                if self.match_char('=') {
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            }
            '&' => {
                if self.match_char('&') {
                    Ok(Token::And)
                } else {
                    Ok(Token::BitwiseAnd)
                }
            }
            '|' => {
                if self.match_char('|') {
                    Ok(Token::Or)
                } else {
                    Ok(Token::BitwiseOr)
                }
            }
            '^' => Ok(Token::BitwiseXor),
            ':' => {
                if self.match_char(':') {
                    Ok(Token::DoubleColon)
                } else {
                    Ok(Token::Colon)
                }
            }

            // String literals
            '"' => self.string_literal(),

            // Character literals
            '\'' => self.char_literal(),

            // Numbers
            '0'..='9' => self.number_literal(ch),

            // Identifiers and keywords
            'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_keyword(ch),

            _ => Err(LexError::UnexpectedChar {
                found: ch,
                span: (start_pos, self.position),
            }),
        }
    }

    fn string_literal(&mut self) -> Result<Token, LexError> {
        let start_pos = self.position - 1;
        // Raw bytes: `\xNN` escapes must contribute exact bytes (C-like
        // semantics), so the literal is assembled byte-wise and validated as
        // UTF-8 at the end (Silver `str` is UTF-8 text).
        let mut bytes: Vec<u8> = Vec::new();

        while !self.is_at_end() && self.peek() != '"' {
            let ch = self.advance_utf8_char();
            if ch == '\\' {
                // Handle escape sequences
                if self.is_at_end() {
                    return Err(LexError::InvalidString {
                        span: (start_pos, self.position),
                        message: "Unterminated string literal".to_string(),
                    });
                }
                let escaped = self.advance();
                match escaped {
                    'n' => bytes.push(b'\n'),
                    't' => bytes.push(b'\t'),
                    'r' => bytes.push(b'\r'),
                    '\\' => bytes.push(b'\\'),
                    '"' => bytes.push(b'"'),
                    '\'' => bytes.push(b'\''),
                    '0' => bytes.push(0),
                    'x' => {
                        let (hi, lo) = (self.advance(), self.advance());
                        let Some(byte) = Self::parse_hex_byte(hi, lo) else {
                            return Err(LexError::InvalidString {
                                span: (start_pos, self.position),
                                message: format!("Invalid hex escape: \\x{hi}{lo}"),
                            });
                        };
                        bytes.push(byte);
                    }
                    'u' => {
                        if !self.match_char('{') {
                            return Err(LexError::InvalidString {
                                span: (start_pos, self.position),
                                message: "Expected `{` after \\u".to_string(),
                            });
                        }
                        let mut codepoint: u32 = 0;
                        let mut digits = 0u8;
                        while !self.is_at_end() && self.peek() != '}' && digits < 6 {
                            let Some(d) = Self::hex_digit(self.advance()) else {
                                return Err(LexError::InvalidString {
                                    span: (start_pos, self.position),
                                    message: "Invalid hex digit in \\u escape".to_string(),
                                });
                            };
                            codepoint = codepoint * 16 + d as u32;
                            digits += 1;
                        }
                        if digits == 0 {
                            return Err(LexError::InvalidString {
                                span: (start_pos, self.position),
                                message: "Empty \\u{} escape".to_string(),
                            });
                        }
                        if self.is_at_end() || self.peek() != '}' {
                            return Err(LexError::InvalidString {
                                span: (start_pos, self.position),
                                message: "Unclosed \\u{ escape".to_string(),
                            });
                        }
                        self.advance(); // consume `}`
                        let Some(ch) = char::from_u32(codepoint) else {
                            return Err(LexError::InvalidString {
                                span: (start_pos, self.position),
                                message: format!("Invalid Unicode scalar: \\u{{{codepoint:X}}}"),
                            });
                        };
                        let mut buf = [0u8; 4];
                        bytes.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
                    }
                    _ => {
                        return Err(LexError::InvalidString {
                            span: (start_pos, self.position),
                            message: format!("Invalid escape sequence: \\{}", escaped),
                        });
                    }
                }
            } else {
                let mut buf = [0u8; 4];
                bytes.extend_from_slice(ch.encode_utf8(&mut buf).as_bytes());
            }
        }

        if self.is_at_end() {
            return Err(LexError::InvalidString {
                span: (start_pos, self.position),
                message: "Unterminated string literal".to_string(),
            });
        }

        self.advance(); // consume closing quote
        let Ok(value) = String::from_utf8(bytes) else {
            return Err(LexError::InvalidString {
                span: (start_pos, self.position),
                message: "string literal contains bytes that are not valid UTF-8; \
                    use \\u{} escapes or literal characters for text"
                    .to_string(),
            });
        };
        Ok(Token::StringLiteral(value))
    }

    fn char_literal(&mut self) -> Result<Token, LexError> {
        let start_pos = self.position - 1;

        if self.is_at_end() {
            return Err(LexError::InvalidChar {
                span: (start_pos, self.position),
                message: "Unterminated character literal".to_string(),
            });
        }

        let ch = self.advance();
        let value = if ch == '\\' {
            // Handle escape sequences
            if self.is_at_end() {
                return Err(LexError::InvalidChar {
                    span: (start_pos, self.position),
                    message: "Unterminated character literal".to_string(),
                });
            }
            let escaped = self.advance();
            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                '0' => '\0',
                _ => {
                    return Err(LexError::InvalidChar {
                        span: (start_pos, self.position),
                        message: format!("Invalid escape sequence: \\{}", escaped),
                    });
                }
            }
        } else {
            ch
        };

        if self.is_at_end() || self.peek() != '\'' {
            return Err(LexError::InvalidChar {
                span: (start_pos, self.position),
                message: "Unterminated character literal".to_string(),
            });
        }

        self.advance(); // consume closing quote
        Ok(Token::CharLiteral(value))
    }

    fn number_literal(&mut self, first_digit: char) -> Result<Token, LexError> {
        let start_pos = self.position - 1;

        // Check for hex literal (0x or 0X prefix)
        if first_digit == '0' && !self.is_at_end() && (self.peek() == 'x' || self.peek() == 'X') {
            self.advance(); // consume 'x' or 'X'
            let mut hex_str = String::new();
            while !self.is_at_end() && self.peek().is_ascii_hexdigit() {
                hex_str.push(self.advance());
            }
            if hex_str.is_empty() {
                return Err(LexError::InvalidNumber {
                    span: (start_pos, self.position),
                    message: "Invalid hex literal: expected hex digits after 0x".to_string(),
                });
            }
            let value =
                i128::from_str_radix(&hex_str, 16).map_err(|_| LexError::InvalidNumber {
                    span: (start_pos, self.position),
                    message: "Invalid hex number".to_string(),
                })?;
            return Ok(Token::IntLiteral(value));
        }

        let mut number_str = String::new();
        number_str.push(first_digit);

        // Collect digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            number_str.push(self.advance());
        }

        // Check for decimal point
        if !self.is_at_end()
            && self.peek() == '.'
            && self.peek_next().is_some_and(|c| c.is_ascii_digit())
        {
            number_str.push(self.advance()); // consume '.'
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                number_str.push(self.advance());
            }

            // Check for complex number suffix 'i'
            if !self.is_at_end() && self.peek() == 'i' {
                self.advance(); // consume 'i'
                let imaginary = number_str
                    .parse::<f64>()
                    .map_err(|_| LexError::InvalidNumber {
                        span: (start_pos, self.position),
                        message: "Invalid floating point number".to_string(),
                    })?;
                return Ok(Token::ComplexLiteral(0.0, imaginary));
            }

            // Regular floating point number
            let value = number_str
                .parse::<f64>()
                .map_err(|_| LexError::InvalidNumber {
                    span: (start_pos, self.position),
                    message: "Invalid floating point number".to_string(),
                })?;
            Ok(Token::FloatLiteral(value))
        } else {
            // Check for complex number suffix 'i'
            if !self.is_at_end() && self.peek() == 'i' {
                self.advance(); // consume 'i'
                let imaginary = number_str
                    .parse::<f64>()
                    .map_err(|_| LexError::InvalidNumber {
                        span: (start_pos, self.position),
                        message: "Invalid integer number".to_string(),
                    })?;
                return Ok(Token::ComplexLiteral(0.0, imaginary));
            }

            // Regular integer
            let value = number_str
                .parse::<i128>()
                .map_err(|_| LexError::InvalidNumber {
                    span: (start_pos, self.position),
                    message: "Invalid integer number".to_string(),
                })?;
            Ok(Token::IntLiteral(value))
        }
    }

    fn identifier_or_keyword(&mut self, first_char: char) -> Result<Token, LexError> {
        let mut identifier = String::new();
        identifier.push(first_char);

        while !self.is_at_end() && (self.peek().is_alphanumeric() || self.peek() == '_') {
            identifier.push(self.advance());
        }

        // Check if it's a keyword
        let token = match identifier.as_str() {
            // Keywords
            "struct" => Token::Struct,
            "enum" => Token::Enum,
            "impl" => Token::Impl,
            "trait" => Token::Trait,
            // (fn keyword removed — trait methods use C-style)
            "mut" => Token::Mut,
            "const" => Token::Const,
            "static" => Token::Static,
            "volatile" => Token::Volatile,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "match" => Token::Match,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "return" => Token::Return,
            "defer" => Token::Defer,
            "import" => Token::Import,
            "comptime" => Token::Comptime,
            "cast" => Token::Cast,
            "move" => Token::Move,
            "type" => Token::Type,
            "Self" => Token::SelfType,
            "extern" => Token::Extern,
            // (pub keyword removed — items public by default)
            "private" => Token::Private,
            "asm" => Token::Asm,
            "in" => Token::In,
            "macro" => Token::Macro,
            "true" => Token::True,
            "false" => Token::False,
            "launch" => Token::Launch,
            "wait" => Token::Wait,

            // Integer types
            "i8" => Token::I8,
            "i16" => Token::I16,
            "i32" => Token::I32,
            "i64" => Token::I64,
            "i128" => Token::I128,
            "u8" => Token::U8,
            "u16" => Token::U16,
            "u32" => Token::U32,
            "u64" => Token::U64,
            "u128" => Token::U128,

            // Floating point types
            "f32" => Token::F32,
            "f64" => Token::F64,
            "f80" => Token::F80,

            // Complex types
            "c32" => Token::C32,
            "c64" => Token::C64,
            "c80" => Token::C80,

            // Other types
            "bool" => Token::Bool,
            "str" => Token::Str,
            "char" => Token::Char,
            "void" => Token::Void,
            "Vec" => Token::Vec,
            "Optional" => Token::Optional,

            // Not a keyword, it's an identifier
            _ => Token::Identifier(identifier),
        };

        Ok(token)
    }

    /// True when the scanner is positioned at a comment start (`//` or `/*`).
    fn starts_comment(&self) -> bool {
        self.peek() == '/' && matches!(self.peek_next(), Some('/') | Some('*'))
    }

    /// Lex a comment starting at the current `/`. Returns the kind and the
    /// content text with the comment delimiters stripped.
    fn next_comment(&mut self) -> Result<(CommentKind, String), LexError> {
        let start_pos = self.position;
        self.advance(); // consume '/'
        let (kind, content_start) = match self.peek() {
            '/' => {
                self.advance(); // consume second '/'
                if self.peek() == '/' {
                    self.advance(); // third '/' -> doc comment
                    (CommentKind::DocLine, self.position)
                } else {
                    (CommentKind::Line, self.position)
                }
            }
            '*' => {
                self.advance(); // consume '*'
                if self.peek() == '*' {
                    self.advance(); // second '*' -> doc comment
                    (CommentKind::DocBlock, self.position)
                } else {
                    (CommentKind::Block, self.position)
                }
            }
            _ => unreachable!("starts_comment guarantees a comment start"),
        };

        match kind {
            CommentKind::Line | CommentKind::DocLine => {
                while !self.is_at_end() && self.peek() != '\n' {
                    self.advance();
                }
            }
            CommentKind::Block | CommentKind::DocBlock => {
                let mut depth = 1usize;
                let mut content_end = self.position;
                while !self.is_at_end() && depth > 0 {
                    let ch = self.advance();
                    if ch == '/' && !self.is_at_end() && self.peek() == '*' {
                        self.advance();
                        depth += 1;
                    } else if ch == '*' && !self.is_at_end() && self.peek() == '/' {
                        self.advance();
                        depth -= 1;
                        if depth == 0 {
                            // Content ends right before the closing `*/`.
                            content_end = self.position - 2;
                        }
                    }
                }
                if depth > 0 {
                    return Err(LexError::UnexpectedEof {
                        span: (start_pos, self.position),
                    });
                }
                let text = self.input[content_start..content_end].trim().to_string();
                return Ok((kind, text));
            }
        }

        let text = self.input[content_start..self.position].trim().to_string();
        Ok((kind, text))
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' | '\n' => {
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn advance(&mut self) -> char {
        let ch = *self.input.as_bytes().get(self.position).unwrap_or(&0) as char;
        self.position += 1;
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn advance_utf8_char(&mut self) -> char {
        let ch = self.input[self.position..].chars().next().unwrap_or('\0');
        self.position += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        ch
    }

    fn peek(&self) -> char {
        self.input
            .as_bytes()
            .get(self.position)
            .copied()
            .unwrap_or(0) as char
    }

    fn peek_next(&self) -> Option<char> {
        let bytes = self.input.as_bytes();
        bytes.get(self.position + 1).copied().map(|b| b as char)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            false
        } else {
            self.advance();
            true
        }
    }

    fn is_at_end(&self) -> bool {
        self.position >= self.input.len()
    }

    /// Parse exactly two hex digits into a byte value.
    fn parse_hex_byte(hi: char, lo: char) -> Option<u8> {
        let hi = Self::hex_digit(hi)?;
        let lo = Self::hex_digit(lo)?;
        Some(hi * 16 + lo)
    }

    /// Map a single hex digit char to its value (0-15).
    fn hex_digit(ch: char) -> Option<u8> {
        match ch {
            '0'..='9' => Some(ch as u8 - b'0'),
            'a'..='f' => Some(ch as u8 - b'a' + 10),
            'A'..='F' => Some(ch as u8 - b'A' + 10),
            _ => None,
        }
    }
}

impl Token {
    pub fn keyword_name(&self) -> Option<&'static str> {
        match self {
            Token::I8 => Some("i8"),
            Token::I16 => Some("i16"),
            Token::I32 => Some("i32"),
            Token::I64 => Some("i64"),
            Token::I128 => Some("i128"),
            Token::U8 => Some("u8"),
            Token::U16 => Some("u16"),
            Token::U32 => Some("u32"),
            Token::U64 => Some("u64"),
            Token::U128 => Some("u128"),
            Token::F32 => Some("f32"),
            Token::F64 => Some("f64"),
            Token::F80 => Some("f80"),
            Token::C32 => Some("c32"),
            Token::C64 => Some("c64"),
            Token::C80 => Some("c80"),
            Token::Bool => Some("bool"),
            Token::Str => Some("str"),
            Token::Char => Some("char"),
            Token::Void => Some("void"),
            Token::Static => Some("static"),
            Token::Volatile => Some("volatile"),
            Token::Vec => Some("Vec"),
            Token::Optional => Some("Optional"),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::IntLiteral(n) => write!(f, "{}", n),
            Token::FloatLiteral(n) => write!(f, "{}", n),
            Token::ComplexLiteral(r, i) => write!(f, "{}+{}i", r, i),
            Token::StringLiteral(s) => write!(f, "\"{}\"", s),
            Token::CharLiteral(c) => write!(f, "'{}'", c),
            Token::BoolLiteral(b) => write!(f, "{}", b),
            Token::Identifier(s) => write!(f, "{}", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LexError::UnexpectedChar { found, .. } => {
                write!(f, "unexpected character '{}'", found)
            }
            LexError::UnexpectedEof { .. } => {
                write!(f, "unexpected end of file")
            }
            LexError::InvalidNumber { message, .. } => {
                write!(f, "invalid number literal: {message}")
            }
            LexError::InvalidString { message, .. } => {
                write!(f, "invalid string literal: {message}")
            }
            LexError::InvalidChar { message, .. } => {
                write!(f, "invalid character literal: {message}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck::{Arbitrary, Gen, TestResult};
    use quickcheck_macros::quickcheck;

    // Generator for valid Silver identifiers
    #[derive(Debug, Clone)]
    struct ValidIdentifier(String);

    impl Arbitrary for ValidIdentifier {
        fn arbitrary(g: &mut Gen) -> Self {
            let first_char = *g
                .choose(&[
                    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p',
                    'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F',
                    'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
                    'W', 'X', 'Y', 'Z', '_',
                ])
                .unwrap();

            let mut identifier = String::new();
            identifier.push(first_char);

            let len = g.size() % 20; // Limit identifier length
            for _ in 0..len {
                let ch = *g
                    .choose(&[
                        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
                        'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
                        'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
                        'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7',
                        '8', '9', '_',
                    ])
                    .unwrap();
                identifier.push(ch);
            }

            ValidIdentifier(identifier)
        }
    }

    // Generator for valid string content (no quotes or invalid escapes)
    #[derive(Debug, Clone)]
    struct ValidStringContent(String);

    impl Arbitrary for ValidStringContent {
        fn arbitrary(g: &mut Gen) -> Self {
            let mut content = String::new();
            let len = g.size() % 50; // Limit string length

            for _ in 0..len {
                let ch = *g
                    .choose(&[
                        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
                        'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D',
                        'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
                        'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7',
                        '8', '9', ' ', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '+',
                        '=', '[', ']', '{', '}', '|', ';', ':', '<', '>', ',', '.', '?', '/',
                    ])
                    .unwrap();
                content.push(ch);
            }

            ValidStringContent(content)
        }
    }

    // Property test for lexer completeness
    #[quickcheck]
    fn prop_lexer_tokenization_completeness(tokens: Vec<String>) -> TestResult {
        // **Feature: silver-compiler-architecture, Property 1: Lexer tokenization completeness**
        // **Validates: Requirements 1.1**

        if tokens.is_empty() {
            return TestResult::discard();
        }

        // Generate source code from tokens
        let source = tokens.join(" ");

        // Skip if source is too large or contains problematic patterns
        if source.len() > 1000 || source.contains('\0') {
            return TestResult::discard();
        }

        // Tokenize the source
        let mut lexer = Lexer::new(source.clone());
        match lexer.tokenize() {
            Ok(result_tokens) => {
                // Property 1: Lexer should produce tokens for all valid input
                // The result should contain at least one token (EOF is always added)
                if result_tokens.is_empty() {
                    return TestResult::failed();
                }

                // Property 2: Last token should always be EOF
                if result_tokens.last() != Some(&Token::Eof) {
                    return TestResult::failed();
                }

                TestResult::passed()
            }
            Err(_) => {
                // For this property test, we're generating arbitrary strings, so lexing may fail
                // This is expected for invalid input
                TestResult::discard()
            }
        }
    }

    // Property test for numeric literal completeness
    #[quickcheck]
    fn prop_numeric_literal_completeness(int_val: u64) -> TestResult {
        // **Feature: silver-compiler-architecture, Property 1: Lexer tokenization completeness**
        // **Validates: Requirements 1.1**

        // Test positive integer literals (lexer doesn't handle negative signs as part of numbers)
        let int_source = int_val.to_string();
        let mut lexer = Lexer::new(int_source);
        match lexer.tokenize() {
            Ok(tokens) => {
                if tokens.len() < 2 || tokens[tokens.len() - 1] != Token::Eof {
                    return TestResult::failed();
                }
                match &tokens[0] {
                    Token::IntLiteral(parsed_val) => {
                        if *parsed_val != int_val as i128 {
                            return TestResult::failed();
                        }
                    }
                    _ => return TestResult::failed(),
                }
            }
            Err(_) => return TestResult::failed(),
        }

        TestResult::passed()
    }

    // Property test for string literal completeness
    #[quickcheck]
    fn prop_string_literal_completeness(content: ValidStringContent) -> TestResult {
        // **Feature: silver-compiler-architecture, Property 1: Lexer tokenization completeness**
        // **Validates: Requirements 1.1**

        let ValidStringContent(string_content) = content;
        let source = format!("\"{}\"", string_content);

        let mut lexer = Lexer::new(source);
        match lexer.tokenize() {
            Ok(tokens) => {
                if tokens.len() < 2 || tokens[tokens.len() - 1] != Token::Eof {
                    return TestResult::failed();
                }
                match &tokens[0] {
                    Token::StringLiteral(parsed_content) => {
                        if *parsed_content != string_content {
                            return TestResult::failed();
                        }
                    }
                    _ => return TestResult::failed(),
                }
                TestResult::passed()
            }
            Err(_) => TestResult::failed(),
        }
    }

    // Property test for identifier completeness
    #[quickcheck]
    fn prop_identifier_completeness(identifier: ValidIdentifier) -> TestResult {
        // **Feature: silver-compiler-architecture, Property 1: Lexer tokenization completeness**
        // **Validates: Requirements 1.1**

        let ValidIdentifier(id_string) = identifier;

        // Skip keywords as they should be tokenized as keywords, not identifiers
        let keywords = [
            "struct", "enum", "impl", "trait", "fn", "mut", "const", "static", "volatile", "if",
            "else", "while", "for", "break", "continue", "return", "defer", "import", "comptime",
            "cast", "move", "extern", "pub", "asm", "true", "false", "i8", "i16", "i32", "i64",
            "i128", "u8", "private", "u16", "u32", "u64", "u128", "f32", "f64", "f80", "c32",
            "c64", "c80", "bool", "str", "char", "void", "Vec", "Optional", "launch", "wait",
        ];

        if keywords.contains(&id_string.as_str()) {
            return TestResult::discard();
        }

        let mut lexer = Lexer::new(id_string.clone());
        match lexer.tokenize() {
            Ok(tokens) => {
                if tokens.len() < 2 || tokens[tokens.len() - 1] != Token::Eof {
                    return TestResult::failed();
                }
                match &tokens[0] {
                    Token::Identifier(parsed_id) => {
                        if *parsed_id != id_string {
                            return TestResult::failed();
                        }
                    }
                    _ => return TestResult::failed(),
                }
                TestResult::passed()
            }
            Err(_) => TestResult::failed(),
        }
    }

    // Unit tests for specific edge cases
    #[test]
    fn test_empty_input() {
        let mut lexer = Lexer::new("".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Eof]);
    }

    #[test]
    fn test_whitespace_only() {
        let mut lexer = Lexer::new("   \t\n\r  ".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::Eof]);
    }

    #[test]
    fn test_complex_numbers() {
        let mut lexer = Lexer::new("3.5i".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::ComplexLiteral(0.0, 3.5), Token::Eof]);

        let mut lexer = Lexer::new("42i".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::ComplexLiteral(0.0, 42.0), Token::Eof]);
    }

    #[test]
    fn test_escape_sequences() {
        let mut lexer = Lexer::new("\"hello\\nworld\"".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![Token::StringLiteral("hello\nworld".to_string()), Token::Eof]
        );

        let mut lexer = Lexer::new("'\\t'".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::CharLiteral('\t'), Token::Eof]);
    }

    #[test]
    fn test_all_keywords() {
        let keywords = [
            ("struct", Token::Struct),
            ("enum", Token::Enum),
            ("impl", Token::Impl),
            ("trait", Token::Trait),
            // (fn removed)
            ("mut", Token::Mut),
            ("const", Token::Const),
            ("static", Token::Static),
            ("volatile", Token::Volatile),
            ("if", Token::If),
            ("else", Token::Else),
            ("while", Token::While),
            ("for", Token::For),
            ("match", Token::Match),
            ("break", Token::Break),
            ("continue", Token::Continue),
            ("return", Token::Return),
            ("defer", Token::Defer),
            ("import", Token::Import),
            ("comptime", Token::Comptime),
            ("cast", Token::Cast),
            ("move", Token::Move),
            ("type", Token::Type),
            ("Self", Token::SelfType),
            ("extern", Token::Extern),
            // (pub removed)
            ("private", Token::Private),
            ("asm", Token::Asm),
            ("in", Token::In),
            ("static", Token::Static),
            ("volatile", Token::Volatile),
            ("true", Token::True),
            ("false", Token::False),
            ("launch", Token::Launch),
            ("wait", Token::Wait),
        ];

        for (keyword, expected_token) in &keywords {
            let mut lexer = Lexer::new(keyword.to_string());
            let tokens = lexer.tokenize().unwrap();
            assert_eq!(tokens, vec![expected_token.clone(), Token::Eof]);
        }
    }

    #[test]
    fn test_all_types() {
        let types = [
            ("i8", Token::I8),
            ("i16", Token::I16),
            ("i32", Token::I32),
            ("i64", Token::I64),
            ("i128", Token::I128),
            ("u8", Token::U8),
            ("u16", Token::U16),
            ("u32", Token::U32),
            ("u64", Token::U64),
            ("u128", Token::U128),
            ("f32", Token::F32),
            ("f64", Token::F64),
            ("f80", Token::F80),
            ("c32", Token::C32),
            ("c64", Token::C64),
            ("c80", Token::C80),
            ("bool", Token::Bool),
            ("str", Token::Str),
            ("char", Token::Char),
            ("void", Token::Void),
            ("Vec", Token::Vec),
            ("Optional", Token::Optional),
        ];

        for (type_str, expected_token) in &types {
            let mut lexer = Lexer::new(type_str.to_string());
            let tokens = lexer.tokenize().unwrap();
            assert_eq!(tokens, vec![expected_token.clone(), Token::Eof]);
        }
    }

    #[test]
    fn test_nested_block_comments() {
        let mut lexer = Lexer::new("/* outer /* inner */ outer */".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Comment {
                    kind: CommentKind::Block,
                    text: "outer /* inner */ outer".to_string(),
                },
                Token::Eof
            ],
            "nested block comments are captured as a single token"
        );
    }

    #[test]
    fn test_mixed_tokens() {
        let mut lexer = Lexer::new("i32 main() { return 42; }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let expected = vec![
            Token::I32,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::IntLiteral(42),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];
        assert_eq!(tokens, expected);
    }
    #[test]
    fn test_let_and_ref_are_identifiers() {
        let mut lexer = Lexer::new("i32 let = 1; i32 ref = 2;".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::I32,
                Token::Identifier("let".to_string()),
                Token::Assign,
                Token::IntLiteral(1),
                Token::Semicolon,
                Token::I32,
                Token::Identifier("ref".to_string()),
                Token::Assign,
                Token::IntLiteral(2),
                Token::Semicolon,
                Token::Eof,
            ]
        );
    }
}
