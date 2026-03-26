use std::fmt;

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
    Fn,
    Let,
    Mut,
    Const,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Return,
    Import,
    Comptime,
    Cast,
    Move,
    Ref,
    Extern,
    Pub,
    Private,
    Asm,
    True,
    False,

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
    Arrow,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    LeftShift,
    RightShift,
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
    Colon,
    DoubleColon,
    Question,
    At,
    Hash,

    // Special
    Identifier(String),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone)]
pub struct LexErrorCompat {
    pub kind: LexError,
    pub span: Span,
}

// Public interface function expected by main.rs
pub fn lex(input: &str) -> Result<Vec<LexToken>, Vec<LexErrorCompat>> {
    let mut lexer = Lexer::new(input.to_string());
    match lexer.tokenize_with_spans() {
        Ok(tokens) => Ok(tokens),
        Err(error) => Err(vec![LexErrorCompat {
            kind: error.clone(),
            span: match error {
                LexError::UnexpectedChar { span, .. } => Span {
                    start: span.0,
                    end: span.1,
                },
                LexError::UnexpectedEof { span } => Span {
                    start: span.0,
                    end: span.1,
                },
                LexError::InvalidNumber { span, .. } => Span {
                    start: span.0,
                    end: span.1,
                },
                LexError::InvalidString { span, .. } => Span {
                    start: span.0,
                    end: span.1,
                },
                LexError::InvalidChar { span, .. } => Span {
                    start: span.0,
                    end: span.1,
                },
            },
        }]),
    }
}

#[derive(Debug, Clone)]
pub struct Lexer {
    input: String,
    position: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            position: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace_and_comments()?;
            if self.is_at_end() {
                break;
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
            self.skip_whitespace_and_comments()?;
            if self.is_at_end() {
                break;
            }

            let start = self.position;
            let token = self.next_token()?;
            let end = self.position;
            let text = self.input.get(start..end).unwrap_or("").to_string();

            tokens.push(LexToken {
                kind: token,
                span: Span { start, end },
                text,
            });
        }

        let end = self.position;
        tokens.push(LexToken {
            kind: Token::Eof,
            span: Span { start: end, end },
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
            '.' => Ok(Token::Dot),
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
                } else if self.match_char('>') {
                    Ok(Token::Arrow)
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
                } else if self.match_char('<') {
                    Ok(Token::LeftShift)
                } else {
                    Ok(Token::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    Ok(Token::GreaterEqual)
                } else if self.match_char('>') {
                    Ok(Token::RightShift)
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
        let mut value = String::new();

        while !self.is_at_end() && self.peek() != '"' {
            let ch = self.advance();
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
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '"' => value.push('"'),
                    '\'' => value.push('\''),
                    '0' => value.push('\0'),
                    _ => {
                        return Err(LexError::InvalidString {
                            span: (start_pos, self.position),
                            message: format!("Invalid escape sequence: \\{}", escaped),
                        });
                    }
                }
            } else {
                value.push(ch);
            }
        }

        if self.is_at_end() {
            return Err(LexError::InvalidString {
                span: (start_pos, self.position),
                message: "Unterminated string literal".to_string(),
            });
        }

        self.advance(); // consume closing quote
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
        let mut number_str = String::new();
        number_str.push(first_digit);

        // Collect digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            number_str.push(self.advance());
        }

        // Check for decimal point
        if !self.is_at_end()
            && self.peek() == '.'
            && self.peek_next().map_or(false, |c| c.is_ascii_digit())
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
            "fn" => Token::Fn,
            "let" => Token::Let,
            "mut" => Token::Mut,
            "const" => Token::Const,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "return" => Token::Return,
            "import" => Token::Import,
            "comptime" => Token::Comptime,
            "cast" => Token::Cast,
            "move" => Token::Move,
            "ref" => Token::Ref,
            "extern" => Token::Extern,
            "pub" => Token::Pub,
            "private" => Token::Private,
            "asm" => Token::Asm,
            "true" => Token::True,
            "false" => Token::False,

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

    fn skip_whitespace_and_comments(&mut self) -> Result<(), LexError> {
        loop {
            self.skip_whitespace();
            if self.is_at_end() {
                break;
            }

            if self.peek() == '/' {
                if self.peek_next() == Some('/') {
                    // Line comment
                    self.advance(); // consume first '/'
                    self.advance(); // consume second '/'
                    self.skip_line_comment();
                } else if self.peek_next() == Some('*') {
                    // Block comment
                    self.advance(); // consume '/'
                    self.advance(); // consume '*'
                    self.skip_block_comment()?;
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        Ok(())
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    self.advance();
                }
                _ => break,
            }
        }
    }

    fn skip_line_comment(&mut self) {
        while !self.is_at_end() && self.peek() != '\n' {
            self.advance();
        }
    }

    fn skip_block_comment(&mut self) -> Result<(), LexError> {
        let start_pos = self.position - 2;
        let mut depth = 1;

        while !self.is_at_end() && depth > 0 {
            let ch = self.advance();
            if ch == '/' && !self.is_at_end() && self.peek() == '*' {
                self.advance();
                depth += 1;
            } else if ch == '*' && !self.is_at_end() && self.peek() == '/' {
                self.advance();
                depth -= 1;
            }
        }

        if depth > 0 {
            Err(LexError::UnexpectedEof {
                span: (start_pos, self.position),
            })
        } else {
            Ok(())
        }
    }

    fn advance(&mut self) -> char {
        let ch = self.input.chars().nth(self.position).unwrap_or('\0');
        self.position += 1;
        self.column += 1;
        ch
    }

    fn peek(&self) -> char {
        self.input.chars().nth(self.position).unwrap_or('\0')
    }

    fn peek_next(&self) -> Option<char> {
        self.input.chars().nth(self.position + 1)
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
            LexError::UnexpectedChar { found, span } => {
                write!(
                    f,
                    "Unexpected character '{}' at position {}-{}",
                    found, span.0, span.1
                )
            }
            LexError::UnexpectedEof { span } => {
                write!(
                    f,
                    "Unexpected end of file at position {}-{}",
                    span.0, span.1
                )
            }
            LexError::InvalidNumber { span, message } => {
                write!(
                    f,
                    "Invalid number at position {}-{}: {}",
                    span.0, span.1, message
                )
            }
            LexError::InvalidString { span, message } => {
                write!(
                    f,
                    "Invalid string at position {}-{}: {}",
                    span.0, span.1, message
                )
            }
            LexError::InvalidChar { span, message } => {
                write!(
                    f,
                    "Invalid character at position {}-{}: {}",
                    span.0, span.1, message
                )
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
            "struct", "enum", "impl", "trait", "fn", "let", "mut", "const", "if", "else", "while",
            "for", "break", "continue", "return", "import", "comptime", "cast", "move", "ref",
            "extern", "pub", "asm", "true", "false", "i8", "i16", "i32", "i64", "i128", "u8",
            "private", "u16", "u32", "u64", "u128", "f32", "f64", "f80", "c32", "c64", "c80",
            "bool", "str", "char", "void", "Vec", "Optional",
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
        let mut lexer = Lexer::new("3.14i".to_string());
        let tokens = lexer.tokenize().unwrap();
        assert_eq!(tokens, vec![Token::ComplexLiteral(0.0, 3.14), Token::Eof]);

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
            ("fn", Token::Fn),
            ("let", Token::Let),
            ("mut", Token::Mut),
            ("const", Token::Const),
            ("if", Token::If),
            ("else", Token::Else),
            ("while", Token::While),
            ("for", Token::For),
            ("break", Token::Break),
            ("continue", Token::Continue),
            ("return", Token::Return),
            ("import", Token::Import),
            ("comptime", Token::Comptime),
            ("cast", Token::Cast),
            ("move", Token::Move),
            ("ref", Token::Ref),
            ("extern", Token::Extern),
            ("pub", Token::Pub),
            ("private", Token::Private),
            ("asm", Token::Asm),
            ("true", Token::True),
            ("false", Token::False),
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
        assert_eq!(tokens, vec![Token::Eof]);
    }

    #[test]
    fn test_mixed_tokens() {
        let mut lexer = Lexer::new("fn main() -> i32 { return 42; }".to_string());
        let tokens = lexer.tokenize().unwrap();
        let expected = vec![
            Token::Fn,
            Token::Identifier("main".to_string()),
            Token::LeftParen,
            Token::RightParen,
            Token::Arrow,
            Token::I32,
            Token::LeftBrace,
            Token::Return,
            Token::IntLiteral(42),
            Token::Semicolon,
            Token::RightBrace,
            Token::Eof,
        ];
        assert_eq!(tokens, expected);
    }
}
