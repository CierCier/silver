// Parse error types and handling

use crate::lexer::{Span, Token};
use std::fmt;

/// Parse errors with source location information
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    UnexpectedToken {
        expected: Vec<String>,
        found: Token,
        span: Span,
    },
    UnexpectedEof {
        expected: Vec<String>,
        span: Span,
    },
    InvalidSyntax {
        message: String,
        span: Span,
    },
}

impl ParseError {
    /// Create a helpful error message with suggestions
    pub fn with_suggestion(&self, suggestion: String) -> String {
        format!("{}\nSuggestion: {}", self, suggestion)
    }

    /// Get the span of the error
    pub fn span(&self) -> &Span {
        match self {
            ParseError::UnexpectedToken { span, .. } => span,
            ParseError::UnexpectedEof { span, .. } => span,
            ParseError::InvalidSyntax { span, .. } => span,
        }
    }

    /// Generate helpful suggestions based on the error context
    pub fn suggest_fix(&self) -> Option<String> {
        match self {
            ParseError::UnexpectedToken {
                expected, found, ..
            } => {
                // Provide context-specific suggestions
                if expected.contains(&"';'".to_string()) {
                    Some("Did you forget a semicolon at the end of the statement?".to_string())
                } else if expected.contains(&"'{'".to_string()) {
                    Some(
                        "Expected a block starting with '{'. Did you forget to open a block?"
                            .to_string(),
                    )
                } else if expected.contains(&"'}'".to_string()) {
                    Some("Expected '}' to close the block. Check for matching braces.".to_string())
                } else if expected.contains(&"identifier".to_string()) {
                    match found {
                        Token::IntLiteral(_) => Some("Identifiers cannot start with a number. Try using a letter or underscore first.".to_string()),
                        // Check if it's a keyword token
<<<<<<< HEAD
                        Token::Struct | Token::Enum | Token::Impl | Token::Trait | Token::Fn | 
                        Token::Let | Token::Mut | Token::Const | Token::If | Token::Else | 
                        Token::While | Token::For | Token::Break | Token::Continue | Token::Return |
                        Token::Import | Token::Comptime | Token::Cast | Token::Move | Token::Ref | 
                        Token::Extern | Token::Pub | Token::Private | Token::True | Token::False => 
=======
                        Token::Struct | Token::Enum | Token::Impl | Token::Trait | Token::Fn |
                        Token::Let | Token::Mut | Token::Const | Token::If | Token::Else |
                        Token::While | Token::For | Token::Break | Token::Continue | Token::Return |
                        Token::Import | Token::Comptime | Token::Cast | Token::Move | Token::Ref |
                        Token::Extern | Token::Pub | Token::Private | Token::True | Token::False =>
>>>>>>> cc823df (shift to LL3)
                            Some("This is a reserved keyword and cannot be used as an identifier.".to_string()),
                        _ => Some("Expected an identifier (variable or function name).".to_string()),
                    }
                } else if expected.contains(&"')'".to_string()) {
                    Some(
                        "Expected ')' to close the parentheses. Check for matching parentheses."
                            .to_string(),
                    )
                } else if expected.contains(&"']'".to_string()) {
                    Some(
                        "Expected ']' to close the bracket. Check for matching brackets."
                            .to_string(),
                    )
                } else if expected.len() == 1 {
                    Some(format!("Expected {}.", expected[0]))
                } else if expected.len() > 1 {
                    Some(format!("Expected one of: {}.", expected.join(", ")))
                } else {
                    None
                }
            }
            ParseError::UnexpectedEof { expected, .. } => {
                if expected.contains(&"'}'".to_string()) {
                    Some("Reached end of file while looking for '}'. You may have an unclosed block.".to_string())
                } else if expected.contains(&"')'".to_string()) {
                    Some("Reached end of file while looking for ')'. You may have unclosed parentheses.".to_string())
                } else if expected.contains(&"']'".to_string()) {
                    Some("Reached end of file while looking for ']'. You may have unclosed brackets.".to_string())
                } else {
                    Some(format!(
                        "Unexpected end of file. Expected one of: {}.",
                        expected.join(", ")
                    ))
                }
            }
            ParseError::InvalidSyntax { message, .. } => {
                // Provide suggestions based on common syntax errors
                if message.contains("type") {
                    Some("Check that the type annotation is correct. Valid types include i32, f64, bool, etc.".to_string())
                } else if message.contains("expression") {
                    Some(
                        "Check that the expression is well-formed and all operators have operands."
                            .to_string(),
                    )
                } else {
                    None
                }
            }
        }
    }

    /// Format error with helpful context and suggestions
    pub fn format_with_help(&self) -> String {
        let base_message = self.to_string();
        if let Some(suggestion) = self.suggest_fix() {
            format!("{}\n\nHelp: {}", base_message, suggestion)
        } else {
            base_message
        }
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                expected,
                found,
                span,
            } => {
                write!(
                    f,
                    "Unexpected token {:?} at {:?}, expected one of: {}",
                    found,
                    span,
                    expected.join(", ")
                )
            }
            ParseError::UnexpectedEof { expected, span } => {
                write!(
                    f,
                    "Unexpected end of file at {:?}, expected one of: {}",
                    span,
                    expected.join(", ")
                )
            }
            ParseError::InvalidSyntax { message, span } => {
                write!(f, "Invalid syntax at {:?}: {}", span, message)
            }
        }
    }
}

impl std::error::Error for ParseError {}

/// Result type for parser operations
pub type ParseResult<T> = Result<T, ParseError>;
