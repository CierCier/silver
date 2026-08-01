//! Semantic token encoding (`textDocument/semanticTokens/full`).
//!
//! The lexer token stream supplies keywords, operators, literals and
//! comments; the analysis supplies the classification of identifiers
//! (functions, types, variables, ...) with spans. Results are emitted in
//! the LSP delta encoding.

use agc::lexer::Token;
use rustc_hash::FxHashMap as HashMap;
use tower_lsp_server::ls_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens, SemanticTokensLegend,
    SemanticTokensServerCapabilities,
};

use crate::analysis::{Analysis, OccurrenceKind};
use crate::util::byte_to_position;

pub(crate) const TOKEN_TYPES: &[&str] = &[
    "namespace",
    "type",
    "struct",
    "enum",
    "enumMember",
    "typeParameter",
    "parameter",
    "variable",
    "property",
    "function",
    "method",
    "macro",
    "keyword",
    "comment",
    "string",
    "number",
    "operator",
];

pub(crate) const TOKEN_MODIFIERS: &[&str] = &[
    "declaration",
    "definition",
    "readonly",
    "static",
    "documentation",
];

pub(crate) fn server_capability() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(
        tower_lsp_server::ls_types::SemanticTokensOptions {
            legend: legend(),
            full: Some(tower_lsp_server::ls_types::SemanticTokensFullOptions::Bool(
                true,
            )),
            range: None,
            ..Default::default()
        },
    )
}

pub(crate) fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: TOKEN_TYPES
            .iter()
            .map(|s| SemanticTokenType::from(*s))
            .collect(),
        token_modifiers: TOKEN_MODIFIERS
            .iter()
            .map(|s| SemanticTokenModifier::from(*s))
            .collect(),
    }
}

fn type_index(name: &str) -> u32 {
    TOKEN_TYPES.iter().position(|t| *t == name).unwrap_or(0) as u32
}

fn modifier_bit(name: &str) -> u32 {
    1u32 << TOKEN_MODIFIERS.iter().position(|m| *m == name).unwrap_or(0)
}

pub(crate) fn semantic_tokens(analysis: &Analysis) -> SemanticTokens {
    // Map identifier token spans to their classified occurrences.
    let mut occurrences: HashMap<(usize, usize), &crate::analysis::Occurrence> = HashMap::default();
    for occ in &analysis.occurrences {
        occurrences.insert((occ.span.start, occ.span.end), occ);
    }

    let mut data: Vec<SemanticToken> = Vec::new();
    let mut prev_line: u32 = 0;
    let mut prev_char: u32 = 0;

    for token in &analysis.tokens {
        let Some((kind, modifiers)) = classify(&token.span, &token.kind, &occurrences) else {
            continue;
        };
        let start = byte_to_position(&analysis.text, token.span.start);
        let end = byte_to_position(&analysis.text, token.span.end);
        let length = end.character.saturating_sub(start.character);
        if length == 0 {
            continue;
        }
        let (delta_line, delta_char) = if start.line == prev_line {
            (0, start.character.saturating_sub(prev_char))
        } else {
            (start.line - prev_line, start.character)
        };
        data.push(SemanticToken {
            delta_line,
            delta_start: delta_char,
            length,
            token_type: kind,
            token_modifiers_bitset: modifiers,
        });
        prev_line = start.line;
        prev_char = start.character;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}

fn classify(
    span: &agc::lexer::Span,
    token: &Token,
    occurrences: &HashMap<(usize, usize), &crate::analysis::Occurrence>,
) -> Option<(u32, u32)> {
    match token {
        // Keywords
        Token::Struct
        | Token::Enum
        | Token::Impl
        | Token::Trait
        | Token::Let
        | Token::Mut
        | Token::Const
        | Token::Static
        | Token::Volatile
        | Token::If
        | Token::Else
        | Token::While
        | Token::For
        | Token::Match
        | Token::Break
        | Token::Continue
        | Token::Return
        | Token::Type
        | Token::SelfType
        | Token::Defer
        | Token::Import
        | Token::Comptime
        | Token::Cast
        | Token::Move
        | Token::Ref
        | Token::Extern
        | Token::Private
        | Token::Asm
        | Token::In
        | Token::Macro
        | Token::True
        | Token::False => Some((type_index("keyword"), 0)),

        // Builtin type tokens
        Token::I8
        | Token::I16
        | Token::I32
        | Token::I64
        | Token::I128
        | Token::U8
        | Token::U16
        | Token::U32
        | Token::U64
        | Token::U128
        | Token::F32
        | Token::F64
        | Token::F80
        | Token::C32
        | Token::C64
        | Token::C80
        | Token::Bool
        | Token::Str
        | Token::Char
        | Token::Void
        | Token::Vec
        | Token::Optional => Some((type_index("type"), 0)),

        Token::IntLiteral(_) | Token::FloatLiteral(_) | Token::ComplexLiteral(_, _) => {
            Some((type_index("number"), 0))
        }
        Token::StringLiteral(_) | Token::CharLiteral(_) => Some((type_index("string"), 0)),
        Token::BoolLiteral(_) => Some((type_index("keyword"), 0)),
        Token::Comment { .. } => Some((type_index("comment"), 0)),

        Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Equal
        | Token::NotEqual
        | Token::Less
        | Token::Greater
        | Token::LessEqual
        | Token::GreaterEqual
        | Token::And
        | Token::Or
        | Token::Not
        | Token::Assign
        | Token::Arrow
        | Token::PlusAssign
        | Token::MinusAssign
        | Token::StarAssign
        | Token::SlashAssign
        | Token::PercentAssign
        | Token::BitwiseAnd
        | Token::BitwiseOr
        | Token::BitwiseXor
        | Token::BitwiseNot
        | Token::Increment
        | Token::Decrement => Some((type_index("operator"), 0)),

        Token::Identifier(_) => {
            let occ = occurrences.get(&(span.start, span.end))?;
            let kind = match occ.kind {
                OccurrenceKind::Namespace => type_index("namespace"),
                OccurrenceKind::Type => type_index("type"),
                OccurrenceKind::Struct => type_index("struct"),
                OccurrenceKind::Enum => type_index("enum"),
                OccurrenceKind::Trait => type_index("type"),
                OccurrenceKind::TypeAlias => type_index("type"),
                OccurrenceKind::EnumMember => type_index("enumMember"),
                OccurrenceKind::TypeParam => type_index("typeParameter"),
                OccurrenceKind::Parameter => type_index("parameter"),
                OccurrenceKind::Variable | OccurrenceKind::Const => type_index("variable"),
                OccurrenceKind::Property => type_index("property"),
                OccurrenceKind::Function => type_index("function"),
                OccurrenceKind::Method => type_index("method"),
                OccurrenceKind::Macro => type_index("macro"),
            };
            let mut modifiers = 0u32;
            if occ.is_definition {
                modifiers |= modifier_bit("declaration");
                modifiers |= modifier_bit("definition");
            }
            if occ.readonly {
                modifiers |= modifier_bit("readonly");
            }
            if occ.is_static {
                modifiers |= modifier_bit("static");
            }
            if occ.documented {
                modifiers |= modifier_bit("documentation");
            }
            Some((kind, modifiers))
        }
        _ => None,
    }
}
