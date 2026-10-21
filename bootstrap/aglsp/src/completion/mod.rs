//! Syntax-aware completion and signature help engine.

pub(crate) mod context;
pub(crate) mod import;
pub(crate) mod items;
pub(crate) mod match_arms;
pub(crate) mod member;
pub(crate) mod snippets;
pub(crate) mod struct_init;

use agc::lexer::Token;
use agc::symbol_index::SymbolIndex;
use tower_lsp_server::ls_types::*;

use crate::completion::context::{SyntaxContext, determine_context};
use crate::doc;
use crate::util::find_occurrence;

pub(crate) fn completion(analysis: &SymbolIndex, offset: usize) -> Vec<CompletionItem> {
    let ctx = determine_context(analysis, offset);
    let items = match ctx {
        SyntaxContext::ImportPath {
            segments,
            prefix,
            after_dot,
        } => import::complete_import_path(analysis, &segments, &prefix, after_dot),

        SyntaxContext::SelectiveImport {
            module_path,
            prefix,
        } => import::complete_selective_import(analysis, &module_path, &prefix),

        SyntaxContext::MemberAccess {
            receiver_text,
            member_prefix,
            is_double_colon,
            recv_span,
        } => member::complete_member(
            analysis,
            &receiver_text,
            &member_prefix,
            is_double_colon,
            recv_span,
        )
        .unwrap_or_else(|| items::complete_statement_or_expr(analysis, offset, &member_prefix, false)),

        SyntaxContext::StructInit {
            struct_name,
            field_prefix,
            existing_fields,
        } => struct_init::complete_struct_init(
            analysis,
            &struct_name,
            &field_prefix,
            &existing_fields,
        )
        .unwrap_or_else(|| items::complete_statement_or_expr(analysis, offset, &field_prefix, false)),

        SyntaxContext::MatchArms {
            match_expr,
            arm_prefix,
            existing_arms,
        } => match_arms::complete_match_arms(
            analysis,
            &match_expr,
            &arm_prefix,
            &existing_arms,
        )
        .unwrap_or_else(|| items::complete_statement_or_expr(analysis, offset, &arm_prefix, false)),

        SyntaxContext::TypePosition { prefix } => items::complete_type_position(analysis, &prefix),

        SyntaxContext::MacroBuiltin { prefix } => snippets::macro_completions(&prefix),

        SyntaxContext::TopLevel { prefix } => items::complete_top_level(analysis, &prefix),

        SyntaxContext::StatementOrExpr { prefix, in_loop } => {
            items::complete_statement_or_expr(analysis, offset, &prefix, in_loop)
        }
    };

    sort_dedupe(items)
}

pub(crate) fn signature_help(analysis: &SymbolIndex, offset: usize) -> Option<SignatureHelp> {
    let tokens = &analysis.tokens;
    let mut paren_idx: Option<usize> = None;
    for (i, t) in tokens.iter().enumerate() {
        if t.span.start >= offset {
            break;
        }
        if matches!(t.kind, Token::LeftParen) {
            paren_idx = Some(i);
        }
    }
    let mut idx = paren_idx?;

    let target = loop {
        let prev = tokens.get(idx.wrapping_sub(1))?;
        match &prev.kind {
            Token::DoubleColon => {
                idx -= 1;
            }
            Token::Identifier(_) => break prev,
            Token::RightParen => {
                let mut depth = 0;
                let mut j = idx;
                loop {
                    if j == 0 {
                        return None;
                    }
                    j -= 1;
                    match &tokens[j].kind {
                        Token::RightParen => depth += 1,
                        Token::LeftParen => {
                            depth -= 1;
                            if depth == 0 {
                                idx = j;
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => return None,
        }
    };

    let occ = find_occurrence(target.span.start, &analysis.occurrences)?;
    let sym = analysis.symbols.get(occ.symbol?)?;
    if sym.parameters.is_empty() {
        return None;
    }

    let signature = SignatureInformation {
        label: sym.signature.clone(),
        documentation: sym.doc.as_deref().map(|d| {
            Documentation::MarkupContent(MarkupContent {
                kind: MarkupKind::Markdown,
                value: doc::doc_to_markdown(d),
            })
        }),
        parameters: Some(
            sym.parameters
                .iter()
                .map(|p| ParameterInformation {
                    label: ParameterLabel::Simple(format!("{}: {}", p.name, p.type_str)),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    };

    let open_end = tokens[idx].span.end;
    let mut commas: u32 = 0;
    let mut depth = 0;
    for t in tokens {
        if t.span.end <= open_end {
            continue;
        }
        if t.span.start >= offset {
            break;
        }
        match &t.kind {
            Token::LeftParen => depth += 1,
            Token::RightParen => {
                if depth == 0 {
                    break;
                }
                depth -= 1;
            }
            Token::Comma if depth == 0 => commas += 1,
            _ => {}
        }
    }

    let receiver_offset = if sym.kind == agc::symbol_index::SymbolKind::Method && !sym.is_static {
        1
    } else {
        0
    };

    let active = commas
        .saturating_add(receiver_offset)
        .min((sym.parameters.len() as u32).saturating_sub(1))
        .saturating_sub(receiver_offset);

    Some(SignatureHelp {
        signatures: vec![signature],
        active_signature: Some(0),
        active_parameter: Some(active),
    })
}

fn sort_dedupe(mut items: Vec<CompletionItem>) -> Vec<CompletionItem> {
    items.sort_by(|a, b| {
        let ka = a.sort_text.as_deref().unwrap_or("2");
        let kb = b.sort_text.as_deref().unwrap_or("2");
        ka.cmp(kb).then_with(|| a.label.cmp(&b.label))
    });

    let mut out: Vec<CompletionItem> = Vec::new();
    let mut seen: Vec<(String, Option<String>, Option<CompletionItemKind>)> = Vec::new();

    for item in items {
        if seen.iter().any(|(label, detail, kind)| {
            label == &item.label && detail == &item.detail && kind == &item.kind
        }) {
            continue;
        }
        seen.push((item.label.clone(), item.detail.clone(), item.kind));
        out.push(item);
    }
    out
}
