//! Inlay hints: param names at call sites + implicit move/copy at let/assign/return/call args.
//! Why: surface implicit Copy vs move for non-obvious ownership without explicit `move`.
use agc::symbol_index::{SymbolIndex, SymbolKind};
use tower_lsp_server::ls_types::*;

use crate::util::byte_to_position;

pub(crate) fn inlay_hints(analysis: &SymbolIndex) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    for site in &analysis.call_sites {
        let Some(callee_id) = site.callee else {
            continue;
        };
        let Some(callee) = analysis.symbols.get(callee_id) else {
            continue;
        };
        if callee.parameters.is_empty() {
            continue;
        }
        // Instance methods declare the receiver as their first parameter; a
        // call with one fewer argument omits it (static calls pass all).
        let skip_receiver =
            callee.kind == SymbolKind::Method && site.args.len() + 1 == callee.parameters.len();
        let params = if skip_receiver {
            &callee.parameters[1..]
        } else {
            &callee.parameters[..]
        };
        for (arg, param) in site.args.iter().zip(params.iter()) {
            if param.name == "_" || param.name.is_empty() {
                continue;
            }
            hints.push(InlayHint {
                position: byte_to_position(&analysis.text, arg.0),
                label: InlayHintLabel::String(format!("{}:", param.name)),
                kind: Some(InlayHintKind::PARAMETER),
                padding_left: Some(false),
                padding_right: Some(true),
                tooltip: None,
                text_edits: None,
                data: None,
            });
        }
    }
    // Implicit move at let/assign/return/call args (explicit `move` excluded upstream).
    for hint in &analysis.move_hints {
        if hint.kind == agc::symbol_index::MoveHintKind::Move {
            hints.push(InlayHint {
                position: byte_to_position(&analysis.text, hint.span.0),
                label: InlayHintLabel::String("move".to_string()),
                kind: Some(InlayHintKind::PARAMETER),
                padding_left: Some(false),
                padding_right: Some(true),
                tooltip: None,
                text_edits: None,
                data: None,
            });
        }
    }
    for symbol in &analysis.symbols {
        let Some(ty) = symbol.inferred_type.as_deref() else {
            continue;
        };
        // Only show type inlay hint for local variables that do NOT have an explicit type written in the source code.
        if symbol.kind != SymbolKind::Local
            || symbol.span.file != analysis.buffer_file
            || symbol.has_explicit_type
        {
            continue;
        }
        hints.push(InlayHint {
            position: byte_to_position(&analysis.text, symbol.span.start),
            label: InlayHintLabel::String(ty.to_string()),
            kind: Some(InlayHintKind::TYPE),
            padding_left: Some(false),
            padding_right: Some(true),
            tooltip: None,
            text_edits: None,
            data: None,
        });
    }
    hints
}
