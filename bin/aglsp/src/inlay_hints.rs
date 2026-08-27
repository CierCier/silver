//! Call-site parameter-name inlay hints.
//!
//! For every recorded call site with a resolved callee, a hint is placed
//! before each argument showing the callee's parameter name:
//!
//!   String s = @format("{}: {}", name, age);
//!                          ^^^^^ ^^^
//!                          name:  age:

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
    for symbol in &analysis.symbols {
        let Some(ty) = symbol.inferred_type.as_deref() else {
            continue;
        };
        if symbol.kind != SymbolKind::Local {
            continue;
        }
        hints.push(InlayHint {
            position: byte_to_position(&analysis.text, symbol.span.start),
            label: InlayHintLabel::String(format!(": {ty}")),
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

#[cfg(test)]
mod tests {
    use super::*;
    use agc::lexer;
    use agc::semantic::typeck::TypeChecker;
    use agc::symbol_index::analyze;
    use agc::symbol_table::CompilerSymbolTable;

    fn analyze_src(source: &str) -> SymbolIndex {
        let file_id = lexer::register_source("/tmp/test_buffer.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        assert!(!graph.has_errors(), "parse errors: {:?}", graph.errors());
        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (_, _) = tc.check_program_with_table(&mut program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);
        analyze(&program, source, &tokens, expr_types, file_id)
    }

    #[test]
    fn parameter_name_hints_for_free_and_method_calls() {
        let analysis = analyze_src(
            "struct Counter { i64 v; }\n\
             impl Counter { i64 add(Counter* self, i64 amount) { self.v = self.v + amount; return self.v; } }\n\
             i64 scale(i64 value, i64 factor) { return value * factor; }\n\
             i32 main() { Counter c; i64 a = scale(5, 3); i64 b = c.add(7); return 0; }",
        );
        let hints = inlay_hints(&analysis);
        // scale(5, 3): value:, factor:  |  c.add(7): amount: (receiver skipped)
        assert_eq!(hints.len(), 3, "hints: {hints:?}");
        let labels: Vec<&str> = hints
            .iter()
            .map(|h| match &h.label {
                InlayHintLabel::String(s) => s.as_str(),
                _ => "",
            })
            .collect();
        assert!(labels.contains(&"value:"), "{labels:?}");
        assert!(labels.contains(&"factor:"), "{labels:?}");
        assert!(labels.contains(&"amount:"), "{labels:?}");
        assert!(
            hints
                .iter()
                .all(|h| h.kind == Some(InlayHintKind::PARAMETER))
        );
    }

    #[test]
    fn no_hints_for_unknown_callee() {
        // `unknown_fn` fails to resolve; and zero-argument calls record no
        // sites — both produce no hints.
        let analysis = analyze_src("i32 main() { unknown_fn(1, 2); return 0; }");
        assert!(inlay_hints(&analysis).is_empty());
    }

    #[test]
    fn no_hints_for_zero_arg_call() {
        let analysis = analyze_src("void no_args() {}\n i32 main() { no_args(); return 0; }");
        assert!(inlay_hints(&analysis).is_empty());
    }
}
