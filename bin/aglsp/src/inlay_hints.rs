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
    // Implicit move/copy at let/assign/return/call args (explicit `move` excluded upstream).
    for hint in &analysis.move_hints {
        let label = match hint.kind {
            agc::symbol_index::MoveHintKind::Move => "move",
            agc::symbol_index::MoveHintKind::Copy => "copy",
        };
        hints.push(InlayHint {
            position: byte_to_position(&analysis.text, hint.span.0),
            label: InlayHintLabel::String(label.to_string()),
            kind: Some(InlayHintKind::PARAMETER),
            padding_left: Some(false),
            padding_right: Some(true),
            tooltip: None,
            text_edits: None,
            data: None,
        });
    }
    for symbol in &analysis.symbols {
        let Some(ty) = symbol.inferred_type.as_deref() else {
            continue;
        };
        if symbol.kind != SymbolKind::Local || symbol.span.file != analysis.buffer_file {
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
             impl Counter {\n\
                 Counter create(i64 initial_count) { Counter c = { .v = initial_count }; return c; }\n\
                 i64 add(Counter* self, i64 amount) { self.v = self.v + amount; return self.v; }\n\
             }\n\
             i64 scale(i64 value, i64 factor) { return value * factor; }\n\
             i32 main() { Counter c = Counter.create(10); i64 a = scale(5, 3); i64 b = c.add(7); return 0; }",
        );
        let hints = inlay_hints(&analysis);
        // Counter.create(10): initial_count: | scale(5, 3): value:, factor: | c.add(7): amount: (receiver skipped)
        let param_labels: Vec<&str> = hints
            .iter()
            .filter_map(|h| match &h.label {
                InlayHintLabel::String(s) if s.ends_with(':') => Some(s.as_str()),
                _ => None,
            })
            .collect();
        assert_eq!(param_labels.len(), 4, "param hints: {hints:?}");
        assert!(param_labels.contains(&"initial_count:"), "{param_labels:?}");
        assert!(param_labels.contains(&"value:"), "{param_labels:?}");
        assert!(param_labels.contains(&"factor:"), "{param_labels:?}");
        assert!(param_labels.contains(&"amount:"), "{param_labels:?}");
        assert!(param_labels.iter().all(|_| {
            hints
                .iter()
                .any(|h| h.kind == Some(InlayHintKind::PARAMETER))
        }));
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

    #[test]
    fn implicit_move_and_copy_hints() {
        let analysis = analyze_src(
            "String from_str(String* self, str s) { return String.from_str(s); }\n\
             i32 main() {\n\
             String a = String.from_str(\"hello\");\n\
             String b = a;\n\
             i64 x = 5;\n\
             i64 y = x;\n\
             String c = move a;\n\
             return 0;\n\
             }",
        );
        let hints = inlay_hints(&analysis);
        let move_labels: Vec<&str> = hints
            .iter()
            .filter_map(|h| match &h.label {
                InlayHintLabel::String(s) if s == "move" => Some(s.as_str()),
                _ => None,
            })
            .collect();
        let copy_labels: Vec<&str> = hints
            .iter()
            .filter_map(|h| match &h.label {
                InlayHintLabel::String(s) if s == "copy" => Some(s.as_str()),
                _ => None,
            })
            .collect();
        // b = a (String) -> move, y = x (i64) -> copy
        // c = move a is explicit -> no implicit hint for that `a`
        assert!(
            move_labels.len() >= 1,
            "expected at least one move hint, got {hints:?}"
        );
        assert!(
            copy_labels.len() >= 1,
            "expected at least one copy hint, got {hints:?}"
        );
    }

    #[test]
    fn no_spurious_hints_for_serialized_structs() {
        let src = r#"
        #[serialize(json, yaml)]
        struct TaggedRecord {
            i32 id "json:id db:record_id";
            str title "json:title yaml:Title";
            bool active "json:active";
        }

        i32 main() {
            TaggedRecord rec = { .id = 1, .title = "Silver Manual", .active = true };
            @json(rec);
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/test_struct_attrs.ag", src);
        let tokens = lexer::lex_with_source(src, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(src);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        agc::semantic::serialize::synthesize_serialization_for_program(&mut program);

        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (_, _) = tc.check_program_with_table(&mut program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);
        let analysis = analyze(&program, src, &tokens, expr_types, file_id);
        let hints = inlay_hints(&analysis);

        // There should be NO parameter hints matching "name:", "value:", "expected:", "out:"
        // in struct declarations or comments.
        for hint in &hints {
            if let InlayHintLabel::String(label) = &hint.label {
                assert!(
                    !["name:", "value:", "expected:", "out:", "other:"].contains(&label.as_str()),
                    "found bogus synthesized hint: {label} at {:?}",
                    hint.position
                );
            }
        }
    }
}
