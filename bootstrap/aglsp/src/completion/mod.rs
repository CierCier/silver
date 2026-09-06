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

#[cfg(test)]
mod tests {
    use super::*;
    use agc::lexer;
    use agc::module_loader::ModuleLoader;
    use agc::parser::FileImportResolverHook;
    use agc::symbol_index::analyze;

    #[test]
    fn aliased_selective_import_appears_in_completions() {
        let source = "import std.io.file { println as pln };\nfn main() {\n    pl;\n}";
        let path = std::path::Path::new("/tmp/lsp_sel_import_test.ag");
        let file_id = lexer::register_source(&path.display().to_string(), source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        assert!(!graph.has_errors(), "parse errors: {:?}", graph.errors());

        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .to_path_buf();
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(repo_root);
        let hook = FileImportResolverHook::new(&loader);
        let lowered = hook.lower_program_imports(&mut program, path.parent(), Some(path));
        assert!(lowered.is_ok(), "lowering failed: {:?}", lowered.err());

        let expr_types = Default::default();
        let analysis = analyze(&program, source, &tokens, expr_types, file_id);

        let offset = source.rfind("pl").unwrap() + 2;
        let items = completion(&analysis, offset);
        assert!(
            items.iter().any(|item| item.label == "pln"),
            "alias 'pln' missing from completions: {:?}",
            items.iter().map(|i| i.label.clone()).collect::<Vec<_>>()
        );
    }

    #[test]
    fn macro_builtins_completion_at_at_sign() {
        let source = "i32 main() {\n    @pr;\n}";
        let path = std::path::Path::new("/tmp/lsp_macro_test.ag");
        let file_id = lexer::register_source(&path.display().to_string(), source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind("@pr").unwrap() + 3;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"@println".to_string()), "got {labels:?}");
        assert!(labels.contains(&"@print".to_string()), "got {labels:?}");
    }

    #[test]
    fn member_completion_partial_and_static() {
        let source = r#"
        struct User {
            i32 id;
            str name;
        }
        impl User {
            User make(str name) {
                User u = { .id = 1, .name = name };
                return u;
            }
            i32 get_id(User* self) {
                return self.id;
            }
        }
        i32 main() {
            User u = User.make("Alice");
            u.na;
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_member_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let mut program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let mut tc = agc::semantic::typeck::TypeChecker::new();
        let mut table = agc::symbol_table::CompilerSymbolTable::new();
        let (_, _) = tc.check_program_with_table(&mut program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);
        let analysis = analyze(&program, source, &tokens, expr_types, file_id);

        let offset = source.rfind("u.na").unwrap() + 4;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"name".to_string()), "expected 'name' in {labels:?}");

        let static_offset = source.rfind("User.").unwrap() + 5;
        let static_items = completion(&analysis, static_offset);
        let static_labels: Vec<String> = static_items.iter().map(|i| i.label.clone()).collect();
        assert!(
            static_labels.contains(&"make".to_string()),
            "expected 'make' in {static_labels:?}"
        );
    }

    #[test]
    fn struct_literal_designated_initializer_completion() {
        let source = r#"
        struct User {
            i32 id;
            str name;
        }
        i32 main() {
            User u = { .id = 1, .name = "Alice" };
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_struct_init_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind(".name").unwrap() + 1;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(
            labels.contains(&".id".to_string()) && labels.contains(&".name".to_string()),
            "expected struct fields in {labels:?}"
        );
    }

    #[test]
    fn member_completion_on_self_and_explicit_local() {
        let source = r#"
        struct Point {
            i32 x;
            i32 y;
        }
        impl Point {
            i32 magnitude(Point* self) {
                self.x;
                return 0;
            }
        }
        i32 main() {
            Point pt;
            pt.y;
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_self_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let self_offset = source.rfind("self.x").unwrap() + 5;
        let self_items = completion(&analysis, self_offset);
        let self_labels: Vec<String> = self_items.iter().map(|i| i.label.clone()).collect();
        assert!(self_labels.contains(&"x".to_string()), "got {self_labels:?}");
        assert!(self_labels.contains(&"y".to_string()), "got {self_labels:?}");

        let pt_offset = source.rfind("pt.y").unwrap() + 3;
        let pt_items = completion(&analysis, pt_offset);
        let pt_labels: Vec<String> = pt_items.iter().map(|i| i.label.clone()).collect();
        assert!(pt_labels.contains(&"x".to_string()), "got {pt_labels:?}");
        assert!(pt_labels.contains(&"y".to_string()), "got {pt_labels:?}");
    }

    #[test]
    fn type_position_completion_suggests_types_and_generic_snippets() {
        let source = "struct Point { i32 x; }\ni32 main() {\n    let \n}";
        let file_id = lexer::register_source("/tmp/lsp_type_pos_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind("let ").unwrap() + 4;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"i32".to_string()), "got {labels:?}");
        assert!(labels.contains(&"Point".to_string()), "got {labels:?}");
        assert!(labels.contains(&"Vec<T>".to_string()), "got {labels:?}");
        assert!(!labels.contains(&"while".to_string()), "type pos had control flow: {labels:?}");
    }

    #[test]
    fn match_pattern_completion_suggests_enum_arms() {
        let source = r#"
        enum Status {
            Pending,
            Active(i32),
            Failed,
        }
        i32 main() {
            Status s = Status.Pending;
            match s {
                
            }
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_match_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind("match s {").unwrap() + 10;
        let items = completion(&analysis, offset);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.iter().any(|l| l.contains("Fill all 3 match arms")), "got {labels:?}");
        assert!(labels.contains(&"Pending".to_string()), "got {labels:?}");
        assert!(labels.contains(&"Active".to_string()), "got {labels:?}");
        assert!(labels.contains(&"Failed".to_string()), "got {labels:?}");
    }

    #[test]
    fn top_level_context_suggests_declaration_snippets() {
        let source = "str";
        let file_id = lexer::register_source("/tmp/lsp_top_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let items = completion(&analysis, 3);
        let labels: Vec<String> = items.iter().map(|i| i.label.clone()).collect();
        assert!(labels.contains(&"struct".to_string()), "got {labels:?}");
        assert!(labels.contains(&"str".to_string()), "got {labels:?}");
        assert!(!labels.contains(&"break".to_string()), "top-level leaked break: {labels:?}");
        assert!(!labels.contains(&"defer".to_string()), "top-level leaked defer: {labels:?}");
    }

    #[test]
    fn statement_context_expands_named_parameters() {
        let source = r#"
        i32 compute(i32 width, i32 height) {
            return width * height;
        }
        i32 main() {
            comp;
            return 0;
        }
        "#;
        let file_id = lexer::register_source("/tmp/lsp_named_params_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let graph = agc::grammar::parse_ag(source);
        let program = agc::grammar::lower_source_graph(&graph, file_id as usize);
        let analysis = analyze(&program, source, &tokens, Default::default(), file_id);

        let offset = source.rfind("comp").unwrap() + 4;
        let items = completion(&analysis, offset);
        let compute_item = items.iter().find(|i| i.label == "compute").expect("compute not found");
        assert_eq!(
            compute_item.insert_text.as_deref(),
            Some("compute(${1:width}, ${2:height})$0")
        );
    }
}

