use agc::lexer;
use agc::parser::{FileImportResolverHook, Parser};
use agc::semantic::typeck::TypeChecker;
use agc::symbol_index::{SymbolIndex, analyze};
use agc::symbol_table::CompilerSymbolTable;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use tower_lsp_server::ls_types::*;

use crate::Backend;
use crate::util::*;
impl Backend {
    pub(crate) async fn check_diagnostics(&self, uri: &Uri, text: &str) {
        // Register the buffer with the source registry and lex with its file
        // id (mirroring the compiler driver) so `Span.file` distinguishes the
        // buffer from inlined imported files.
        let source_path = uri.to_file_path();
        let file_key = source_path.as_ref().map_or_else(
            || format!("untitled:{uri:?}"),
            |path| path.display().to_string(),
        );
        let file_id = lexer::register_source(&file_key, text);
        let tokens = match lexer::lex_with_source(text, file_id) {
            Ok(t) => t,
            Err(errors) => {
                let diags: Vec<Diagnostic> = errors
                    .iter()
                    .map(|e| Diagnostic {
                        range: span_to_range(text, &e.span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("{:?}", e.kind),
                        ..Default::default()
                    })
                    .collect();
                self.cache.lock().insert(
                    uri.clone(),
                    SymbolIndex {
                        text: text.to_string(),
                        symbols: Vec::new(),
                        occurrences: Vec::new(),
                        call_sites: Vec::new(),
                        expr_types: Default::default(),
                        import_paths: Vec::new(),
                        tokens: Vec::new(),
                        foreign_files: Default::default(),
                    },
                );
                let current_uris: HashSet<Uri> = [uri.clone()].into_iter().collect();
                let previous_uris = self.diagnostic_uris.lock().clone();
                self.client
                    .publish_diagnostics(uri.clone(), diags, None)
                    .await;
                for stale_uri in previous_uris.difference(&current_uris) {
                    self.client
                        .publish_diagnostics(stale_uri.clone(), Vec::new(), None)
                        .await;
                }
                *self.diagnostic_uris.lock() = current_uris;
                return;
            }
        };

        let mut parser = Parser::new(tokens.clone());
        let (mut program, parse_errors) = parser.parse_program();

        let mut diagnostics: Vec<Diagnostic> = parse_errors
            .iter()
            .map(|e| Diagnostic {
                range: span_to_range(text, e.span()),
                severity: Some(DiagnosticSeverity::ERROR),
                message: match e {
                    agc::parser::ParseError::InvalidSyntax { message, .. } => message.clone(),
                    _ => format!("{:?}", e),
                },
                ..Default::default()
            })
            .collect();

        let base_dir = source_path.as_ref().and_then(|p| p.parent());
        // Keep the user's import span before lowering mutates the item list.
        let import_span = program.items.iter().find_map(|item| {
            matches!(item.kind, agc::parser::ast::ItemKind::Import(_))
                .then_some(item.span)
                .filter(|span| span.file == file_id)
        });
        let imported_modules =
            match FileImportResolverHook::with_cache(&self.loader, &self.file_cache)
                .lower_program_imports(&mut program, base_dir, source_path.as_deref())
            {
                Ok(result) => result.module_artifacts,
                Err(e) => {
                    diagnostics.push(Diagnostic {
                        range: import_span
                            .as_ref()
                            .map(|span| span_to_range(text, span))
                            .unwrap_or_default(),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("import error: {e}"),
                        ..Default::default()
                    });
                    Vec::new()
                }
            };

        // Mirror the compiler driver: gate #[cfg(...)] items and fold
        // @cfg(...) expressions BEFORE semantic analysis/type checking —
        // otherwise @cfg calls reach the macro registry as "unknown builtin
        // macro '@cfg'". Default cfg set (no --cfg flags) plus derived
        // debug/arch/os cfgs matches `agc` with no flags.
        let mut cfg_set = agc::cfg::CfgSet::parse(&[]);
        agc::cfg::add_derived_cfgs(&mut cfg_set, None, None);
        for error in agc::cfg::gate_items(&mut program, &cfg_set) {
            diagnostics.push(Diagnostic {
                range: span_to_range(text, &error.span),
                severity: Some(DiagnosticSeverity::ERROR),
                message: error.message.clone(),
                ..Default::default()
            });
        }
        agc::semantic::cfg_hook::fold_and_prune(&mut program, &cfg_set);

        // Type-check (runs the semantic analyzer internally: duplicate
        // symbols, unknown identifiers/types/traits, scoping) and capture
        // expression types for hover.
        let mut tc = TypeChecker::new().with_imported_modules(&imported_modules);
        let mut table = CompilerSymbolTable::new();
        let (type_errors, _monomorphs) = tc.check_program_with_table(&program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);

        let mut by_uri: HashMap<Uri, Vec<Diagnostic>> = HashMap::default();
        by_uri.insert(uri.clone(), diagnostics);

        // Lint only the active buffer. Imported files may be user sources too,
        // but their warnings must not be published as diagnostics for this URI.
        let warning_config = agc::semantic::linter::WarningConfig::default();
        for warning in agc::semantic::linter::lint_program(&program, &warning_config) {
            if warning.span.file == file_id || warning.span.file == 0 {
                by_uri.entry(uri.clone()).or_default().push(Diagnostic {
                    range: span_to_range(text, &warning.span),
                    severity: Some(DiagnosticSeverity::WARNING),
                    message: warning.message,
                    ..Default::default()
                });
            }
        }
        for err in &type_errors {
            if err.span.file == file_id || err.span.file == 0 {
                by_uri.entry(uri.clone()).or_default().push(Diagnostic {
                    range: span_to_range(text, &err.span),
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: err.message.clone(),
                    ..Default::default()
                });
            } else if let Some(source_file) = lexer::source_file(err.span.file) {
                if let Some(imported_uri) = Uri::from_file_path(&source_file.path) {
                    by_uri.entry(imported_uri).or_default().push(Diagnostic {
                        range: span_to_range(&source_file.text, &err.span),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: err.message.clone(),
                        ..Default::default()
                    });
                } else {
                    by_uri.entry(uri.clone()).or_default().push(Diagnostic {
                        range: Range::default(),
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("{}: {}", source_file.path, err.message),
                        ..Default::default()
                    });
                }
            }
        }

        let analysis = analyze(&program, text, &tokens, expr_types, file_id);
        self.cache.lock().insert(uri.clone(), analysis);

        let current_uris: HashSet<Uri> = by_uri.keys().cloned().collect();
        let previous_uris = self.diagnostic_uris.lock().clone();
        for (target_uri, diags) in by_uri {
            self.client
                .publish_diagnostics(target_uri, diags, None)
                .await;
        }
        for stale_uri in previous_uris.difference(&current_uris) {
            self.client
                .publish_diagnostics(stale_uri.clone(), Vec::new(), None)
                .await;
        }
        *self.diagnostic_uris.lock() = current_uris;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use agc::parser::Parser;
    use agc::symbol_index::analyze;

    /// The frontend pipeline used by `check_diagnostics` for a buffer with no
    /// imports: lex → parse → cfg gate/fold → type-check. Returns the type
    /// errors so tests can assert the LSP surfaces exactly what the compiler
    /// would.
    fn frontend_type_errors(source: &str) -> Vec<String> {
        let file_id = lexer::register_source("/tmp/lsp_cfg_test.ag", source);
        let tokens = lexer::lex_with_source(source, file_id).expect("lex failed");
        let mut parser = Parser::new(tokens.clone());
        let (mut program, parse_errors) = parser.parse_program();
        assert!(parse_errors.is_empty(), "parse errors: {parse_errors:?}");

        let mut cfg_set = agc::cfg::CfgSet::parse(&[]);
        agc::cfg::add_derived_cfgs(&mut cfg_set, None, None);
        let cfg_errors = agc::cfg::gate_items(&mut program, &cfg_set);
        assert!(cfg_errors.is_empty(), "cfg errors: {}", cfg_errors.len());
        agc::semantic::cfg_hook::fold_and_prune(&mut program, &cfg_set);

        let mut tc = TypeChecker::new();
        let mut table = CompilerSymbolTable::new();
        let (type_errors, _) = tc.check_program_with_table(&program, &mut table);
        type_errors.into_iter().map(|e| e.message).collect()
    }

    #[test]
    fn cfg_expression_is_not_reported_as_unknown_macro() {
        // @cfg must fold (debug → true) before typeck; a persistent
        // "unknown builtin macro '@cfg'" here is the regression.
        let errors = frontend_type_errors(
            "i32 main() { i32 x = 1; if (@cfg(debug)) { x = x + 1; } return 0; }",
        );
        assert!(
            !errors
                .iter()
                .any(|e| e.contains("unknown builtin macro '@cfg'")),
            "got cfg macro errors: {errors:?}"
        );
    }

    #[test]
    fn cfg_item_gating_runs_before_symbol_registration() {
        // The gated-out function must not produce "unknown function" errors
        // at its call site (it is removed before analysis).
        let errors = frontend_type_errors(
            "i32 main() { i32 x = 0; return x; }\n#[cfg(missing_user_key)]\ni32 gated() { return 1; }",
        );
        assert!(
            !errors.iter().any(|e| e.contains("unknown")),
            "gated item leaked into analysis: {errors:?}"
        );
    }
}
