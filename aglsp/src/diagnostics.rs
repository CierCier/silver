use agc::lexer;
use agc::parser::{FileImportResolverHook, Parser};
use agc::semantic::typeck::TypeChecker;
use agc::symbol_index::{SymbolIndex, analyze};
use agc::symbol_table::CompilerSymbolTable;
use rustc_hash::FxHashMap as HashMap;
use tower_lsp_server::ls_types::*;

use crate::Backend;
use crate::util::*;
impl Backend {
    pub(crate) async fn check_diagnostics(&self, uri: &Uri, text: &str) {
        // Register the buffer with the source registry and lex with its file
        // id (mirroring the compiler driver) so `Span.file` distinguishes the
        // buffer from inlined imported files.
        let source_path = uri.to_file_path();
        let file_id = source_path
            .as_ref()
            .map(|p| lexer::register_source(&p.display().to_string(), text))
            .unwrap_or(0);
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
                self.client
                    .publish_diagnostics(uri.clone(), diags, None)
                    .await;
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

        // Resolve imports with per-file caching via FileImportResolverHook.
        let base_dir = source_path.as_ref().and_then(|p| p.parent());
        let imported_modules =
            match FileImportResolverHook::with_cache(&self.loader, &self.file_cache)
                .lower_program_imports(&mut program, base_dir, source_path.as_deref())
            {
                Ok(result) => result.module_artifacts,
                Err(e) => {
                    diagnostics.push(Diagnostic {
                        range: Range {
                            start: Position {
                                line: 0,
                                character: 0,
                            },
                            end: Position {
                                line: 0,
                                character: 0,
                            },
                        },
                        severity: Some(DiagnosticSeverity::ERROR),
                        message: format!("import error: {e}"),
                        ..Default::default()
                    });
                    Vec::new()
                }
            };

        // Type-check (runs the semantic analyzer internally: duplicate
        // symbols, unknown identifiers/types/traits, scoping) and capture
        // expression types for hover.
        let mut tc = TypeChecker::new().with_imported_modules(&imported_modules);
        let mut table = CompilerSymbolTable::new();
        let (type_errors, _monomorphs) = tc.check_program_with_table(&program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);

        let mut by_uri: HashMap<Uri, Vec<Diagnostic>> = HashMap::default();
        by_uri.insert(uri.clone(), diagnostics);

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
        for (target_uri, diags) in by_uri {
            self.client
                .publish_diagnostics(target_uri, diags, None)
                .await;
        }
    }
}
