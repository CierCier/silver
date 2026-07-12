use agc::lexer;
use agc::lexer::Span;
use agc::parser::ast::{self, ItemKind};
use agc::parser::{FileImportResolverHook, Parser};
use agc::semantic::typeck::TypeChecker;
use agc::symbol_table::CompilerSymbolTable;
use rustc_hash::FxHashMap as HashMap;
use tower_lsp::lsp_types::*;

use crate::format::*;
use crate::util::*;
use crate::Backend;

impl Backend {
    pub(crate) async fn check_diagnostics(&self, uri: &Url, text: &str) {
        let tokens = match lexer::lex(text) {
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
                    (text.to_string(), HashMap::default(), HashMap::default(), HashMap::default()),
                );
                self.client
                    .publish_diagnostics(uri.clone(), diags, None)
                    .await;
                return;
            }
        };

        let mut parser = Parser::new(tokens);
        let (mut program, parse_errors) = parser.parse_program();

        let mut diagnostics: Vec<Diagnostic> = parse_errors
            .iter()
            .map(|e| Diagnostic {
                range: span_to_range(text, e.span()),
                severity: Some(DiagnosticSeverity::ERROR),
                message: match e {
                    agc::parser::ParseError::InvalidSyntax { message, .. } => {
                        message.clone()
                    }
                    _ => format!("{:?}", e),
                },
                ..Default::default()
            })
            .collect();

        // Resolve imports with per-file caching via FileImportResolverHook.
        let source_path = uri.to_file_path().ok();
        let base_dir = source_path.as_ref().and_then(|p| p.parent());
        let imported_modules = match FileImportResolverHook::with_cache(&self.loader, &self.file_cache)
            .lower_program_imports(&mut program, base_dir, source_path.as_deref())
        {
            Ok(result) => result.module_artifacts,
            Err(e) => {
                diagnostics.push(Diagnostic {
                    range: Range { start: Position { line: 0, character: 0 }, end: Position { line: 0, character: 0 } },
                    severity: Some(DiagnosticSeverity::ERROR),
                    message: format!("import error: {e}"),
                    ..Default::default()
                });
                Vec::new()
            }
        };
        // Collect definitions for go‑to‑definition.
        let defs = collect_definitions(&program, text.len());

        // Build struct size map for hover display.
        let struct_map: HashMap<String, usize> = {
            let mut m = HashMap::default();
            for item in &program.items {
                if let ItemKind::Struct(s) = &item.kind {
                    let packed = is_repr_packed(&item.attributes);
                    if let Some(size) = compute_struct_size(s, &m, packed) {
                        m.insert(s.name.name.clone(), size);
                    }
                }
            }
            m
        };
        // Build pre-formatted hover texts for every top-level item (and nested extern items).
        let hover_texts: HashMap<String, String> = program
            .items
            .iter()
            .flat_map(|item| {
                // Flatten extern blocks into their contained functions/variables.
                if let ItemKind::ExternBlock(block) = &item.kind {
                    let mut entries = Vec::new();
                    for f in &block.functions {
                        entries.push((f.name.name.clone(), format_extern_function_sig(f)));
                    }
                    for v in &block.variables {
                        let text = format!("extern type: {}", format_type(&v.var_type));
                        entries.push((v.name.name.clone(), text));
                    }
                    return entries;
                }
                // Flatten impl blocks into their contained methods.
                if let ItemKind::Impl(imp) = &item.kind {
                    let mut entries = Vec::new();
                    for method in &imp.items {
                        if let ast::ImplItemKind::Function(f) = method {
                            entries.push((
                                f.name.name.clone(),
                                format_impl_function_sig(f),
                            ));
                        }
                    }
                    return entries;
                }
                // Structs get formatted with attributes and size info.
                if let ItemKind::Struct(s) = &item.kind {
                    let text = format_struct_hover(s, &item.attributes, &s.name.name, &struct_map);
                    return vec![(s.name.name.clone(), text)];
                }
                let name = match &item.kind {
                    ItemKind::Function(f) => &f.name.name,
                    ItemKind::Enum(e) => &e.name.name,
                    ItemKind::GlobalVariable(v) => &v.name.name,
                    ItemKind::Trait(t) => &t.name.name,
                    ItemKind::TypeAlias(a) => &a.name.name,
                    ItemKind::ExternFunction(f) => &f.name.name,
                    ItemKind::ExternVariable(v) => &v.name.name,
                    ItemKind::Macro(m) => &m.name.name,
                    _ => return Vec::new(),
                };
                match format_item_hover(item) {
                    Some(text) => vec![(name.clone(), text)],
                    None => Vec::new(),
                }
            })
            .collect();

        // Type‑check and capture expression types for hover.
        let mut tc = TypeChecker::new().with_imported_modules(&imported_modules);
        let mut table = CompilerSymbolTable::new();
        let (type_errors, _monomorphs) = tc.check_program_with_table(&program, &mut table);
        let expr_types = std::mem::take(&mut tc.expr_types);

        for err in &type_errors {
            diagnostics.push(Diagnostic {
                range: span_to_range(text, &err.span),
                severity: Some(DiagnosticSeverity::ERROR),
                message: err.message.clone(),
                ..Default::default()
            });
        }

        self.cache
            .lock()
            .insert(uri.clone(), (text.to_string(), expr_types, defs, hover_texts));
        self.client
            .publish_diagnostics(uri.clone(), diagnostics, None)
            .await;
    }
}

/// Collect top‑level definition names and their spans (current file only).
pub(crate) fn collect_definitions(program: &ast::Program, source_len: usize) -> HashMap<String, Span> {
    let mut defs = HashMap::default();
    for item in &program.items {
        let (name, span) = match &item.kind {
            ItemKind::Import(_) => continue,
            ItemKind::Function(f) => (&f.name.name, &f.name.span),
            ItemKind::Struct(s) => (&s.name.name, &s.name.span),
            ItemKind::Enum(e) => (&e.name.name, &e.name.span),
            ItemKind::GlobalVariable(v) => (&v.name.name, &v.name.span),
            ItemKind::Trait(t) => (&t.name.name, &t.name.span),
            ItemKind::TypeAlias(a) => (&a.name.name, &a.name.span),
            ItemKind::ExternFunction(f) => (&f.name.name, &f.name.span),
            ItemKind::ExternVariable(v) => (&v.name.name, &v.name.span),
            ItemKind::Macro(m) => (&m.name.name, &m.name.span),
            ItemKind::ExternBlock(_) | ItemKind::Impl(_) => continue,
        };
        if span.start < source_len {
            defs.insert(name.clone(), span.clone());
        }
    }
    defs
}
