use parking_lot::Mutex;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use crate::lexer;
use crate::module_artifact::ModuleArtifact;
use crate::module_loader::{
    ModuleLoader, ResolvedSourceImportKind, import_path_to_string, validate_import_conflicts,
};
use crate::parser::Parser;
use crate::parser::ast;

/// Cache keyed by file path: (mtime_nanos, fully-parsed program).
pub type FileItemCache = HashMap<PathBuf, (u128, ast::Program)>;

#[derive(Debug, Default)]
pub struct ImportLoweringResult {
    pub module_artifacts: Vec<ModuleArtifact>,
    pub module_dependencies: Vec<String>,
    pub transitive_module_deps: Vec<String>,
}

pub struct FileImportResolverHook<'a> {
    loader: &'a ModuleLoader,
    seen_modules: HashSet<String>,
    seen_files: HashSet<PathBuf>,
    module_imports: Vec<(String, ModuleArtifact)>,
    file_cache: Option<&'a Mutex<FileItemCache>>,
}

// (ImportAliasPlan removed — import guards and aliases not supported)
impl<'a> FileImportResolverHook<'a> {
    pub fn new(loader: &'a ModuleLoader) -> Self {
        Self {
            loader,
            seen_modules: HashSet::default(),
            seen_files: HashSet::default(),
            module_imports: Vec::new(),
            file_cache: None,
        }
    }

    pub fn with_cache(loader: &'a ModuleLoader, file_cache: &'a Mutex<FileItemCache>) -> Self {
        Self {
            loader,
            seen_modules: HashSet::default(),
            seen_files: HashSet::default(),
            module_imports: Vec::new(),
            file_cache: Some(file_cache),
        }
    }

    pub fn lower_program_imports(
        mut self,
        program: &mut ast::Program,
        base_dir: Option<&Path>,
        root_source: Option<&Path>,
    ) -> Result<ImportLoweringResult, String> {
        if let Some(root) = root_source {
            let _ = self.mark_file_seen(root);
        }

        // Default import: every program gets `std.sys.entry`, which supplies
        // the `_start` entry point. Silver never links libc/crt0, so without
        // it no executable would have an entry symbol. `seen_modules` dedupes
        // this against an explicit `import std.sys.entry;` or `import std.sys;`.
        program.items.insert(
            0,
            ast::Item {
                kind: ast::ItemKind::Import(ast::ImportItem {
                    path: vec![
                        ast::Identifier {
                            name: "std".to_string(),
                            span: lexer::Span::default(),
                        },
                        ast::Identifier {
                            name: "sys".to_string(),
                            span: lexer::Span::default(),
                        },
                        ast::Identifier {
                            name: "entry".to_string(),
                            span: lexer::Span::default(),
                        },
                    ],
                }),
                span: lexer::Span::default(),
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            },
        );

        self.lower_program_recursive(program, base_dir)?;
        validate_import_conflicts(
            self.module_imports
                .iter()
                .map(|(module_path, module)| (module_path.as_str(), module)),
        )?;
        let direct_deps: HashSet<String> = self
            .module_imports
            .iter()
            .map(|(module_path, _)| module_path.clone())
            .collect();
        let transitive: Vec<String> = self
            .seen_modules
            .iter()
            .filter(|p| !direct_deps.contains(p.as_str()))
            .cloned()
            .collect();

        Ok(ImportLoweringResult {
            module_dependencies: direct_deps.into_iter().collect(),
            transitive_module_deps: transitive,
            module_artifacts: self.module_imports.into_iter().map(|(_, m)| m).collect(),
        })
    }
    fn lower_program_recursive(
        &mut self,
        program: &mut ast::Program,
        base_dir: Option<&Path>,
    ) -> Result<(), String> {
        let mut original_items = Vec::new();
        let mut lowered_program_attributes = std::mem::take(&mut program.attributes);
        // (alias_plan removed)

        for item in std::mem::take(&mut program.items) {
            let ast::Item {
                kind,
                span,
                visibility,
                attributes,
            } = item;

            let ast::ItemKind::Import(import_item) = kind else {
                original_items.push(ast::Item {
                    kind,
                    span,
                    visibility,
                    attributes,
                });
                continue;
            };

            let module_path = import_path_to_string(&import_item.path);
            // Every module path is resolved at most once. A re-import — even
            // while the module is still being lowered further up the stack —
            // is a no-op: the merged program is a single symbol space, so the
            // in-progress module's items will already be included when its
            // own lowering completes.
            if !self.seen_modules.insert(module_path.clone()) {
                if crate::profiler::verbose() {
                    crate::profiler::skip_phase(&format!(
                        "import {module_path} (already imported)"
                    ));
                }
                continue;
            }

            let resolved = self
                .loader
                .find_source_import(&import_item.path, base_dir)
                .ok_or_else(|| format!("import `{module_path}` could not be resolved"))?;
            match resolved.kind {
                ResolvedSourceImportKind::File => {
                    if !self.mark_file_seen(&resolved.source_path) {
                        if crate::profiler::verbose() {
                            crate::profiler::skip_phase(&format!(
                                "import {} (already imported)",
                                import_label(&resolved.source_path)
                            ));
                        }
                        continue;
                    }

                    let import_phase = format!("import {}", import_label(&resolved.source_path));
                    if crate::profiler::verbose() {
                        crate::profiler::begin_phase(&import_phase);
                    }

                    // Check file cache for pre-parsed program (mtime-based).
                    let cache_hit = self.file_cache.and_then(|cache| {
                        let mtime = std::fs::metadata(&resolved.source_path)
                            .and_then(|m| m.modified())
                            .ok()
                            .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                            .map(|d| d.as_nanos())
                            .unwrap_or(0);
                        let guard = cache.lock();
                        guard.get(&resolved.source_path).and_then(|(cm, prog)| {
                            if *cm == mtime {
                                Some(prog.clone())
                            } else {
                                None
                            }
                        })
                    });

                    let mut imported_program = match cache_hit {
                        Some(prog) => prog,
                        None => {
                            let prog = parse_program_from_file(&resolved.source_path)?;
                            // Populate cache.
                            if let Some(cache) = &self.file_cache {
                                let mtime = std::fs::metadata(&resolved.source_path)
                                    .and_then(|m| m.modified())
                                    .ok()
                                    .and_then(|t| t.duration_since(SystemTime::UNIX_EPOCH).ok())
                                    .map(|d| d.as_nanos())
                                    .unwrap_or(0);
                                cache
                                    .lock()
                                    .insert(resolved.source_path.clone(), (mtime, prog.clone()));
                            }
                            prog
                        }
                    };
                    self.lower_program_recursive(
                        &mut imported_program,
                        resolved.source_path.parent(),
                    )?;
                    lowered_program_attributes.extend(imported_program.attributes);
                    program.comments.extend(imported_program.comments);
                    original_items.extend(imported_program.items);
                    if crate::profiler::verbose() {
                        crate::profiler::end_phase(&import_phase);
                    }
                }
                ResolvedSourceImportKind::Module => {
                    let artifact = ModuleArtifact::from_path(&resolved.source_path)?;
                    self.module_imports
                        .push((resolved.module_path.clone(), artifact));
                }
            }
        }
        program.items = original_items;
        program.attributes = lowered_program_attributes;
        Ok(())
    }

    fn mark_file_seen(&mut self, path: &Path) -> bool {
        if self.seen_files.contains(path) {
            return false;
        }
        let stable = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        self.seen_files.insert(stable)
    }
}

fn parse_program_from_file(path: &Path) -> Result<ast::Program, String> {
    let label = import_label(path);
    let verbose = crate::profiler::verbose();
    if verbose {
        crate::profiler::begin_phase(&format!("read {label}"));
    }
    let src = std::fs::read_to_string(path)
        .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    if verbose {
        crate::profiler::end_phase(&format!("read {label}"));
    }
    // Register the imported file so its diagnostic spans resolve to this file.
    let file_id = crate::lexer::register_source(&path.display().to_string(), &src);
    if verbose {
        crate::profiler::begin_phase(&format!("lex {label}"));
    }
    let tokens = lexer::lex_with_source(&src, file_id)
        .map_err(|e| format!("lexer errors in {}: {e:?}", path.display()))?;
    if verbose {
        crate::profiler::end_phase(&format!("lex {label}"));
    }
    if verbose {
        crate::profiler::begin_phase(&format!("parse {label}"));
    }
    let mut parser = Parser::new_with_source(tokens, path.display().to_string());
    let (program, errors) = parser.parse_program();
    if verbose {
        crate::profiler::end_phase(&format!("parse {label}"));
    }
    if errors.is_empty() {
        return Ok(program);
    }
    Err(crate::diagnostics::render(
        *errors[0].span(),
        &errors[0].format_with_help(),
        crate::diagnostics::Severity::Error,
    )
    .to_string())
}

/// Convert an imported file path to its Silver module form, e.g.
/// `/…/std/io/scanner.ag` -> `std.io.scanner`. Falls back to the last two
/// path components for non-std files.
fn import_label(path: &Path) -> String {
    let s = path.display().to_string();
    let comps: Vec<&str> = s.split('/').collect();
    let start = comps
        .iter()
        .position(|c| *c == "std")
        .unwrap_or_else(|| comps.len().saturating_sub(2));
    let mut parts: Vec<&str> = comps[start..].to_vec();
    if let Some(last) = parts.last_mut() {
        *last = last.strip_suffix(".ag").unwrap_or(last);
    }
    parts.join(".")
}
