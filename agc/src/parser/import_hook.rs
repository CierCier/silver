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
    /// Whether the default `std.sys.entry` import (the `_start` entry point)
    /// is injected. Libraries emitted as modules have no entry point of
    /// their own and must not define the runtime; disable for --emit=module.
    include_entry_import: bool,
    /// Selective imports awaiting alias materialization after the traversal
    /// (module path string + the selected names). Captured BEFORE module
    /// dedup: a module already inlined transitively still needs its selected
    /// aliases bound for this import statement.
    pending_selections: Vec<(String, Vec<ast::ImportedName>)>,
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
            include_entry_import: true,
            pending_selections: Vec::new(),
        }
    }

    pub fn with_cache(loader: &'a ModuleLoader, file_cache: &'a Mutex<FileItemCache>) -> Self {
        Self {
            loader,
            seen_modules: HashSet::default(),
            seen_files: HashSet::default(),
            module_imports: Vec::new(),
            file_cache: Some(file_cache),
            include_entry_import: true,
            pending_selections: Vec::new(),
        }
    }

    /// Disable the automatic `std.sys.entry` import (used when emitting a
    /// library module, which has no entry point).
    pub fn with_entry_import(mut self, include: bool) -> Self {
        self.include_entry_import = include;
        self
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
        if self.include_entry_import {
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
                    selection: None,
                    }),
                    span: lexer::Span::default(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
            );
        }

        // Auto-import: bare enum constructors (`Some`/`None` for Optional,
        // `Ok`/`Err` for Result) resolve via typeck's expected-type inference
        // but need the enum types registered. Inject the matching std module
        // when the source uses those bare names without an explicit import.
        // `seen_modules` dedupes against explicit `import std.optional;` /
        // `import std.result;`.
        for (ctor, module_path) in [
            ("Some", vec!["std", "optional"]),
            ("None", vec!["std", "optional"]),
            ("Ok", vec!["std", "result"]),
            ("Err", vec!["std", "result"]),
        ] {
            if uses_bare_constructor(&program.items, ctor) {
                let module_key = module_path.join(".");
                if !self.seen_modules.contains(&module_key) {
                    program.items.insert(
                        0,
                        ast::Item {
                            kind: ast::ItemKind::Import(ast::ImportItem {
                                path: module_path
                                    .into_iter()
                                    .map(|seg| ast::Identifier {
                                        name: seg.to_string(),
                                        span: lexer::Span::default(),
                                    })
                                    .collect(),
                                selection: None,
                            }),
                            span: lexer::Span::default(),
                            visibility: ast::Visibility::Private,
                            attributes: Vec::new(),
                        },
                    );
                }
            }
        }

        self.lower_program_recursive(program, base_dir)?;
        apply_all_pending_selections(
            std::mem::take(&mut self.pending_selections),
            self.module_imports.as_mut_slice(),
            program,
        );
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
            // Selective imports are recorded before the dedup check: a
            // module already inlined transitively still needs its selected
            // names aliased for THIS import statement.
            if let Some(selection) = &import_item.selection {
                self.pending_selections
                    .push((module_path.clone(), selection.clone()));
            }
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
                    // Check on-disk module cache (templates, concurrency intrinsics, and mutual-import tests are source-inlined)
                    let p_str = resolved.source_path.to_str().unwrap_or("");
                    let is_inlined_module = p_str.contains("std/")
                        && !p_str.ends_with("std/atomic.ag")
                        && !p_str.ends_with("std/ops.ag");
                    if !is_inlined_module {
                        if let Some(cached) = self.loader.get_cached_module(&resolved.source_path, &module_path) {
                            if let Ok(artifact) = ModuleArtifact::from_path(&cached.agm_path) {
                                if crate::profiler::verbose() {
                                    crate::profiler::skip_phase(&format!(
                                        "import {module_path} (cache hit: {})",
                                        cached.key.hash_hex
                                    ));
                                }
                                self.module_imports.push((resolved.module_path.clone(), artifact));
                                continue;
                            }
                        }
                    }

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
/// Apply a selective import's aliases to a module ARTIFACT: for each
/// `name as alias` selection, duplicate the matching export under the local
/// name with `link_name` pinned to the real symbol — a pure compile-time
/// rename (calls through the alias jump straight to the original symbol).
/// Selections never REMOVE exports (non-restrictive semantics); an alias
/// naming an unknown export warns and is skipped.
fn apply_selection_to_artifact(
    artifact: &mut ModuleArtifact,
    selection: &mut [ast::ImportedName],
) {
    for selected in selection.iter_mut() {
        if selected.local_name.name == selected.name.name {
            continue;
        }
        // Already materialized on a previous pass — don't duplicate.
        if artifact
            .exports
            .iter()
            .any(|e| e.name == selected.local_name.name)
        {
            continue;
        }
        match artifact.exports.iter().find(|e| e.name == selected.name.name) {
            Some(source_export) => {
                let mut clone = source_export.clone();
                clone.name = selected.local_name.name.clone();
                clone.link_name = Some(
                    source_export
                        .link_name
                        .clone()
                        .unwrap_or_else(|| source_export.name.clone()),
                );
                artifact.exports.push(clone);
                // Done via the artifact; no body clone needed.
                selected.local_name = selected.name.clone();
            }
            None => {
                eprintln!(
                    "agc: warning: selective import alias '{}' names no export '{}'; ignored",
                    selected.local_name.name, selected.name.name
                );
            }
        }
    }
}

/// Materialize `name as alias` selections for SOURCE-inlined modules: the
/// merged program is a single symbol space, so an aliased function is
/// cloned under its local name (a distinct symbol; unused clones cost only
/// dead code). Selections without an alias need no clone; non-restrictive
/// semantics keep everything else reachable.
fn build_function_aliases(
    selection: &[ast::ImportedName],
    program_items: &[ast::Item],
) -> Vec<ast::Item> {
    let mut aliases = Vec::new();
    for selected in selection {
        if selected.local_name.name == selected.name.name {
            continue;
        }
        for item in program_items {
            if let ast::ItemKind::Function(func) = &item.kind {
                if func.name.name == selected.name.name {
                    let mut alias_func = func.clone();
                    alias_func.name = ast::Identifier {
                        name: selected.local_name.name.clone(),
                        span: selected.local_name.span,
                    };
                    aliases.push(ast::Item {
                        kind: ast::ItemKind::Function(alias_func),
                        span: item.span,
                        visibility: item.visibility.clone(),
                        attributes: item.attributes.clone(),
                    });
                }
            }
        }
    }
    aliases
}

/// Run every captured selective import through both alias routes once the
/// full traversal has finished: artifacts get aliased export duplicates,
/// source-inlined modules get renamed function clones. Must run at the
/// OUTERMOST lowering level only, when all transitive inlines and artifact
/// pushes have landed.
fn apply_all_pending_selections(
    pending: Vec<(String, Vec<ast::ImportedName>)>,
    module_imports: &mut [(String, ModuleArtifact)],
    program: &mut ast::Program,
) {
    let mut pending = pending;
    for (module_path, selection) in pending.iter_mut() {
        if let Some((_, artifact)) = module_imports
            .iter_mut()
            .find(|(p, _)| p == module_path)
        {
            apply_selection_to_artifact(artifact, selection);
            // Selections materialized through the artifact are done; the
            // rest fall through to the source route.
            selection.retain(|sel| sel.local_name.name == sel.name.name);
        }
    }
    pending.retain(|(_module_path, selection)| !selection.is_empty());
    for (_module_path, selection) in pending {
        let aliases = build_function_aliases(&selection, &program.items);
        program.items.extend(aliases);
    }
}

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

/// True when any item in `items` uses a bare identifier named `ctor` in an
/// expression position (i.e. not as a field/method access target). Used to
/// decide whether to auto-inject `std.optional` / `std.result` for bare
/// `Some`/`None`/`Ok`/`Err` constructors.
fn uses_bare_constructor(items: &[ast::Item], ctor: &str) -> bool {
    items.iter().any(|item| {
        let mut found = false;
        match &item.kind {
            ast::ItemKind::Function(func) => {
                scan_block_for_bare_ctor(&func.body, ctor, &mut found);
            }
            ast::ItemKind::Impl(impl_item) => {
                for member in &impl_item.items {
                    if let ast::ImplItemKind::Function(func) = member {
                        scan_block_for_bare_ctor(&func.body, ctor, &mut found);
                    }
                }
            }
            ast::ItemKind::Macro(def) => {
                scan_block_for_bare_ctor(&def.body, ctor, &mut found);
            }
            _ => {}
        }
        found
    })
}

fn scan_block_for_bare_ctor(block: &ast::Block, ctor: &str, found: &mut bool) {
    for stmt in &block.statements {
        match &stmt.kind {
            ast::StatementKind::Let(let_stmt) => {
                if let Some(init) = &let_stmt.initializer {
                    scan_expr_for_bare_ctor(init, ctor, found);
                }
            }
            ast::StatementKind::Expression(expr)
            | ast::StatementKind::Return(Some(expr))
            | ast::StatementKind::Break(Some(expr)) => {
                scan_expr_for_bare_ctor(expr, ctor, found);
            }
            ast::StatementKind::Block(block) => scan_block_for_bare_ctor(block, ctor, found),
            ast::StatementKind::Defer(inner) => {
                // Defer holds a statement; scan its expression/block shape.
                match &inner.kind {
                    ast::StatementKind::Expression(expr) => {
                        scan_expr_for_bare_ctor(expr, ctor, found);
                    }
                    ast::StatementKind::Block(block) => {
                        scan_block_for_bare_ctor(block, ctor, found);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        if *found {
            return;
        }
    }
}

fn scan_expr_for_bare_ctor(expr: &ast::Expression, ctor: &str, found: &mut bool) {
    if *found {
        return;
    }
    match expr.kind.as_ref() {
        // Bare `Some(...)` / `Ok(...)` call: the callee is a bare identifier.
        ast::ExpressionKind::Call {
            function,
            arguments,
        } => {
            if let ast::ExpressionKind::Identifier(ident) = function.kind.as_ref()
                && ident.name == ctor
            {
                *found = true;
                return;
            }
            scan_expr_for_bare_ctor(function, ctor, found);
            for arg in arguments {
                scan_expr_for_bare_ctor(arg, ctor, found);
            }
        }
        // Bare `None` / `Err` (payload-less): a bare identifier not followed by
        // a dot (i.e. not a field/method access target).
        ast::ExpressionKind::Identifier(ident) => {
            if ident.name == ctor {
                *found = true;
            }
        }
        ast::ExpressionKind::Block(block) => scan_block_for_bare_ctor(block, ctor, found),
        ast::ExpressionKind::Ternary {
            condition,
            then_expr,
            else_expr,
        } => {
            scan_expr_for_bare_ctor(condition, ctor, found);
            scan_expr_for_bare_ctor(then_expr, ctor, found);
            scan_expr_for_bare_ctor(else_expr, ctor, found);
        }
        ast::ExpressionKind::UnwrapOr { value, fallback } => {
            scan_expr_for_bare_ctor(value, ctor, found);
            scan_expr_for_bare_ctor(fallback, ctor, found);
        }
        ast::ExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            scan_expr_for_bare_ctor(condition, ctor, found);
            scan_block_for_bare_ctor(then_branch, ctor, found);
            if let Some(else_block) = else_branch {
                scan_block_for_bare_ctor(else_block, ctor, found);
            }
        }
        ast::ExpressionKind::While { condition, body } => {
            scan_expr_for_bare_ctor(condition, ctor, found);
            scan_block_for_bare_ctor(body, ctor, found);
        }
        ast::ExpressionKind::For {
            condition, body, ..
        } => {
            scan_expr_for_bare_ctor(condition, ctor, found);
            scan_block_for_bare_ctor(body, ctor, found);
        }
        ast::ExpressionKind::Binary { left, right, .. } => {
            scan_expr_for_bare_ctor(left, ctor, found);
            scan_expr_for_bare_ctor(right, ctor, found);
        }
        ast::ExpressionKind::Unary { operand, .. }
        | ast::ExpressionKind::Postfix { operand, .. }
        | ast::ExpressionKind::Move(operand)
        | ast::ExpressionKind::Comptime(operand) => {
            scan_expr_for_bare_ctor(operand, ctor, found);
        }
        ast::ExpressionKind::Cast { expression, .. } => {
            scan_expr_for_bare_ctor(expression, ctor, found);
        }
        ast::ExpressionKind::FieldAccess { object, .. } => {
            // `Some.variant` / `Optional.Some` are typed accesses, not bare
            // constructors; scan the object but treat the field as a member.
            scan_expr_for_bare_ctor(object, ctor, found);
        }
        ast::ExpressionKind::Index { object, index, .. } => {
            scan_expr_for_bare_ctor(object, ctor, found);
            scan_expr_for_bare_ctor(index, ctor, found);
        }
        ast::ExpressionKind::MethodCall {
            receiver,
            arguments,
            ..
        } => {
            scan_expr_for_bare_ctor(receiver, ctor, found);
            for arg in arguments {
                scan_expr_for_bare_ctor(arg, ctor, found);
            }
        }
        ast::ExpressionKind::StructLiteral { fields, .. } => {
            for field in fields {
                scan_expr_for_bare_ctor(&field.value, ctor, found);
            }
        }
        ast::ExpressionKind::Array(elements) | ast::ExpressionKind::Tuple(elements) => {
            for element in elements {
                scan_expr_for_bare_ctor(element, ctor, found);
            }
        }
        ast::ExpressionKind::Initializer { items, .. } => {
            for item in items {
                match item {
                    ast::InitializerItem::Positional(expr)
                    | ast::InitializerItem::Field { value: expr, .. } => {
                        scan_expr_for_bare_ctor(expr, ctor, found);
                    }
                    ast::InitializerItem::Index { index, value, .. } => {
                        scan_expr_for_bare_ctor(index, ctor, found);
                        scan_expr_for_bare_ctor(value, ctor, found);
                    }
                }
            }
        }
        ast::ExpressionKind::Match { expression, arms } => {
            scan_expr_for_bare_ctor(expression, ctor, found);
            for arm in arms {
                scan_expr_for_bare_ctor(&arm.body, ctor, found);
            }
        }
        ast::ExpressionKind::MacroCall { args, .. } => {
            for arg in args {
                if let ast::MacroArg::Expression(expr) = arg {
                    scan_expr_for_bare_ctor(expr, ctor, found);
                }
            }
        }
        ast::ExpressionKind::ForIn { iterable, body, .. } => {
            scan_expr_for_bare_ctor(iterable, ctor, found);
            scan_block_for_bare_ctor(body, ctor, found);
        }
        ast::ExpressionKind::Reference {
            expression: operand,
            ..
        }
        | ast::ExpressionKind::Launch(operand)
        | ast::ExpressionKind::Wait(operand) => {
            scan_expr_for_bare_ctor(operand, ctor, found);
        }
        ast::ExpressionKind::Literal(_)
        | ast::ExpressionKind::TypeName(_)
        | ast::ExpressionKind::Asm { .. }
        | ast::ExpressionKind::EnumVariant { .. } => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::{lex, Span};
    use crate::module_artifact::{ExportKind, ModuleExport};
    use crate::parser::Parser;

    fn parse(source: &str) -> ast::Program {
        let tokens = lex(source).expect("lex failed");
        let mut parser = Parser::new(tokens);
        let (program, errors) = parser.parse_program();
        assert!(errors.is_empty(), "parse errors: {errors:?}");
        program
    }

    #[test]
    fn artifact_alias_export_uses_original_link_name() {
        // `println as pln` on an artifact: the alias export must carry the
        // ORIGINAL link symbol so calls through `pln` are a compile-time
        // rename, and the original export must remain (non-restrictive).
        let mut artifact = ModuleArtifact {
            module_name: "std.io.file".to_string(),
            module_path: String::new(),
            source_path: String::new(),
            source_hash_fnv1a64: 0,
            compiler_version: String::new(),
            target_triple: String::new(),
            code_artifacts: Default::default(),
            module_deps: Vec::new(),
            transitive_deps: Vec::new(),
            exports: Vec::new(),
            native_libs: Vec::new(),
            native_lib_paths: Vec::new(),
            generic_templates: Vec::new(),
            artifact_path: None,
        };
        artifact.exports.push(ModuleExport {
            kind: ExportKind::Function,
            name: "println".to_string(),
            signature: "fn(str)->i32".to_string(),
            type_params: Vec::new(),
            link_name: Some("println__str__i32".to_string()),
            abi: None,
            is_variadic: false,
            type_key: None,
            fields: Vec::new(),
            layout: None,
            enum_backing_type: None,
            enum_variants: Vec::new(),
            trait_items: Vec::new(),
            const_value: None,
            is_mutable: false,
        });
        let selection = vec![ast::ImportedName {
            name: ast::Identifier {
                name: "println".to_string(),
                span: Span::default(),
            },
            local_name: ast::Identifier {
                name: "pln".to_string(),
                span: Span::default(),
            },
        }];
        apply_selection_to_artifact(&mut artifact, &mut selection.clone());
        assert_eq!(artifact.exports.len(), 2, "alias added, original kept");
        let alias = artifact.exports.last().unwrap();
        assert_eq!(alias.name, "pln");
        assert_eq!(
            alias.link_name.as_deref(),
            Some("println__str__i32"),
            "alias links to the original symbol"
        );
    }

    #[test]
    fn build_function_aliases_clones_selected_fn() {
        let program = parse("i64 make() { return 1; } i32 main() { return 0; }");
        let selection = vec![ast::ImportedName {
            name: ast::Identifier {
                name: "make".to_string(),
                span: Span::default(),
            },
            local_name: ast::Identifier {
                name: "create".to_string(),
                span: Span::default(),
            },
        }];
        let aliases = build_function_aliases(&selection, &program.items);
        assert_eq!(aliases.len(), 1);
        match &aliases[0].kind {
            ast::ItemKind::Function(f) => {
                assert_eq!(f.name.name, "create");
                assert_eq!(f.parameters.len(), 0);
            }
            _ => panic!("expected function alias"),
        }
    }

    #[test]
    fn scan_detects_bare_constructors() {
        let program = parse(
            "i32 main() { Optional<i32> a = Some(1); Result<i32, str> r = Ok(2); return 0; }",
        );
        assert!(
            uses_bare_constructor(&program.items, "Some"),
            "expected Some detected"
        );
        assert!(
            uses_bare_constructor(&program.items, "Ok"),
            "expected Ok detected"
        );
        assert!(!uses_bare_constructor(&program.items, "Err"), "no Err used");
    }

    #[test]
    fn scan_ignores_typed_access() {
        // `Optional.Some(...)` is a typed construction, not a bare ctor.
        let program = parse("i32 main() { Optional<i32> a = Optional<i32>.Some(1); return 0; }");
        assert!(
            !uses_bare_constructor(&program.items, "Some"),
            "typed Some not bare"
        );
    }
}
