use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::module_artifact::{ExportKind, ModuleArtifact, ModuleExport};
use crate::parser::ast;

#[derive(Debug, Clone)]
pub struct ModuleCatalog {
    pub modules: Vec<ModuleArtifact>,
    pub native_libs: Vec<String>,
    pub loaded_paths: Vec<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct SourceImportCatalog {
    pub imports: Vec<ResolvedSourceImport>,
    pub loaded_paths: Vec<PathBuf>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedSourceImport {
    pub module_path: String,
    pub source_path: PathBuf,
    pub kind: ResolvedSourceImportKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResolvedSourceImportKind {
    File,
    Module,
}

#[derive(Debug)]
pub struct ModuleLoader {
    search_dirs: Vec<PathBuf>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            search_dirs: Vec::new(),
        }
    }

    pub fn add_search_dir(&mut self, dir: impl Into<PathBuf>) {
        self.search_dirs.push(dir.into());
    }

    pub fn resolve_imports(&self, program: &ast::Program) -> Result<ModuleCatalog, String> {
        let mut modules = Vec::new();
        let mut native_libs = Vec::new();
        let mut seen_modules = HashSet::new();
        let mut loaded_paths = Vec::new();
        let mut import_entries: Vec<(String, ModuleArtifact)> = Vec::new();

        for item in &program.items {
            let ast::ItemKind::Import(import_item) = &item.kind else {
                continue;
            };
            let module_path = import_path_to_string(&import_item.path);
            if seen_modules.contains(&module_path) {
                continue;
            }
            let artifact_path = self
                .find_module_path(&module_path)
                .ok_or_else(|| format!("module `{module_path}` not found"))?;
            let module = ModuleArtifact::from_path(&artifact_path)?;
            for lib in &module.native_libs {
                if !native_libs.contains(lib) {
                    native_libs.push(lib.clone());
                }
            }
            loaded_paths.push(artifact_path);
            seen_modules.insert(module_path.clone());
            import_entries.push((module_path, module));
        }

        validate_import_conflicts(
            import_entries
                .iter()
                .map(|(module_path, module)| (module_path.as_str(), module)),
        )?;
        for (_, module) in import_entries {
            modules.push(module);
        }

        Ok(ModuleCatalog {
            modules,
            native_libs,
            loaded_paths,
        })
    }

    pub fn resolve_source_imports(
        &self,
        program: &ast::Program,
        base_dir: Option<&Path>,
    ) -> Result<SourceImportCatalog, String> {
        let mut imports = Vec::new();
        let mut loaded_paths = Vec::new();
        let mut seen_modules = HashSet::new();

        for item in &program.items {
            let ast::ItemKind::Import(import_item) = &item.kind else {
                continue;
            };
            let module_path = import_path_to_string(&import_item.path);
            if !seen_modules.insert(module_path.clone()) {
                continue;
            }
            let resolved = self
                .find_source_import(&import_item.path, base_dir)
                .ok_or_else(|| format!("import `{module_path}` could not be resolved"))?;
            loaded_paths.push(resolved.source_path.clone());
            imports.push(resolved);
        }

        Ok(SourceImportCatalog {
            imports,
            loaded_paths,
        })
    }

    pub fn find_module_path(&self, module: &str) -> Option<PathBuf> {
        let segments = module
            .split('.')
            .filter(|segment| !segment.is_empty())
            .collect::<Vec<_>>();
        if segments.is_empty() {
            return None;
        }

        for dir in &self.search_dirs {
            let mut candidate = dir.clone();
            for segment in &segments {
                candidate.push(segment);
            }
            candidate.set_extension("agm");
            if candidate.is_file() {
                return Some(candidate);
            }
        }
        None
    }

    pub fn resolve_module_closure(
        &self,
        roots: &[ModuleArtifact],
    ) -> Result<Vec<ModuleArtifact>, String> {
        let mut resolved = Vec::new();
        let mut seen = HashSet::new();
        let mut pending = roots.to_vec();

        while let Some(module) = pending.pop() {
            if !seen.insert(module.module_path.clone()) {
                continue;
            }
            for dep in &module.module_deps {
                if seen.contains(dep) {
                    continue;
                }
                let artifact_path = self
                    .find_module_path(dep)
                    .ok_or_else(|| format!("module `{dep}` not found"))?;
                pending.push(ModuleArtifact::from_path(&artifact_path)?);
            }
            resolved.push(module);
        }

        Ok(resolved)
    }

    pub fn find_source_import(
        &self,
        path: &[ast::Identifier],
        base_dir: Option<&Path>,
    ) -> Option<ResolvedSourceImport> {
        let segments: Vec<&str> = path.iter().map(|segment| segment.name.as_str()).collect();
        if segments.is_empty() {
            return None;
        }

        let module_path = segments.join(".");

        // Priority 1: relative_path (relative to the currently compiled file)
        if let Some(base) = base_dir {
            if let Some((source_path, kind)) = resolve_source_in_root(base, &segments) {
                return Some(ResolvedSourceImport {
                    module_path: module_path.clone(),
                    source_path,
                    kind,
                });
            }
        }

        // Priority 2: cwd (current working directory of the process)
        let cwd = std::env::current_dir().ok();
        if let Some(cwd) = &cwd {
            if let Some((source_path, kind)) = resolve_source_in_root(&cwd, &segments) {
                return Some(ResolvedSourceImport {
                    module_path: module_path.clone(),
                    source_path,
                    kind,
                });
            }
        }

        // Priority 3+: include dirs then sysroot dirs as appended by build_module_loader.
        for root in &self.search_dirs {
            // Skip roots already checked as base_dir or cwd.
            if base_dir.is_some_and(|base| base == root) {
                continue;
            }
            if cwd.as_ref().is_some_and(|cwd| cwd == root) {
                continue;
            }

            if let Some((source_path, kind)) = resolve_source_in_root(root, &segments) {
                return Some(ResolvedSourceImport {
                    module_path: module_path.clone(),
                    source_path,
                    kind,
                });
            }
        }

        None
    }
}

fn resolve_source_in_root(
    root: &Path,
    segments: &[&str],
) -> Option<(PathBuf, ResolvedSourceImportKind)> {
    let mut joined = root.to_path_buf();
    for segment in segments {
        joined.push(segment);
    }

    // Prefer source file (.ag)
    let source_path = joined.with_extension("ag");
    if source_path.is_file() {
        return Some((source_path, ResolvedSourceImportKind::File));
    }

    // Fallback to module interface file (.agm)
    let binary_path = joined.with_extension("agm");
    if binary_path.is_file() {
        return Some((binary_path, ResolvedSourceImportKind::Module));
    }

    None
}

pub fn import_path_to_string(path: &[ast::Identifier]) -> String {
    path.iter()
        .map(|segment| segment.name.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

pub fn filter_exports(artifact: &ModuleArtifact, import: &ast::ImportItem) -> Vec<ModuleExport> {
    if let Some(items) = &import.items {
        let mut selected = Vec::new();
        for item in items {
            if let Some(export) = artifact
                .exports
                .iter()
                .find(|export| export.name == item.name.name)
            {
                selected.push(export.clone());
            }
        }
        return selected;
    }
    artifact.exports.clone()
}

pub fn module_loader_default_dirs(sysroot: Option<&Path>) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    if let Some(root) = sysroot {
        dirs.push(root.join("lib").join("silver"));
    }
    if let Ok(home) = std::env::var("SILVER_SYSROOT") {
        if !home.is_empty() {
            dirs.push(PathBuf::from(home).join("lib").join("silver"));
        }
    }
    dirs
}

pub fn collect_imported_libs(
    program: &ast::Program,
    loader: &ModuleLoader,
) -> Result<Vec<String>, String> {
    let catalog = loader.resolve_imports(program)?;
    Ok(catalog.native_libs)
}

pub fn collect_imported_artifacts(
    program: &ast::Program,
    loader: &ModuleLoader,
    base_dir: Option<&Path>,
) -> Result<Vec<ModuleArtifact>, String> {
    let catalog = loader.resolve_source_imports(program, base_dir)?;
    let mut artifacts = Vec::new();
    let mut import_entries: Vec<(String, ModuleArtifact)> = Vec::new();

    for import in &catalog.imports {
        let artifact = match import.kind {
            ResolvedSourceImportKind::File => ModuleArtifact::from_source(&import.source_path)?,
            ResolvedSourceImportKind::Module => ModuleArtifact::from_path(&import.source_path)?,
        };
        import_entries.push((import.module_path.clone(), artifact));
    }

    validate_import_conflicts(
        import_entries
            .iter()
            .map(|(module_path, module)| (module_path.as_str(), module)),
    )?;
    for (_, artifact) in import_entries {
        artifacts.push(artifact);
    }

    Ok(artifacts)
}

pub fn validate_import_conflicts<'a>(
    imports: impl IntoIterator<Item = (&'a str, &'a ModuleArtifact)>,
) -> Result<(), String> {
    let mut non_function_exports: HashMap<String, (String, ExportKind)> = HashMap::new();
    let mut function_exports: HashMap<String, HashMap<String, String>> = HashMap::new();
    let mut function_owner: HashMap<String, String> = HashMap::new();

    for (module_path, module) in imports {
        for export in &module.exports {
            let name = export.name.clone();
            match export.kind {
                ExportKind::Function => {
                    if let Some((prev_module, prev_kind)) = non_function_exports.get(&name) {
                        return Err(format!(
                            "import conflict for `{name}`: function from `{module_path}` conflicts with {} from `{prev_module}`",
                            export_kind_label(*prev_kind)
                        ));
                    }
                    function_owner
                        .entry(name.clone())
                        .or_insert_with(|| module_path.to_string());
                    let signatures = function_exports.entry(name.clone()).or_default();
                    if let Some(previous_module) = signatures.get(&export.signature) {
                        return Err(format!(
                            "import conflict for `{name}`: duplicate function signature `{}` from `{}` and `{}`",
                            export.signature, previous_module, module_path
                        ));
                    }
                    signatures.insert(export.signature.clone(), module_path.to_string());
                }
                kind => {
                    if let Some((prev_module, prev_kind)) = non_function_exports.get(&name) {
                        return Err(format!(
                            "import conflict for `{name}`: {} from `{module_path}` conflicts with {} from `{prev_module}`",
                            export_kind_label(kind),
                            export_kind_label(*prev_kind),
                        ));
                    }
                    if let Some(prev_module) = function_owner.get(&name) {
                        return Err(format!(
                            "import conflict for `{name}`: {} from `{module_path}` conflicts with function from `{prev_module}`",
                            export_kind_label(kind),
                        ));
                    }
                    non_function_exports.insert(name, (module_path.to_string(), kind));
                }
            }
        }
    }

    Ok(())
}

fn export_kind_label(kind: ExportKind) -> &'static str {
    match kind {
        ExportKind::Function => "function",
        ExportKind::Struct => "struct",
        ExportKind::Enum => "enum",
        ExportKind::Trait => "trait",
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn ident(name: &str) -> ast::Identifier {
        ast::Identifier {
            name: name.to_string(),
            span: Span { start: 0, end: 0 },
        }
    }

    fn import_program(path: &[&str]) -> ast::Program {
        ast::Program {
            attributes: Vec::new(),
            items: vec![ast::Item {
                kind: ast::ItemKind::Import(ast::ImportItem {
                    path: path.iter().map(|segment| ident(segment)).collect(),
                    alias: None,
                    items: None,
                }),
                span: Span { start: 0, end: 0 },
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            }],
            span: Span { start: 0, end: 0 },
        }
    }

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("agc-{label}-{nonce}"))
    }

    #[test]
    fn resolves_imports_with_priority() {
        let rel_dir = unique_temp_dir("priority-rel");
        let include_dir = unique_temp_dir("priority-include");
        let sys_dir = unique_temp_dir("priority-sys");

        // Setup std/io.ag in all locations relevant to search order.
        for (dir, content) in [
            (&rel_dir, "rel"),
            (&include_dir, "include"),
            (&sys_dir, "sys"),
        ] {
            std::fs::create_dir_all(dir.join("std")).unwrap();
            std::fs::write(dir.join("std").join("io.ag"), content).unwrap();
        }

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&include_dir);
        loader.add_search_dir(&sys_dir);

        // 1. Check relative priority
        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "io"]), Some(&rel_dir))
            .unwrap();
        assert_eq!(
            catalog.imports[0].source_path,
            rel_dir.join("std").join("io.ag")
        );

        // 2. Check include-dir priority (without relative source)
        std::fs::remove_file(rel_dir.join("std").join("io.ag")).unwrap();
        std::fs::write(include_dir.join("std").join("unique.ag"), "include").unwrap();
        std::fs::write(sys_dir.join("std").join("unique.ag"), "sys").unwrap();

        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "unique"]), None)
            .unwrap();
        assert_eq!(
            catalog.imports[0].source_path,
            include_dir.join("std").join("unique.ag")
        );

        // 3. Check sysroot priority
        std::fs::remove_file(include_dir.join("std").join("unique.ag")).unwrap();
        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "unique"]), None)
            .unwrap();
        assert_eq!(
            catalog.imports[0].source_path,
            sys_dir.join("std").join("unique.ag")
        );

        let _ = std::fs::remove_dir_all(rel_dir);
        let _ = std::fs::remove_dir_all(include_dir);
        let _ = std::fs::remove_dir_all(sys_dir);
    }

    #[test]
    fn prefers_source_over_binary() {
        let root = unique_temp_dir("priority-pref");
        std::fs::create_dir_all(root.join("std")).unwrap();

        let source_path = root.join("std").join("io.ag");
        let binary_path = root.join("std").join("io.agm");

        std::fs::write(&source_path, "// source").unwrap();
        std::fs::write(&binary_path, "binary content").unwrap();

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "io"]), None)
            .unwrap();

        assert_eq!(catalog.imports[0].kind, ResolvedSourceImportKind::File);
        assert_eq!(catalog.imports[0].source_path, source_path);

        // Remove source, should find binary
        std::fs::remove_file(source_path).unwrap();
        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "io"]), None)
            .unwrap();
        assert_eq!(catalog.imports[0].kind, ResolvedSourceImportKind::Module);
        assert_eq!(catalog.imports[0].source_path, binary_path);

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn error_when_import_not_found() {
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(unique_temp_dir("empty"));

        let error = loader
            .resolve_source_imports(&import_program(&["unknown", "module"]), None)
            .unwrap_err();

        assert_eq!(error, "import `unknown.module` could not be resolved");
    }

    #[test]
    fn rejects_conflicting_non_function_exports() {
        let root = unique_temp_dir("conflict-non-function");
        std::fs::create_dir_all(root.join("a")).unwrap();
        std::fs::create_dir_all(root.join("b")).unwrap();
        std::fs::write(
            root.join("a").join("mod1.ag"),
            "pub struct Thing { i32 x; }",
        )
        .unwrap();
        std::fs::write(
            root.join("b").join("mod2.ag"),
            "pub struct Thing { i32 y; }",
        )
        .unwrap();

        let program = ast::Program {
            attributes: Vec::new(),
            items: vec![
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("a"), ident("mod1")],
                        alias: None,
                        items: None,
                    }),
                    span: Span { start: 0, end: 0 },
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("b"), ident("mod2")],
                        alias: None,
                        items: None,
                    }),
                    span: Span { start: 0, end: 0 },
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
            ],
            span: Span { start: 0, end: 0 },
        };

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let error = collect_imported_artifacts(&program, &loader, None).unwrap_err();
        assert!(error.contains("import conflict for `Thing`"));
        assert!(error.contains("struct"));

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn rejects_duplicate_function_signatures_across_modules() {
        let root = unique_temp_dir("conflict-fn-signature");
        std::fs::create_dir_all(root.join("a")).unwrap();
        std::fs::create_dir_all(root.join("b")).unwrap();
        std::fs::write(
            root.join("a").join("mod1.ag"),
            "pub i32 add(i32 x, i32 y) { return x + y; }",
        )
        .unwrap();
        std::fs::write(
            root.join("b").join("mod2.ag"),
            "pub i32 add(i32 a, i32 b) { return a + b; }",
        )
        .unwrap();

        let program = ast::Program {
            attributes: Vec::new(),
            items: vec![
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("a"), ident("mod1")],
                        alias: None,
                        items: None,
                    }),
                    span: Span { start: 0, end: 0 },
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("b"), ident("mod2")],
                        alias: None,
                        items: None,
                    }),
                    span: Span { start: 0, end: 0 },
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
            ],
            span: Span { start: 0, end: 0 },
        };

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let error = collect_imported_artifacts(&program, &loader, None).unwrap_err();
        assert!(error.contains("import conflict for `add`"));
        assert!(error.contains("duplicate function signature"));

        let _ = std::fs::remove_dir_all(root);
    }
}
