use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
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

use parking_lot::Mutex;
use std::sync::Arc;
use crate::cache_store::{CacheStore, CacheKey, CacheKeyBuilder, CachedModule};

#[derive(Debug)]
pub struct ModuleLoader {
    pub search_dirs: Vec<PathBuf>,
    pub cwd: Option<PathBuf>,
    /// Cache of loaded module artifacts keyed by module path (e.g. "std.mem.vec").
    pub module_cache: Mutex<HashMap<String, Result<ModuleArtifact, String>>>,
    pub cache_store: Option<Arc<CacheStore>>,
    pub no_cache: bool,
    pub target: Option<String>,
    pub opt_level: Option<String>,
    pub debug_info: bool,
    pub leak_check: bool,
    pub cfg_flags: Vec<String>,
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            search_dirs: Vec::new(),
            cwd: std::env::current_dir().ok(),
            module_cache: Mutex::new(HashMap::default()),
            cache_store: None,
            no_cache: false,
            target: None,
            opt_level: None,
            debug_info: false,
            leak_check: false,
            cfg_flags: Vec::new(),
        }
    }

    pub fn compute_cache_key(&self, source_path: &Path, _module_path: &str) -> Option<CacheKey> {
        let canonical = std::fs::canonicalize(source_path).unwrap_or_else(|_| source_path.to_path_buf());
        let mut builder = CacheKeyBuilder::new(&canonical.display().to_string());
        if builder.add_file(&canonical).is_err() {
            return None;
        }
        builder.add_compiler_version(env!("CARGO_PKG_VERSION"));
        builder.add_target(self.target.as_deref().unwrap_or("default"));
        builder.add_opt_level(self.opt_level.as_deref());
        builder.add_flags(&self.cfg_flags);
        if self.debug_info {
            builder.add_str("debug_info");
        }
        if self.leak_check {
            builder.add_str("leak_check");
        }
        Some(builder.finish())
    }

    pub fn get_cached_module(&self, source_path: &Path, module_path: &str) -> Option<CachedModule> {
        if self.no_cache {
            return None;
        }
        let store = self.cache_store.as_ref()?;
        let key = self.compute_cache_key(source_path, module_path)?;
        store.get(&key)
    }

    pub fn add_search_dir(&mut self, dir: impl Into<PathBuf>) {
        self.search_dirs.push(dir.into());
    }

    fn load_cached_module(&self, module_path: &str) -> Result<ModuleArtifact, String> {
        let mut cache = self.module_cache.lock();
        if let Some(entry) = cache.get(module_path) {
            return entry.clone();
        }
        let artifact_path = if let Some(path) = self.find_module_path(module_path) {
            Some(path)
        } else {
            let idents: Vec<crate::parser::ast::Identifier> = module_path
                .split('.')
                .map(|name| crate::parser::ast::Identifier {
                    name: name.to_string(),
                    span: crate::lexer::Span::default(),
                })
                .collect();
            if let Some(resolved) = self.find_source_import(&idents, None) {
                if let Some(cached) = self.get_cached_module(&resolved.source_path, module_path) {
                    Some(cached.agm_path)
                } else if resolved.kind == ResolvedSourceImportKind::Module {
                    Some(resolved.source_path)
                } else {
                    None
                }
            } else {
                None
            }
        };
        let artifact_path = artifact_path.ok_or_else(|| format!("module `{module_path}` not found"))?;
        let result = ModuleArtifact::from_path(&artifact_path);
        let cached = result.clone();
        cache.insert(module_path.to_string(), cached);
        result
    }

    pub fn resolve_imports(&self, program: &ast::Program) -> Result<ModuleCatalog, String> {
        let mut modules = Vec::new();
        let mut native_libs = Vec::new();
        let mut seen_modules = HashSet::default();
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
            let module = self.load_cached_module(&module_path)?;
            let artifact_path = module.artifact_path.clone().unwrap_or_default();
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
        let mut seen_modules = HashSet::default();

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
            let submodule_config = candidate.with_extension("submodule.toml");
            if submodule_config.is_file() {
                match ensure_submodule_built(&submodule_config, &candidate) {
                    Ok(compiled) => return Some(compiled),
                    Err(err) => {
                        eprintln!("agc: {err}");
                    }
                }
            }
        }
        None
    }

    pub fn resolve_module_closure(
        &self,
        roots: &[ModuleArtifact],
    ) -> Result<Vec<ModuleArtifact>, String> {
        let mut seen = HashSet::default();
        let mut resolved = Vec::new();

        self.resolve_module_closure_dfs(roots, &mut seen, &mut resolved)?;

        Ok(resolved)
    }

    /// Recursive DFS helper for `resolve_module_closure`. Every module path is
    /// visited at most once (the seen set dedups), so dependency cycles — even
    /// mutual ones — resolve to a single copy of each module rather than
    /// recursing forever.
    fn resolve_module_closure_dfs(
        &self,
        modules: &[ModuleArtifact],
        seen: &mut HashSet<String>,
        resolved: &mut Vec<ModuleArtifact>,
    ) -> Result<(), String> {
        for module in modules {
            if !seen.insert(module.module_path.clone()) {
                continue;
            }
            for dep in &module.module_deps {
                if let Ok(dep_module) = self.load_cached_module(dep) {
                    self.resolve_module_closure_dfs(std::slice::from_ref(&dep_module), seen, resolved)?;
                }
            }
            resolved.push(module.clone());
        }
        Ok(())
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
        if let Some(base) = base_dir
            && let Some((source_path, kind)) = resolve_source_in_root(base, &segments)
        {
            return Some(ResolvedSourceImport {
                module_path: module_path.clone(),
                source_path,
                kind,
            });
        }

        // Priority 2: cwd (current working directory of the process)
        if let Some(cwd) = &self.cwd
            && let Some((source_path, kind)) = resolve_source_in_root(cwd, &segments)
        {
            return Some(ResolvedSourceImport {
                module_path: module_path.clone(),
                source_path,
                kind,
            });
        }

        // Priority 3+: include dirs then sysroot dirs as appended by build_module_loader.
        for root in &self.search_dirs {
            // Skip roots already checked as base_dir or cwd.
            if base_dir.is_some_and(|base| base == root) {
                continue;
            }
            if self.cwd.as_ref().is_some_and(|cwd| cwd == root) {
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

fn find_agsm_binary() -> Option<PathBuf> {
    if let Ok(current_exe) = std::env::current_exe() {
        if let Some(parent) = current_exe.parent() {
            let candidate = parent.join("agsm");
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }

    if let Ok(cwd) = std::env::current_dir() {
        let debug_candidate = cwd.join("target").join("debug").join("agsm");
        if debug_candidate.is_file() {
            return Some(debug_candidate);
        }
        let release_candidate = cwd.join("target").join("release").join("agsm");
        if release_candidate.is_file() {
            return Some(release_candidate);
        }
    }

    if let Ok(path_var) = std::env::var("PATH") {
        for dir in std::env::split_paths(&path_var) {
            let candidate = dir.join("agsm");
            if candidate.is_file() {
                return Some(candidate);
            }
        }
    }

    None
}

fn ensure_submodule_built(submodule_config: &Path, output_path: &Path) -> Result<PathBuf, String> {
    let needs_rebuild = if !output_path.is_file() {
        true
    } else {
        match (
            std::fs::metadata(submodule_config),
            std::fs::metadata(output_path),
        ) {
            (Ok(cfg_meta), Ok(out_meta)) => match (cfg_meta.modified(), out_meta.modified()) {
                (Ok(cfg_time), Ok(out_time)) => cfg_time > out_time,
                _ => false,
            },
            _ => true,
        }
    };

    if !needs_rebuild {
        return Ok(output_path.to_path_buf());
    }

    let agsm_bin = find_agsm_binary().ok_or_else(|| {
        format!(
            "`agsm` binary not found to compile {}",
            submodule_config.display()
        )
    })?;

    let output = std::process::Command::new(agsm_bin)
        .arg("build")
        .arg(submodule_config)
        .arg("-o")
        .arg(output_path)
        .output()
        .map_err(|err| format!("failed to execute agsm: {err}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(format!(
            "agsm build failed for {}: {}",
            submodule_config.display(),
            stderr.trim()
        ));
    }

    Ok(output_path.to_path_buf())
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

    // On-demand build from submodule configuration (.submodule.toml)
    let submodule_config = joined.with_extension("submodule.toml");
    if submodule_config.is_file() {
        match ensure_submodule_built(&submodule_config, &binary_path) {
            Ok(compiled) => return Some((compiled, ResolvedSourceImportKind::Module)),
            Err(err) => {
                eprintln!("agc: {err}");
            }
        }
    }

    None
}

pub fn import_path_to_string(path: &[ast::Identifier]) -> String {
    path.iter()
        .map(|segment| segment.name.as_str())
        .collect::<Vec<_>>()
        .join(".")
}

/// Returns all exports from the artifact (import guards removed).
pub fn filter_exports(artifact: &ModuleArtifact, _import: &ast::ImportItem) -> Vec<ModuleExport> {
    artifact.exports.clone()
}

fn add_dir_unique(dirs: &mut Vec<PathBuf>, path: PathBuf) {
    if !dirs.contains(&path) {
        dirs.push(path);
    }
}

fn add_sysroot_candidates(dirs: &mut Vec<PathBuf>, root: &Path) {
    let inc = root.join("include");
    let inc_silver = inc.join("silver");
    if inc_silver.is_dir() {
        add_dir_unique(dirs, inc_silver);
    }
    add_dir_unique(dirs, inc);

    let lib = root.join("lib");
    let lib_silver = lib.join("silver");
    if lib_silver.is_dir() {
        add_dir_unique(dirs, lib_silver);
    }
    add_dir_unique(dirs, lib);
}

pub fn module_loader_default_dirs(sysroot: Option<&Path>) -> Vec<PathBuf> {
    let mut dirs = Vec::new();

    // 1. Explicit CLI --sysroot
    if let Some(root) = sysroot {
        add_sysroot_candidates(&mut dirs, root);
    }

    // 2. SILVER_SYSROOT environment variable
    if let Ok(home) = std::env::var("SILVER_SYSROOT")
        && !home.is_empty()
    {
        add_sysroot_candidates(&mut dirs, Path::new(&home));
    }

    // 3. Build-time configured sysroot (from build.rs)
    if let Some(build_sysroot) = option_env!("SILVER_BUILD_SYSROOT") {
        if !build_sysroot.is_empty() {
            add_sysroot_candidates(&mut dirs, Path::new(build_sysroot));
        }
    }

    // 4. Executable-relative sysroot (e.g. <prefix>/bin/agc -> <prefix>/include, <prefix>/lib)
    if let Ok(current_exe) = std::env::current_exe() {
        if let Some(bin_dir) = current_exe.parent() {
            if let Some(prefix) = bin_dir.parent() {
                add_sysroot_candidates(&mut dirs, prefix);
            }
        }
    }

    // 5. User installation directory (~/.local/share/silver, $XDG_DATA_HOME/silver, etc.)
    if let Ok(xdg_data) = std::env::var("XDG_DATA_HOME") {
        if !xdg_data.is_empty() {
            let user_silver = Path::new(&xdg_data).join("silver");
            add_sysroot_candidates(&mut dirs, &user_silver);
        }
    } else if let Ok(home) = std::env::var("HOME") {
        if !home.is_empty() {
            let user_silver = Path::new(&home).join(".local").join("share").join("silver");
            add_sysroot_candidates(&mut dirs, &user_silver);
        }
    }

    // Standard system share directories
    add_sysroot_candidates(&mut dirs, Path::new("/usr/local/share/silver"));
    add_sysroot_candidates(&mut dirs, Path::new("/usr/share/silver"));

    // 6. Build-time configured include dirs
    if let Some(build_includes) = option_env!("SILVER_BUILD_INCLUDE_DIRS") {
        for path in std::env::split_paths(build_includes) {
            add_dir_unique(&mut dirs, path);
        }
    }

    // 7. Build-time configured root dirs
    if let Some(build_roots) = option_env!("SILVER_BUILD_ROOT_DIRS") {
        for path in std::env::split_paths(build_roots) {
            add_sysroot_candidates(&mut dirs, &path);
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
    let mut non_function_exports: HashMap<String, (String, ExportKind)> = HashMap::default();
    let mut function_exports: HashMap<String, HashMap<String, String>> = HashMap::default();
    let mut function_owner: HashMap<String, String> = HashMap::default();
    let mut seen_source_hashes = std::collections::HashSet::new();

    for (module_path, module) in imports {
        if !seen_source_hashes.insert(module.source_hash_fnv1a64) {
            continue;
        }
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
        ExportKind::Constant => "constant",
        ExportKind::Global => "global",
        ExportKind::TypeAlias => "type alias",
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
            span: Span::default(),
        }
    }

    fn import_program(path: &[&str]) -> ast::Program {
        ast::Program {
            attributes: Vec::new(),
            comments: Vec::new(),
            items: vec![ast::Item {
                kind: ast::ItemKind::Import(ast::ImportItem {
                    path: path.iter().map(|segment| ident(segment)).collect(),
                    selection: None,
                }),
                span: Span::default(),
                visibility: ast::Visibility::Private,
                attributes: Vec::new(),
            }],
            span: Span::default(),
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
        std::fs::write(root.join("a").join("mod1.ag"), "struct Thing { i32 x; }").unwrap();
        std::fs::write(root.join("b").join("mod2.ag"), "struct Thing { i32 y; }").unwrap();

        let program = ast::Program {
            attributes: Vec::new(),
            comments: Vec::new(),
            items: vec![
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("a"), ident("mod1")],
                    selection: None,
                    }),
                    span: Span::default(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("b"), ident("mod2")],
                    selection: None,
                    }),
                    span: Span::default(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
            ],
            span: Span::default(),
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
            "i32 add(i32 x, i32 y) { return x + y; }",
        )
        .unwrap();
        std::fs::write(
            root.join("b").join("mod2.ag"),
            "i32 add(i32 a, i32 b) { return a + b; }",
        )
        .unwrap();

        let program = ast::Program {
            attributes: Vec::new(),
            comments: Vec::new(),
            items: vec![
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("a"), ident("mod1")],
                    selection: None,
                    }),
                    span: Span::default(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
                ast::Item {
                    kind: ast::ItemKind::Import(ast::ImportItem {
                        path: vec![ident("b"), ident("mod2")],
                    selection: None,
                    }),
                    span: Span::default(),
                    visibility: ast::Visibility::Private,
                    attributes: Vec::new(),
                },
            ],
            span: Span::default(),
        };

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let error = collect_imported_artifacts(&program, &loader, None).unwrap_err();
        assert!(error.contains("import conflict for `add`"));
        assert!(error.contains("duplicate function signature"));

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_submodule_config_when_binary_missing() {
        let root = unique_temp_dir("submodule-ondemand");
        std::fs::create_dir_all(root.join("vendor").join("math")).unwrap();

        let header_path = root.join("vendor").join("math").join("math.h");
        let config_path = root.join("vendor").join("math").join("math.submodule.toml");
        let binary_path = root.join("vendor").join("math").join("math.agm");

        std::fs::write(&header_path, "int add(int a, int b);").unwrap();
        std::fs::write(
            &config_path,
            r#"
name = "math"
standard = "c99"
includes = ["math.h"]
"#,
        )
        .unwrap();

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        // Resolving import when .agm is missing will compile on demand if agsm binary is available
        if find_agsm_binary().is_some() {
            let catalog = loader
                .resolve_source_imports(&import_program(&["vendor", "math", "math"]), None)
                .unwrap();
            assert_eq!(catalog.imports[0].kind, ResolvedSourceImportKind::Module);
            assert_eq!(catalog.imports[0].source_path, binary_path);
            assert!(binary_path.is_file());
        }

        let _ = std::fs::remove_dir_all(root);
    }
}
