use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::module_artifact::{ModuleArtifact, ModuleExport};
use crate::parser::ast;

pub const PACKAGE_ROOT_NAME: &str = "pkg";

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
    package_root: Option<PathBuf>,
}

impl ModuleLoader {
    pub fn new() -> Self {
        Self {
            search_dirs: Vec::new(),
            package_root: None,
        }
    }

    pub fn add_search_dir(&mut self, dir: impl Into<PathBuf>) {
        self.search_dirs.push(dir.into());
    }

    pub fn set_package_root(&mut self, dir: impl Into<PathBuf>) {
        self.package_root = Some(dir.into());
    }

    pub fn package_root(&self) -> Option<&Path> {
        self.package_root.as_deref()
    }

    pub fn resolve_imports(&self, program: &ast::Program) -> Result<ModuleCatalog, String> {
        let mut modules = Vec::new();
        let mut native_libs = Vec::new();
        let mut seen_modules = HashSet::new();
        let mut loaded_paths = Vec::new();

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
            seen_modules.insert(module_path);
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
                .find_source_import(&import_item.path)
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
        let filename = format!("{module}.agbm");
        for dir in &self.search_dirs {
            let candidate = dir.join(&filename);
            if candidate.is_file() {
                return Some(candidate);
            }
        }
        None
    }

    pub fn find_source_import(&self, path: &[ast::Identifier]) -> Option<ResolvedSourceImport> {
        let mut segments: Vec<&str> = path.iter().map(|segment| segment.name.as_str()).collect();
        if segments.is_empty() {
            return None;
        }

        let module_path = segments.join(".");
        let roots = if segments.first().copied() == Some(PACKAGE_ROOT_NAME) {
            segments.remove(0);
            if segments.is_empty() {
                return None;
            }
            vec![self.package_root.as_ref()?.clone()]
        } else {
            self.search_dirs.clone()
        };

        for root in roots {
            if let Some((source_path, kind)) = resolve_source_in_root(&root, &segments) {
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

    let file_path = joined.with_extension("ag");
    if file_path.is_file() {
        return Some((file_path, ResolvedSourceImportKind::File));
    }

    let module_path = joined.join("mod.ag");
    if module_path.is_file() {
        return Some((module_path, ResolvedSourceImportKind::Module));
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
) -> Result<Vec<ModuleArtifact>, String> {
    let catalog = loader.resolve_imports(program)?;
    Ok(catalog.modules)
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
    fn resolves_file_imports_from_search_dirs() {
        let root = unique_temp_dir("file-import");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(root.join("std").join("io.ag"), "").unwrap();

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "io"]))
            .unwrap();

        assert_eq!(catalog.imports.len(), 1);
        assert_eq!(catalog.imports[0].kind, ResolvedSourceImportKind::File);
        assert_eq!(
            catalog.imports[0].source_path,
            root.join("std").join("io.ag")
        );

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_module_directory_imports_from_search_dirs() {
        let root = unique_temp_dir("module-import");
        std::fs::create_dir_all(root.join("std").join("math")).unwrap();
        std::fs::write(root.join("std").join("math").join("mod.ag"), "").unwrap();

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);

        let catalog = loader
            .resolve_source_imports(&import_program(&["std", "math"]))
            .unwrap();

        assert_eq!(catalog.imports.len(), 1);
        assert_eq!(catalog.imports[0].kind, ResolvedSourceImportKind::Module);
        assert_eq!(
            catalog.imports[0].source_path,
            root.join("std").join("math").join("mod.ag")
        );

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn resolves_reserved_pkg_imports_from_package_root() {
        let package_root = unique_temp_dir("pkg-import");
        std::fs::create_dir_all(package_root.join("submodule")).unwrap();
        std::fs::write(package_root.join("submodule").join("thing.ag"), "").unwrap();

        let mut loader = ModuleLoader::new();
        loader.add_search_dir(unique_temp_dir("search-dir-ignored"));
        loader.set_package_root(&package_root);

        let catalog = loader
            .resolve_source_imports(&import_program(&["pkg", "submodule", "thing"]))
            .unwrap();

        assert_eq!(catalog.imports.len(), 1);
        assert_eq!(catalog.imports[0].module_path, "pkg.submodule.thing");
        assert_eq!(
            catalog.imports[0].source_path,
            package_root.join("submodule").join("thing.ag")
        );

        let _ = std::fs::remove_dir_all(package_root);
    }

    #[test]
    fn pkg_root_requires_package_root_to_be_set() {
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(unique_temp_dir("search-dir"));

        let error = loader
            .resolve_source_imports(&import_program(&["pkg", "util"]))
            .unwrap_err();

        assert_eq!(error, "import `pkg.util` could not be resolved");
    }
}
