use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::module_artifact::{ModuleArtifact, ModuleExport};
use crate::parser::ast;

#[derive(Debug, Clone)]
pub struct ModuleCatalog {
    pub modules: Vec<ModuleArtifact>,
    pub native_libs: Vec<String>,
    pub loaded_paths: Vec<PathBuf>,
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
