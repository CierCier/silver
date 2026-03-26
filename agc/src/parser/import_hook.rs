use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::lexer;
use crate::module_artifact::ModuleArtifact;
use crate::module_loader::{
    ModuleLoader, ResolvedSourceImportKind, import_path_to_string, validate_import_conflicts,
};
use crate::parser::Parser;
use crate::parser::ast;

#[derive(Debug, Default)]
pub struct ImportLoweringResult {
    pub module_artifacts: Vec<ModuleArtifact>,
}

pub struct FileImportResolverHook<'a> {
    loader: &'a ModuleLoader,
    seen_modules: HashSet<String>,
    seen_files: HashSet<PathBuf>,
    module_imports: Vec<(String, ModuleArtifact)>,
}

impl<'a> FileImportResolverHook<'a> {
    pub fn new(loader: &'a ModuleLoader) -> Self {
        Self {
            loader,
            seen_modules: HashSet::new(),
            seen_files: HashSet::new(),
            module_imports: Vec::new(),
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

        self.lower_program_recursive(program, base_dir)?;
        validate_import_conflicts(
            self.module_imports
                .iter()
                .map(|(module_path, module)| (module_path.as_str(), module)),
        )?;

        Ok(ImportLoweringResult {
            module_artifacts: self.module_imports.into_iter().map(|(_, m)| m).collect(),
        })
    }

    fn lower_program_recursive(
        &mut self,
        program: &mut ast::Program,
        base_dir: Option<&Path>,
    ) -> Result<(), String> {
        let mut lowered_items = Vec::new();
        let mut lowered_program_attributes = std::mem::take(&mut program.attributes);

        for item in std::mem::take(&mut program.items) {
            let ast::Item {
                kind,
                span,
                visibility,
                attributes,
            } = item;

            let ast::ItemKind::Import(import_item) = kind else {
                lowered_items.push(ast::Item {
                    kind,
                    span,
                    visibility,
                    attributes,
                });
                continue;
            };

            let module_path = import_path_to_string(&import_item.path);
            if !self.seen_modules.insert(module_path.clone()) {
                continue;
            }

            let resolved = self
                .loader
                .find_source_import(&import_item.path, base_dir)
                .ok_or_else(|| format!("import `{module_path}` could not be resolved"))?;

            match resolved.kind {
                ResolvedSourceImportKind::File => {
                    if !self.mark_file_seen(&resolved.source_path) {
                        continue;
                    }
                    let mut imported_program = parse_program_from_file(&resolved.source_path)?;
                    self.lower_program_recursive(&mut imported_program, resolved.source_path.parent())?;
                    lowered_program_attributes.extend(imported_program.attributes);
                    lowered_items.extend(imported_program.items);
                }
                ResolvedSourceImportKind::Module => {
                    let artifact = ModuleArtifact::from_path(&resolved.source_path)?;
                    self.module_imports.push((resolved.module_path.clone(), artifact));
                }
            }
        }

        program.items = lowered_items;
        program.attributes = lowered_program_attributes;
        Ok(())
    }

    fn mark_file_seen(&mut self, path: &Path) -> bool {
        let stable = std::fs::canonicalize(path).unwrap_or_else(|_| path.to_path_buf());
        self.seen_files.insert(stable)
    }
}

fn parse_program_from_file(path: &Path) -> Result<ast::Program, String> {
    let src =
        std::fs::read_to_string(path).map_err(|e| format!("failed to read {}: {e}", path.display()))?;
    let tokens = lexer::lex(&src).map_err(|e| format!("lexer errors in {}: {e:?}", path.display()))?;
    let mut parser = Parser::new_with_source(tokens, path.display().to_string());
    let (program, errors) = parser.parse_program();
    if errors.is_empty() {
        return Ok(program);
    }
    Err(format!(
        "parser errors in {}: {:?}",
        path.display(),
        errors[0].format_with_help()
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Span;
    use crate::module_artifact::{ExportKind, ModuleArtifact, ModuleExport};
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
        std::env::temp_dir().join(format!("agc-import-hook-{label}-{nonce}"))
    }

    #[test]
    fn lowers_file_imports_into_program_items() {
        let root = unique_temp_dir("file");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(root.join("std").join("io.ag"), "i32 imported() { return 1; }").unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        let result = hook
            .lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert!(result.module_artifacts.is_empty());
        assert!(program
            .items
            .iter()
            .all(|item| !matches!(item.kind, ast::ItemKind::Import(_))));
        assert!(program
            .items
            .iter()
            .any(|item| matches!(item.kind, ast::ItemKind::Function(_))));

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn preserves_program_attributes_from_imported_files() {
        let root = unique_temp_dir("file-attrs");
        std::fs::create_dir_all(root.join("std")).unwrap();
        std::fs::write(
            root.join("std").join("io.ag"),
            "#[link(m)] i32 imported() { return 1; }",
        )
        .unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        hook.lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert_eq!(program.attributes.len(), 1);
        assert_eq!(program.attributes[0].name.name, "link");

        let _ = std::fs::remove_dir_all(root);
    }

    #[test]
    fn collects_module_artifacts_for_agm_imports() {
        let root = unique_temp_dir("module");
        std::fs::create_dir_all(root.join("std")).unwrap();

        let artifact = ModuleArtifact {
            module_name: "io".to_string(),
            source_path: "std/io.ag".to_string(),
            source_hash_fnv1a64: 0,
            compiler_version: "test".to_string(),
            target_triple: "unknown".to_string(),
            exports: vec![ModuleExport {
                kind: ExportKind::Function,
                name: "print".to_string(),
                signature: "fn(str)->unit".to_string(),
            }],
            native_libs: vec!["c".to_string()],
        };
        std::fs::write(
            root.join("std").join("io.agm"),
            artifact.to_bytes().expect("artifact encoding should succeed"),
        )
        .unwrap();

        let mut program = import_program(&["std", "io"]);
        let mut loader = ModuleLoader::new();
        loader.add_search_dir(&root);
        let hook = FileImportResolverHook::new(&loader);
        let result = hook
            .lower_program_imports(&mut program, None, None)
            .expect("import lowering should succeed");

        assert_eq!(result.module_artifacts.len(), 1);
        assert_eq!(result.module_artifacts[0].module_name, "io");
        assert!(program.items.is_empty());

        let _ = std::fs::remove_dir_all(root);
    }
}
