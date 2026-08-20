//! Parallel Module Dependency Graph & Build Scheduler.
//!
//! Constructs a directed acyclic graph (DAG) of module dependencies, resolves
//! cache hits vs misses, and executes independent module compilations concurrently
//! using a Rayon thread pool with thread-isolated LLVM contexts.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use rayon::prelude::*;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};

use crate::cache_store::{CacheStore, CachedModule};
use crate::module_loader::{ModuleLoader, ResolvedSourceImportKind};

#[derive(Debug, Clone)]
pub struct ModuleNode {
    pub module_path: String,
    pub source_path: PathBuf,
    pub dependencies: Vec<String>,
}

#[derive(Debug, Default, Clone)]
pub struct DependencyGraph {
    pub nodes: HashMap<String, ModuleNode>,
}

impl DependencyGraph {
    pub fn new() -> Self {
        Self {
            nodes: HashMap::default(),
        }
    }

    /// Recursively discovers all transitive module dependencies starting from the root inputs.
    pub fn build(loader: &ModuleLoader, root_inputs: &[PathBuf]) -> Result<Self, String> {
        let mut graph = Self::new();
        let mut visited = HashSet::default();

        for input in root_inputs {
            graph.discover_module(loader, input, None, &mut visited)?;
        }

        Ok(graph)
    }

    fn discover_module(
        &mut self,
        loader: &ModuleLoader,
        source_path: &Path,
        module_path: Option<String>,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<Option<String>, String> {
        let canonical = std::fs::canonicalize(source_path).unwrap_or_else(|_| source_path.to_path_buf());
        if !visited.insert(canonical.clone()) {
            return Ok(module_path);
        }

        let src = match std::fs::read_to_string(source_path) {
            Ok(s) => s,
            Err(_) => return Ok(None),
        };

        let file_id = crate::lexer::register_source(&source_path.display().to_string(), &src);
        let tokens = match crate::lexer::lex_with_source(&src, file_id) {
            Ok(t) => t,
            Err(_) => return Ok(None),
        };

        let mut parser = crate::parser::Parser::new_with_source(tokens, source_path.display().to_string());
        let (ast, _) = parser.parse_program();

        let current_module_path = module_path.unwrap_or_else(|| {
            source_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("main")
                .to_string()
        });

        let mut direct_deps = Vec::new();
        for item in &ast.items {
            let crate::parser::ast::ItemKind::Import(import_item) = &item.kind else {
                continue;
            };
            let dep_path_str = crate::module_loader::import_path_to_string(&import_item.path);
            if let Some(resolved) = loader.find_source_import(&import_item.path, source_path.parent()) {
                if resolved.kind == ResolvedSourceImportKind::File {
                    if let Ok(Some(dep_mod)) = self.discover_module(
                        loader,
                        &resolved.source_path,
                        Some(dep_path_str.clone()),
                        visited,
                    ) {
                        direct_deps.push(dep_mod);
                    }
                }
            }
        }

        self.nodes.insert(
            current_module_path.clone(),
            ModuleNode {
                module_path: current_module_path.clone(),
                source_path: source_path.to_path_buf(),
                dependencies: direct_deps,
            },
        );

        Ok(Some(current_module_path))
    }

    /// Computes topological concurrency layers for parallel execution.
    /// Each layer contains modules whose dependencies have been satisfied in prior layers.
    pub fn topological_layers(&self) -> Result<Vec<Vec<String>>, String> {
        let mut in_degree: HashMap<String, usize> = HashMap::default();
        let mut dependents: HashMap<String, Vec<String>> = HashMap::default();

        for (name, node) in &self.nodes {
            in_degree.entry(name.clone()).or_insert(0);
            for dep in &node.dependencies {
                if self.nodes.contains_key(dep) {
                    *in_degree.entry(name.clone()).or_insert(0) += 1;
                    dependents
                        .entry(dep.clone())
                        .or_default()
                        .push(name.clone());
                }
            }
        }

        let mut layers = Vec::new();
        let mut current_layer: Vec<String> = in_degree
            .iter()
            .filter(|&(_, deg)| *deg == 0)
            .map(|(k, _)| k.clone())
            .collect();

        let mut processed = 0;
        while !current_layer.is_empty() {
            processed += current_layer.len();
            let mut next_layer = Vec::new();

            for node_name in &current_layer {
                if let Some(deps) = dependents.get(node_name) {
                    for dep in deps {
                        if let Some(deg) = in_degree.get_mut(dep) {
                            *deg -= 1;
                            if *deg == 0 {
                                next_layer.push(dep.clone());
                            }
                        }
                    }
                }
            }

            layers.push(current_layer);
            current_layer = next_layer;
        }

        if processed != self.nodes.len() {
            return Err("cyclic dependency detected in module import graph".to_string());
        }

        Ok(layers)
    }
}

// ===========================================================================
// Parallel Build Graph Executor
// ===========================================================================

pub struct ParallelBuildReport {
    pub total_modules: usize,
    pub cache_hits: usize,
    pub compiled_modules: usize,
    pub object_artifacts: Vec<PathBuf>,
}

pub struct ParallelGraphExecutor<'a> {
    pub graph: &'a DependencyGraph,
    pub loader: &'a ModuleLoader,
    pub store: &'a CacheStore,
    pub jobs: usize,
}

impl<'a> ParallelGraphExecutor<'a> {
    pub fn new(
        graph: &'a DependencyGraph,
        loader: &'a ModuleLoader,
        store: &'a CacheStore,
        jobs: usize,
    ) -> Self {
        Self {
            graph,
            loader,
            store,
            jobs: if jobs == 0 { num_cpus() } else { jobs },
        }
    }

    pub fn execute(&self) -> Result<ParallelBuildReport, Vec<String>> {
        let layers = self
            .graph
            .topological_layers()
            .map_err(|e| vec![e])?;

        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(self.jobs)
            .build()
            .map_err(|e| vec![format!("failed to initialize worker thread pool: {e}")])?;

        let failed = AtomicBool::new(false);
        let errors = Arc::new(Mutex::new(Vec::new()));
        let mut total_hits = 0;
        let mut total_compiled = 0;
        let mut object_artifacts = Vec::new();

        pool.install(|| {
            for layer in layers {
                if failed.load(Ordering::Relaxed) {
                    break;
                }

                // Partition layer into cache hits vs misses
                let mut layer_misses = Vec::new();
                for module_name in layer {
                    if let Some(node) = self.graph.nodes.get(&module_name) {
                        if let Some(cached) = self.loader.get_cached_module(&node.source_path, &node.module_path) {
                            total_hits += 1;
                            object_artifacts.push(cached.obj_path);
                        } else {
                            layer_misses.push(node.clone());
                        }
                    }
                }

                if layer_misses.is_empty() {
                    continue;
                }

                let errors_clone = errors.clone();
                let results: Vec<Result<CachedModule, String>> = layer_misses
                    .par_iter()
                    .map(|node| {
                        if failed.load(Ordering::Relaxed) {
                            return Err("build aborted due to previous error".to_string());
                        }
                        self.compile_single_module(node)
                    })
                    .collect();

                for res in results {
                    match res {
                        Ok(cached) => {
                            total_compiled += 1;
                            object_artifacts.push(cached.obj_path);
                        }
                        Err(err) => {
                            failed.store(true, Ordering::Relaxed);
                            errors_clone.lock().unwrap().push(err);
                        }
                    }
                }
            }
        });

        let recorded_errors = errors.lock().unwrap().clone();
        if !recorded_errors.is_empty() {
            return Err(recorded_errors);
        }

        Ok(ParallelBuildReport {
            total_modules: self.graph.nodes.len(),
            cache_hits: total_hits,
            compiled_modules: total_compiled,
            object_artifacts,
        })
    }

    fn compile_single_module(&self, node: &ModuleNode) -> Result<CachedModule, String> {
        let key = self
            .loader
            .compute_cache_key(&node.source_path, &node.module_path)
            .ok_or_else(|| format!("failed to generate cache key for {}", node.source_path.display()))?;

        if let Some(cached) = self.store.get(&key) {
            return Ok(cached);
        }

        // Fresh thread-local compilation context
        let src = std::fs::read_to_string(&node.source_path)
            .map_err(|e| format!("failed to read {}: {e}", node.source_path.display()))?;

        let file_id = crate::lexer::register_source(&node.source_path.display().to_string(), &src);
        let tokens = crate::lexer::lex_with_source(&src, file_id)
            .map_err(|e| format!("lexer error in {}: {e:?}", node.source_path.display()))?;

        let mut parser = crate::parser::Parser::new_with_source(tokens, node.source_path.display().to_string());
        let (mut ast, errors) = parser.parse_program();
        if !errors.is_empty() {
            return Err(format!("syntax error in {}", node.source_path.display()));
        }

        let import_resolver = crate::parser::FileImportResolverHook::new(self.loader)
            .with_entry_import(false);
        let import_lowering = import_resolver
            .lower_program_imports(&mut ast, node.source_path.parent(), Some(&node.source_path))?;

        let mut cfg_set = crate::cfg::CfgSet::parse(&self.loader.cfg_flags);
        crate::cfg::add_derived_cfgs(
            &mut cfg_set,
            self.loader.opt_level.as_deref(),
            self.loader.target.as_deref(),
        );
        crate::cfg::gate_items(&mut ast, &cfg_set);
        crate::semantic::cfg_hook::fold_and_prune(&mut ast, &cfg_set);

        let mut symbol_table = crate::symbol_table::CompilerSymbolTable::new();
        symbol_table.record_program_symbols(&ast, crate::symbol_table::CompilerPhase::Parse);
        crate::driver::run_semantic_hooks(&mut ast, &mut symbol_table, &import_lowering.module_artifacts);

        crate::semantic::typeck::TypeChecker::resolve_type_aliases_in_program(&mut ast);
        let mut checker = crate::semantic::typeck::TypeChecker::new().with_imported_modules(&import_lowering.module_artifacts);
        let (type_errors, monomorphs) = checker.check_program_with_table(&ast, &mut symbol_table);
        if !type_errors.is_empty() {
            return Err(format!("type errors in {}", node.source_path.display()));
        }

        crate::semantic::monomorph::append_monomorphs(&mut ast, &monomorphs, &import_lowering.module_artifacts);

        // Mark library items public
        let lib_file = crate::lexer::register_source(node.source_path.to_str().unwrap_or_default(), &src);
        for item in &mut ast.items {
            if item.span.file == lib_file && matches!(item.visibility, crate::parser::ast::Visibility::Private) {
                item.visibility = crate::parser::ast::Visibility::Public;
            }
        }

        let target_triple = self.loader.target.clone().unwrap_or_else(|| {
            inkwell::targets::TargetMachine::get_default_triple()
                .as_str()
                .to_str()
                .unwrap_or("x86_64-unknown-linux-gnu")
                .to_string()
        });

        let artifact = crate::module_artifact::ModuleArtifact::from_program(
            node.module_path.clone(),
            node.module_path.clone(),
            node.source_path.display().to_string(),
            &src,
            &ast,
            target_triple,
            crate::module_artifact::ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            import_lowering.module_dependencies,
            import_lowering.transitive_module_deps,
            Vec::new(),
        );

        let agm_bytes = artifact.to_bytes().map_err(|e| format!("failed to encode .agm: {e}"))?;

        let pid = std::process::id();
        let temp_o = std::env::temp_dir().join(format!("par_mod_{pid}_{}.o", key.hash_hex));
        crate::codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table_and_source_with_leak_check(
            &ast,
            &import_lowering.module_artifacts,
            &temp_o,
            self.loader.target.as_deref(),
            self.loader.opt_level.as_deref(),
            &mut symbol_table,
            Some(&node.source_path),
            Some(&src),
            self.loader.debug_info,
            self.loader.leak_check,
        ).map_err(|e| format!("LLVM codegen error in {}: {}", node.source_path.display(), e.message))?;

        let obj_bytes = std::fs::read(&temp_o).map_err(|e| format!("failed to read temp object: {e}"))?;
        let _ = std::fs::remove_file(&temp_o);

        let cached = self.store.put(&key, &agm_bytes, &obj_bytes).map_err(|e| format!("failed to write cache: {e}"))?;
        Ok(cached)
    }
}

fn num_cpus() -> usize {
    std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(4)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_topological_layers_independent() {
        let mut graph = DependencyGraph::new();
        graph.nodes.insert(
            "a".to_string(),
            ModuleNode {
                module_path: "a".to_string(),
                source_path: PathBuf::from("a.ag"),
                dependencies: vec![],
            },
        );
        graph.nodes.insert(
            "b".to_string(),
            ModuleNode {
                module_path: "b".to_string(),
                source_path: PathBuf::from("b.ag"),
                dependencies: vec![],
            },
        );

        let layers = graph.topological_layers().unwrap();
        assert_eq!(layers.len(), 1);
        assert_eq!(layers[0].len(), 2);
    }

    #[test]
    fn test_topological_layers_dependencies() {
        let mut graph = DependencyGraph::new();
        graph.nodes.insert(
            "leaf1".to_string(),
            ModuleNode {
                module_path: "leaf1".to_string(),
                source_path: PathBuf::from("leaf1.ag"),
                dependencies: vec![],
            },
        );
        graph.nodes.insert(
            "leaf2".to_string(),
            ModuleNode {
                module_path: "leaf2".to_string(),
                source_path: PathBuf::from("leaf2.ag"),
                dependencies: vec![],
            },
        );
        graph.nodes.insert(
            "mid".to_string(),
            ModuleNode {
                module_path: "mid".to_string(),
                source_path: PathBuf::from("mid.ag"),
                dependencies: vec!["leaf1".to_string(), "leaf2".to_string()],
            },
        );
        graph.nodes.insert(
            "root".to_string(),
            ModuleNode {
                module_path: "root".to_string(),
                source_path: PathBuf::from("root.ag"),
                dependencies: vec!["mid".to_string()],
            },
        );

        let layers = graph.topological_layers().unwrap();
        assert_eq!(layers.len(), 3);
        assert_eq!(layers[0].len(), 2); // leaf1 and leaf2 in parallel
        assert_eq!(layers[1], vec!["mid".to_string()]); // mid in second layer
        assert_eq!(layers[2], vec!["root".to_string()]); // root in third layer
    }

    #[test]
    fn test_topological_cycle_detection() {
        let mut graph = DependencyGraph::new();
        graph.nodes.insert(
            "a".to_string(),
            ModuleNode {
                module_path: "a".to_string(),
                source_path: PathBuf::from("a.ag"),
                dependencies: vec!["b".to_string()],
            },
        );
        graph.nodes.insert(
            "b".to_string(),
            ModuleNode {
                module_path: "b".to_string(),
                source_path: PathBuf::from("b.ag"),
                dependencies: vec!["a".to_string()],
            },
        );

        assert!(graph.topological_layers().is_err());
    }
}
