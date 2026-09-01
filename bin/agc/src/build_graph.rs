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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct CodegenElements {
    pub functions: usize,
    pub methods: usize,
    pub structs: usize,
    pub enums: usize,
    pub globals: usize,
    pub traits: usize,
    pub type_aliases: usize,
}

impl CodegenElements {
    pub fn from_program(program: &crate::parser::ast::Program) -> Self {
        let mut elements = Self::default();
        for item in &program.items {
            match &item.kind {
                crate::parser::ast::ItemKind::Function(_) => elements.functions += 1,
                crate::parser::ast::ItemKind::Struct(_) => elements.structs += 1,
                crate::parser::ast::ItemKind::Enum(_) => elements.enums += 1,
                crate::parser::ast::ItemKind::GlobalVariable(_) => elements.globals += 1,
                crate::parser::ast::ItemKind::Trait(_) => elements.traits += 1,
                crate::parser::ast::ItemKind::TypeAlias(_) => elements.type_aliases += 1,
                crate::parser::ast::ItemKind::Impl(impl_item) => {
                    elements.methods += impl_item.items.len();
                }
                _ => {}
            }
        }
        elements
    }

    pub fn total(&self) -> usize {
        self.functions
            + self.methods
            + self.structs
            + self.enums
            + self.globals
            + self.traits
            + self.type_aliases
    }

    pub fn add(&mut self, other: &Self) {
        self.functions += other.functions;
        self.methods += other.methods;
        self.structs += other.structs;
        self.enums += other.enums;
        self.globals += other.globals;
        self.traits += other.traits;
        self.type_aliases += other.type_aliases;
    }

    pub fn summary(&self) -> String {
        let total = self.total();
        let mut parts = Vec::new();
        if self.functions > 0 {
            parts.push(format!("{} fns", self.functions));
        }
        if self.methods > 0 {
            parts.push(format!("{} methods", self.methods));
        }
        if self.structs > 0 {
            parts.push(format!("{} structs", self.structs));
        }
        if self.enums > 0 {
            parts.push(format!("{} enums", self.enums));
        }
        if self.globals > 0 {
            parts.push(format!("{} globals", self.globals));
        }
        if self.traits > 0 {
            parts.push(format!("{} traits", self.traits));
        }
        if parts.is_empty() {
            format!("{total} items")
        } else {
            format!("{total} items ({})", parts.join(", "))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ModuleNode {
    pub module_path: String,
    pub source_path: PathBuf,
    pub dependencies: Vec<String>,
    pub codegen_elements: CodegenElements,
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

    pub fn total_codegen_elements(&self) -> CodegenElements {
        let mut total = CodegenElements::default();
        for node in self.nodes.values() {
            total.add(&node.codegen_elements);
        }
        total
    }

    pub fn display_graph(&self) {
        use owo_colors::OwoColorize;

        let total_elem = self.total_codegen_elements();
        eprintln!(
            "{} ({} modules, {})",
            "Module Build Graph".bold().cyan(),
            self.nodes.len().to_string().bold(),
            total_elem.summary().dimmed(),
        );

        let layers = match self.topological_layers() {
            Ok(l) => l,
            Err(_) => {
                for (name, node) in &self.nodes {
                    eprintln!(
                        "  • {} ({})",
                        name.bold(),
                        node.codegen_elements.summary().dimmed()
                    );
                }
                return;
            }
        };

        for (layer_idx, layer) in layers.iter().enumerate() {
            let layer_title = format!("Layer {layer_idx}");
            eprintln!("  {}", layer_title.bold().yellow());
            for (idx, mod_name) in layer.iter().enumerate() {
                let is_last = idx + 1 == layer.len();
                let prefix = if is_last { "    └── " } else { "    ├── " };
                let node = self.nodes.get(mod_name);
                let elem_str = node
                    .map(|n| format!(" ({})", n.codegen_elements.summary()))
                    .unwrap_or_default();
                let deps_str = node
                    .filter(|n| !n.dependencies.is_empty())
                    .map(|n| format!(" [deps: {}]", n.dependencies.join(", ")))
                    .unwrap_or_default();

                eprintln!(
                    "{}{}{}{}",
                    prefix.dimmed(),
                    mod_name.bold().green(),
                    elem_str.dimmed(),
                    deps_str.dimmed()
                );
            }
        }
        eprintln!();
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
        let (ast, errors) = parser.parse_program();
        if !errors.is_empty() {
            for error in &errors {
                eprintln!(
                    "{}",
                    crate::diagnostics::render(
                        *error.span(),
                        &error.to_string(),
                        crate::diagnostics::Severity::Error,
                    )
                );
            }
            return Err(format!("syntax error in {}", source_path.display()));
        }
        let codegen_elements = CodegenElements::from_program(&ast);

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

        let p_str = source_path.to_str().unwrap_or("");
        if p_str.contains("std/")
            && !p_str.ends_with("std/atomic.ag")
            && !p_str.ends_with("std/ops.ag")
        {
            return Ok(Some(current_module_path));
        }

        self.nodes.insert(
            current_module_path.clone(),
            ModuleNode {
                module_path: current_module_path.clone(),
                source_path: source_path.to_path_buf(),
                dependencies: direct_deps,
                codegen_elements,
            },
        );

        Ok(Some(current_module_path))
    }

    /// Finds all strongly connected components (SCCs) using Tarjan's algorithm.
    pub fn strongly_connected_components(&self) -> Vec<Vec<String>> {
        let mut index = 0usize;
        let mut stack = Vec::new();
        let mut indices: HashMap<String, usize> = HashMap::default();
        let mut lowlink: HashMap<String, usize> = HashMap::default();
        let mut on_stack: HashSet<String> = HashSet::default();
        let mut sccs = Vec::new();

        for node_name in self.nodes.keys() {
            if !indices.contains_key(node_name) {
                self.strongconnect(
                    node_name,
                    &mut index,
                    &mut stack,
                    &mut indices,
                    &mut lowlink,
                    &mut on_stack,
                    &mut sccs,
                );
            }
        }

        sccs
    }

    fn strongconnect(
        &self,
        node_name: &str,
        index: &mut usize,
        stack: &mut Vec<String>,
        indices: &mut HashMap<String, usize>,
        lowlink: &mut HashMap<String, usize>,
        on_stack: &mut HashSet<String>,
        sccs: &mut Vec<Vec<String>>,
    ) {
        indices.insert(node_name.to_string(), *index);
        lowlink.insert(node_name.to_string(), *index);
        *index += 1;
        stack.push(node_name.to_string());
        on_stack.insert(node_name.to_string());

        if let Some(node) = self.nodes.get(node_name) {
            for dep in &node.dependencies {
                if self.nodes.contains_key(dep) {
                    if !indices.contains_key(dep) {
                        self.strongconnect(
                            dep,
                            index,
                            stack,
                            indices,
                            lowlink,
                            on_stack,
                            sccs,
                        );
                        let dep_low = lowlink[dep];
                        let cur_low = lowlink.get_mut(node_name).unwrap();
                        *cur_low = (*cur_low).min(dep_low);
                    } else if on_stack.contains(dep) {
                        let dep_idx = indices[dep];
                        let cur_low = lowlink.get_mut(node_name).unwrap();
                        *cur_low = (*cur_low).min(dep_idx);
                    }
                }
            }
        }

        if lowlink[node_name] == indices[node_name] {
            let mut scc = Vec::new();
            while let Some(w) = stack.pop() {
                on_stack.remove(&w);
                scc.push(w.clone());
                if w == node_name {
                    break;
                }
            }
            sccs.push(scc);
        }
    }

    /// Computes topological concurrency layers for parallel execution using the condensation DAG.
    /// Each layer contains modules whose dependencies have been satisfied in prior layers.
    pub fn topological_layers(&self) -> Result<Vec<Vec<String>>, String> {
        let sccs = self.strongly_connected_components();
        let mut node_to_scc: HashMap<String, usize> = HashMap::default();
        for (scc_idx, scc) in sccs.iter().enumerate() {
            for node in scc {
                node_to_scc.insert(node.clone(), scc_idx);
            }
        }

        let mut scc_in_degree: HashMap<usize, usize> = HashMap::default();
        let mut scc_dependents: HashMap<usize, HashSet<usize>> = HashMap::default();

        for (scc_idx, scc) in sccs.iter().enumerate() {
            scc_in_degree.entry(scc_idx).or_insert(0);
            for node in scc {
                if let Some(node_item) = self.nodes.get(node) {
                    for dep in &node_item.dependencies {
                        if let Some(&dep_scc) = node_to_scc.get(dep) {
                            if dep_scc != scc_idx && scc_dependents.entry(dep_scc).or_default().insert(scc_idx) {
                                *scc_in_degree.entry(scc_idx).or_insert(0) += 1;
                            }
                        }
                    }
                }
            }
        }

        let mut layers = Vec::new();
        let mut current_sccs: Vec<usize> = scc_in_degree
            .iter()
            .filter(|&(_, deg)| *deg == 0)
            .map(|(&idx, _)| idx)
            .collect();

        while !current_sccs.is_empty() {
            let mut layer_modules = Vec::new();
            let mut next_sccs = Vec::new();

            for &scc_idx in &current_sccs {
                layer_modules.extend(sccs[scc_idx].clone());
                if let Some(deps) = scc_dependents.get(&scc_idx) {
                    for &dep_scc in deps {
                        if let Some(deg) = scc_in_degree.get_mut(&dep_scc) {
                            *deg -= 1;
                            if *deg == 0 {
                                next_sccs.push(dep_scc);
                            }
                        }
                    }
                }
            }

            layers.push(layer_modules);
            current_sccs = next_sccs;
        }

        Ok(layers)
    }
}

// ===========================================================================
// Build Progress Visualizer
// ===========================================================================

pub struct BuildProgress {
    total_steps: usize,
    completed: std::sync::atomic::AtomicUsize,
    is_terminal: bool,
    enabled: bool,
    verbose: bool,
    start_time: std::time::Instant,
    active: std::sync::Mutex<HashSet<String>>,
}

impl BuildProgress {
    pub fn new(total_steps: usize, enabled: bool, verbose: bool) -> Self {
        use std::io::IsTerminal;
        let is_terminal = std::io::stderr().is_terminal();
        Self {
            total_steps,
            completed: std::sync::atomic::AtomicUsize::new(0),
            is_terminal,
            enabled,
            verbose,
            start_time: std::time::Instant::now(),
            active: std::sync::Mutex::new(HashSet::default()),
        }
    }

    pub fn on_start(&self, module_name: &str, elements: &CodegenElements) {
        if !self.enabled && !self.verbose {
            return;
        }
        let mut active = self.active.lock().unwrap();
        active.insert(module_name.to_string());
        let current = self.completed.load(Ordering::Relaxed) + 1;
        let total = self.total_steps;
        let percent = if total > 0 { (current * 100) / total } else { 100 };

        use owo_colors::OwoColorize;
        let step_prefix = format!("[{:>2}/{:<2}] {:>3}%", current, total, percent);
        let active_list = if active.len() > 1 {
            let names: Vec<_> = active.iter().cloned().collect();
            format!(" [active: {}]", names.join(", "))
        } else {
            String::new()
        };

        if self.is_terminal && !self.verbose {
            eprint!(
                "\r\x1b[2K{} {} {}{}{}",
                step_prefix.dimmed(),
                "compiling".bold().cyan(),
                module_name.bold(),
                format!(" ({})", elements.summary()).dimmed(),
                active_list.dimmed()
            );
            let _ = std::io::Write::flush(&mut std::io::stderr());
        } else if self.verbose {
            eprintln!(
                "{} {} {}{}",
                step_prefix.dimmed(),
                "compiling".bold().cyan(),
                module_name.bold(),
                format!(" ({})", elements.summary()).dimmed()
            );
        }
    }

    pub fn on_finish(
        &self,
        module_name: &str,
        elements: &CodegenElements,
        is_cached: bool,
        duration: std::time::Duration,
    ) {
        let mut active = self.active.lock().unwrap();
        active.remove(module_name);
        let current = self.completed.fetch_add(1, Ordering::Relaxed) + 1;
        let total = self.total_steps;
        let percent = if total > 0 { (current * 100) / total } else { 100 };

        if !self.enabled && !self.verbose {
            return;
        }

        use owo_colors::OwoColorize;
        let step_prefix = format!("[{:>2}/{:<2}] {:>3}%", current, total, percent);
        let status = if is_cached {
            "[cached]".bold().green().to_string()
        } else {
            format!("{} in {:.1?}", "compiled".bold().green(), duration)
        };

        if self.is_terminal && !self.verbose {
            eprintln!(
                "\r\x1b[2K{} {} {}{}",
                step_prefix.dimmed(),
                status,
                module_name.bold(),
                format!(" ({})", elements.summary()).dimmed()
            );
        } else {
            eprintln!(
                "{} {} {}{}",
                step_prefix.dimmed(),
                status,
                module_name.bold(),
                format!(" ({})", elements.summary()).dimmed()
            );
        }
    }

    pub fn on_link(&self, target_path: &Path) {
        if !self.enabled && !self.verbose {
            return;
        }
        let current = self.total_steps;
        let total = self.total_steps;
        use owo_colors::OwoColorize;
        let step_prefix = format!("[{:>2}/{:<2}] 100%", current, total);
        if self.is_terminal && !self.verbose {
            eprintln!(
                "\r\x1b[2K{} {} {}",
                step_prefix.dimmed(),
                "linking".bold().magenta(),
                target_path.display().to_string().bold()
            );
        } else {
            eprintln!(
                "{} {} {}",
                step_prefix.dimmed(),
                "linking".bold().magenta(),
                target_path.display().to_string().bold()
            );
        }
    }

    pub fn on_complete(
        &self,
        total_modules: usize,
        total_elements: &CodegenElements,
        cached_count: usize,
        compiled_count: usize,
    ) {
        if !self.enabled && !self.verbose {
            return;
        }
        use owo_colors::OwoColorize;
        let elapsed = self.start_time.elapsed();
        eprintln!(
            "{} in {:.2?} ({} modules, {}, {} cached, {} compiled)",
            "Build finished".bold().green(),
            elapsed,
            total_modules.to_string().bold(),
            total_elements.summary().bold(),
            cached_count.to_string().green(),
            compiled_count.to_string().cyan()
        );
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
    pub progress: Option<Arc<BuildProgress>>,
}

impl<'a> ParallelGraphExecutor<'a> {
    pub fn new(
        graph: &'a DependencyGraph,
        loader: &'a ModuleLoader,
        store: &'a CacheStore,
        jobs: usize,
        progress: Option<Arc<BuildProgress>>,
    ) -> Self {
        Self {
            graph,
            loader,
            store,
            jobs: if jobs == 0 { num_cpus() } else { jobs },
            progress,
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
                            if let Some(p) = &self.progress {
                                p.on_finish(&node.module_path, &node.codegen_elements, true, std::time::Duration::ZERO);
                            }
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
        let start = std::time::Instant::now();
        if let Some(p) = &self.progress {
            p.on_start(&node.module_path, &node.codegen_elements);
        }
        let res = self.compile_single_module_inner(node);
        let elapsed = start.elapsed();
        if let Some(p) = &self.progress {
            if res.is_ok() {
                p.on_finish(&node.module_path, &node.codegen_elements, false, elapsed);
            }
        }
        res
    }

    fn compile_single_module_inner(&self, node: &ModuleNode) -> Result<CachedModule, String> {
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
            eprint!("\r\x1b[2K");
            for error in &errors {
                eprintln!(
                    "{}",
                    crate::diagnostics::render(
                        *error.span(),
                        &error.to_string(),
                        crate::diagnostics::Severity::Error,
                    )
                );
            }
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
        crate::semantic::serialize::synthesize_serialization_for_program(&mut ast);

        let mut symbol_table = crate::symbol_table::CompilerSymbolTable::new();
        symbol_table.record_program_symbols(&ast, crate::symbol_table::CompilerPhase::Parse);
        crate::driver::run_semantic_hooks(&mut ast, &mut symbol_table, &import_lowering.module_artifacts);

        crate::semantic::typeck::TypeChecker::resolve_type_aliases_in_program(&mut ast);
        let mut checker = crate::semantic::typeck::TypeChecker::new().with_imported_modules(&import_lowering.module_artifacts);
        let (type_errors, monomorphs) = checker.check_program_with_table(&ast, &mut symbol_table);
        if !type_errors.is_empty() {
            eprint!("\r\x1b[2K");
            for error in &type_errors {
                eprintln!(
                    "{}",
                    crate::diagnostics::render(
                        error.span,
                        &error.message,
                        crate::diagnostics::Severity::Error,
                    )
                );
            }
            return Err(format!("type errors in {}", node.source_path.display()));
        }

        let resolved_iter_types = checker.take_resolved_iter_types();
        if !resolved_iter_types.is_empty() {
            crate::semantic::typeck::populate_for_in_iterator_types(&mut ast, &resolved_iter_types);
        }

        let bare_constructors = checker.take_bare_constructors();
        if !bare_constructors.is_empty() {
            crate::semantic::typeck::rewrite_bare_constructors(&mut ast, &bare_constructors);
        }

        let inferred_lets = checker.take_inferred_lets();
        if !inferred_lets.is_empty() {
            crate::semantic::typeck::populate_inferred_let_types(&mut ast, &inferred_lets);
        }

        let mut monomorphs = monomorphs;
        crate::semantic::monomorph::refresh_monomorph_bodies(&mut monomorphs, &ast);
        crate::semantic::monomorph::append_monomorphs(&mut ast, &monomorphs, &import_lowering.module_artifacts);
        let mut post_checker = crate::semantic::typeck::TypeChecker::new().with_imported_modules(&import_lowering.module_artifacts);
        let _ = post_checker.check_program_with_table(&ast, &mut symbol_table);
        let post_bare = post_checker.take_bare_constructors();
        if !post_bare.is_empty() {
            crate::semantic::typeck::rewrite_bare_constructors(&mut ast, &post_bare);
        }

        // Mark library items public and inlined non-library items private
        let lib_file = file_id;
        for item in &mut ast.items {
            if item.span.file == lib_file {
                item.visibility = crate::parser::ast::Visibility::Public;
                if let crate::parser::ast::ItemKind::Impl(impl_item) = &mut item.kind {
                    for member in &mut impl_item.items {
                        if let crate::parser::ast::ImplItemKind::Function(func) = member {
                            func.visibility = crate::parser::ast::Visibility::Public;
                        }
                    }
                }
            } else {
                item.visibility = crate::parser::ast::Visibility::Private;
                if let crate::parser::ast::ItemKind::Impl(impl_item) = &mut item.kind {
                    for member in &mut impl_item.items {
                        if let crate::parser::ast::ImplItemKind::Function(func) = member {
                            func.visibility = crate::parser::ast::Visibility::Private;
                        }
                    }
                }
            }
        }

        let target_triple = self.loader.target.clone().unwrap_or_else(|| {
            inkwell::targets::TargetMachine::get_default_triple()
                .as_str()
                .to_str()
                .unwrap_or("x86_64-unknown-linux-gnu")
                .to_string()
        });

        let mut module_native_libs = crate::attributes::collect_program_link_libraries(&ast).unwrap_or_default();
        for imp_art in &import_lowering.module_artifacts {
            for lib in &imp_art.native_libs {
                if !module_native_libs.contains(lib) {
                    module_native_libs.push(lib.clone());
                }
            }
        }

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
            module_native_libs,
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
        ).map_err(|e| {
            eprint!("\r\x1b[2K");
            if let Some(span) = e.span {
                eprintln!(
                    "{}",
                    crate::diagnostics::render(
                        span,
                        &e.message,
                        crate::diagnostics::Severity::Error,
                    )
                );
            } else {
                eprintln!("error: {}", e.message);
            }
            format!("LLVM codegen error in {}: {}", node.source_path.display(), e.message)
        })?;

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
                codegen_elements: CodegenElements::default(),
            },
        );
        graph.nodes.insert(
            "b".to_string(),
            ModuleNode {
                module_path: "b".to_string(),
                source_path: PathBuf::from("b.ag"),
                dependencies: vec![],
                codegen_elements: CodegenElements::default(),
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
                codegen_elements: CodegenElements::default(),
            },
        );
        graph.nodes.insert(
            "leaf2".to_string(),
            ModuleNode {
                module_path: "leaf2".to_string(),
                source_path: PathBuf::from("leaf2.ag"),
                dependencies: vec![],
                codegen_elements: CodegenElements::default(),
            },
        );
        graph.nodes.insert(
            "mid".to_string(),
            ModuleNode {
                module_path: "mid".to_string(),
                source_path: PathBuf::from("mid.ag"),
                dependencies: vec!["leaf1".to_string(), "leaf2".to_string()],
                codegen_elements: CodegenElements::default(),
            },
        );
        graph.nodes.insert(
            "root".to_string(),
            ModuleNode {
                module_path: "root".to_string(),
                source_path: PathBuf::from("root.ag"),
                dependencies: vec!["mid".to_string()],
                codegen_elements: CodegenElements::default(),
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
                codegen_elements: CodegenElements::default(),
            },
        );
        graph.nodes.insert(
            "b".to_string(),
            ModuleNode {
                module_path: "b".to_string(),
                source_path: PathBuf::from("b.ag"),
                dependencies: vec!["a".to_string()],
                codegen_elements: CodegenElements::default(),
            },
        );

        let layers = graph.topological_layers().unwrap();
        assert_eq!(layers.len(), 1);
        assert_eq!(layers[0].len(), 2);
    }
}
