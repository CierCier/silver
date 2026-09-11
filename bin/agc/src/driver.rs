//! Compiler driver: CLI planning and pipeline orchestration.
//!
//! Owns [`Cli`], [`EmitKind`], [`CompilePlan`] and the per-emit pipeline (lex,
//! parse, import lowering, semantic analysis, type check, monomorph,
//! codegen, link). The linker itself lives in `crate::link`.

use std::env;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::attributes::{collect_program_link_libraries, extend_unique_libs};
use crate::module_artifact::{
    ModuleArtifact, ModuleCodeArtifacts, hash_source_text, module_name_from_path,
};
use crate::module_loader::{ModuleLoader, module_loader_default_dirs};
use crate::package;
use crate::parser::ast;
use crate::semantic::{
    self,
    analyzer::{Analyzer, SemanticAnalyzerHook},
    comptime_cast_hook::ComptimeCastHook,
    typeck::TypeChecker,
};
use crate::symbol_table::{CompilerPhase, CompilerSymbolTable};
use crate::{ast_tree, cfg, codegen, diagnostics, lexer, parser, profiler};
use clap::builder::styling::{AnsiColor, Styles};
use clap::{ArgAction, Parser, ValueEnum};
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use owo_colors::OwoColorize;

use crate::link::{link_exe, link_shared_module};

/// Command-line options for the `agc` driver binary.
#[derive(Parser, Debug)]
#[command(
    name = "agc",
    version = concat!(env!("CARGO_PKG_VERSION"), "  ", env!("GIT_DESCRIBE"), " (", env!("GIT_SHA"), ")"),
    about = "Silver compiler — LLVM-backed systems language",
    override_usage = "agc [OPTIONS] <FILE>... [-o <OUT>]\n       agc [OPTIONS] <COMMAND>",
    help_template = "\
{before-help}{about} {version}

{usage-heading} {usage}

Commands:
  init        Create a new Silver package
  build, b    Compile and link an executable (default)
  run, r      Compile and immediately execute the output binary
  check, c    Analyze source files and report errors without codegen or linking
  clean       Remove cached compiler build artifacts
  test, t     Compile and run tests for a package or file

{all-args}{after-help}",
    styles = Styles::styled()
        .header(AnsiColor::Green.on_default().bold())
        .usage(AnsiColor::Green.on_default().bold())
        .literal(AnsiColor::Cyan.on_default().bold())
        .placeholder(AnsiColor::Cyan.on_default())
        .valid(AnsiColor::Green.on_default())
        .invalid(AnsiColor::Red.on_default().bold())
        .error(AnsiColor::Red.on_default().bold()),
    color = clap::ColorChoice::Always,
    after_help = "\
Examples:
  agc run examples/control_flow.ag          run a file
  agc check std/string.ag                   typecheck only
  agc -O2 -o app src/main.ag                 optimized build
  agc --emit-llvm -o - src/main.ag | opt -S  inspect IR

See 'https://github.com/CierCier/silver' for documentation.",
)]
pub struct Cli {
    /// Input source files (.ag). Optional for --emit=grammar or --clean.
    #[arg(value_name = "FILE", help_heading = "Arguments")]
    inputs: Vec<PathBuf>,

    /// Write output to <file>
    #[arg(
        short = 'o',
        long = "output",
        value_name = "FILE",
        help_heading = "Output & Compilation"
    )]
    output: Option<PathBuf>,

    /// Compile and assemble, but do not link (emit .o)
    #[arg(short = 'c', action = ArgAction::SetTrue, help_heading = "Output & Compilation")]
    compile_only: bool,

    /// Compile only and emit assembly (.s)
    #[arg(short = 'S', action = ArgAction::SetTrue, help_heading = "Output & Compilation")]
    emit_asm: bool,

    /// Emit LLVM IR instead of native output (.ll)
    #[arg(long = "emit-llvm", action = ArgAction::SetTrue, help_heading = "Output & Compilation")]
    emit_llvm: bool,

    /// Explicitly select the output kind (overrides -c/-S/--emit-llvm)
    #[arg(
        long = "emit",
        value_enum,
        value_name = "KIND",
        help_heading = "Output & Compilation"
    )]
    emit: Option<EmitKind>,

    /// Optimization level: 0,1,2,3,s,z,fast (accepts clang-style -O2)
    #[arg(short = 'O', value_name = "LEVEL", default_missing_value = "2", num_args = 0..=1, help_heading = "Output & Compilation")]
    opt_level: Option<String>,

    /// Generate debug information (DWARF). On by default for unoptimized builds.
    #[arg(short = 'g', action = ArgAction::SetTrue, help_heading = "Output & Compilation")]
    debug_info: bool,

    /// Disable debug information (clang-style -g0)
    #[arg(long = "g0", action = ArgAction::SetTrue, help_heading = "Output & Compilation")]
    no_debug_info: bool,

    /// Add directory to include search path
    #[arg(short = 'I', value_name = "DIR", action = ArgAction::Append, help_heading = "Search Paths & Linking")]
    include_dirs: Vec<PathBuf>,

    /// Add a primary module include root (defaults to current working directory)
    #[arg(
        long = "root",
        value_name = "DIR",
        help_heading = "Search Paths & Linking"
    )]
    root: Option<PathBuf>,

    /// Select a package binary target (defaults to the package's name or sole binary).
    #[arg(
        long = "bin",
        value_name = "NAME",
        num_args = 0..=1,
        default_missing_value = "",
        conflicts_with = "lib",
        help_heading = "Package"
    )]
    bin: Option<String>,

    /// Select a package library target (defaults to the package's name or sole library).
    #[arg(
        long = "lib",
        value_name = "NAME",
        num_args = 0..=1,
        default_missing_value = "",
        conflicts_with = "bin",
        help_heading = "Package"
    )]
    lib: Option<String>,

    /// Initialize a package in the current directory or a target directory.
    #[arg(long = "init", action = ArgAction::SetTrue, hide = true)]
    init: bool,

    /// Override the package name used by `init`.
    #[arg(long = "name", value_name = "NAME", help_heading = "Package")]
    init_name: Option<String>,

    /// Define a preprocessor symbol (accepted for clang-compat; not yet used)
    #[arg(short = 'D', value_name = "NAME[=VALUE]", action = ArgAction::Append, help_heading = "Search Paths & Linking")]
    defines: Vec<String>,

    /// Add directory to library search path
    #[arg(short = 'L', value_name = "DIR", action = ArgAction::Append, help_heading = "Search Paths & Linking")]
    lib_dirs: Vec<PathBuf>,

    /// Link with library
    #[arg(short = 'l', value_name = "LIB", action = ArgAction::Append, help_heading = "Search Paths & Linking")]
    libs: Vec<String>,

    /// Warning options (e.g. -Wall, -Werror, -Wunused, -Wno-unused)
    #[arg(short = 'W', value_name = "WARNING", action = ArgAction::Append, help_heading = "Diagnostics")]
    warnings: Vec<String>,

    /// Compile for the given target triple
    #[arg(
        long = "target",
        value_name = "TRIPLE",
        help_heading = "Output & Compilation"
    )]
    target: Option<String>,

    /// Use the given sysroot
    #[arg(
        long = "sysroot",
        value_name = "DIR",
        help_heading = "Output & Compilation"
    )]
    sysroot: Option<PathBuf>,

    /// Do not link the standard library (accepted for clang-compat; not yet used)
    #[arg(long = "no-std", action = ArgAction::SetTrue, help_heading = "Search Paths & Linking")]
    no_std: bool,

    /// Link statically with the no-libc runtime. Always enabled: Silver never
    /// links against libc. This flag only governs the runtime, not the
    /// linker's static/dynamic mode (see `--static`).
    #[arg(long = "static-runtime", action = ArgAction::SetTrue, default_value_t = true, hide = true)]
    static_runtime: bool,

    /// Link the executable fully statically. By default the executable is
    /// dynamically linked so external shared libraries (e.g. raylib) can be
    /// used; Silver code and std are always linked statically into the
    /// objects either way.
    #[arg(long = "static", action = ArgAction::SetTrue, help_heading = "Search Paths & Linking")]
    static_link: bool,

    /// Prefer shared module artifacts and emit shared libraries for module packaging
    #[arg(long = "shared", action = ArgAction::SetTrue, help_heading = "Search Paths & Linking")]
    shared: bool,

    /// Use verbose output
    #[arg(short = 'v', long = "verbose", action = ArgAction::SetTrue)]
    verbose: bool,

    /// Print commands/plan but do not execute (clang-style: also accepts -###)
    #[arg(long = "dry-run", action = ArgAction::SetTrue, help_heading = "Diagnostics")]
    dry_run: bool,

    /// Enable time and memory profiling output
    #[arg(long = "profile", action = ArgAction::SetTrue, help_heading = "Diagnostics")]
    profile: bool,

    /// Enable allocator leak-check, double-free, and buffer overflow diagnostics
    #[arg(long = "leak-check", action = ArgAction::SetTrue, help_heading = "Diagnostics")]
    leak_check: bool,

    /// Enable compile-time cfgs: --cfg "key=value,key2=value2" (repeatable).
    /// Drives #[cfg(key)] item gating and @cfg(key) folding; cpu.* keys also
    /// gate on the runtime CPU probe (see std/cpu.ag).
    #[arg(long = "cfg", value_name = "KEY=VALUE,...", action = ArgAction::Append, help_heading = "Diagnostics")]
    cfg_flags: Vec<String>,

    /// Check only: run syntax, semantic, type, and borrow/move checks without codegen or linking
    #[arg(long = "check", action = ArgAction::SetTrue, hide = true)]
    pub check_only: bool,

    /// Override default on-disk cache directory (defaults to XDG $XDG_CACHE_HOME/silver)
    #[arg(
        long = "cache-dir",
        value_name = "DIR",
        help_heading = "Cache & Performance"
    )]
    pub cache_dir: Option<PathBuf>,

    /// Disable artifact caching
    #[arg(long = "no-cache", alias = "nc", action = ArgAction::SetTrue, help_heading = "Cache & Performance")]
    pub no_cache: bool,

    /// Clean the on-disk compiler cache before building
    #[arg(long = "clean", alias = "clean-cache", action = ArgAction::SetTrue, help_heading = "Cache & Performance")]
    pub clean: bool,

    /// Display cache statistics and disk usage
    #[arg(long = "cache-info", action = ArgAction::SetTrue, help_heading = "Cache & Performance")]
    pub cache_info: bool,

    /// Prune cache by size (e.g. --cache-prune 100M or --cache-prune)
    #[arg(long = "cache-prune", value_name = "SIZE", num_args = 0..=1, default_missing_value = "500M", help_heading = "Cache & Performance")]
    pub cache_prune: Option<String>,

    /// Number of parallel compilation jobs (defaults to CPU count)
    #[arg(
        short = 'j',
        long = "jobs",
        value_name = "N",
        default_value_t = 0,
        help_heading = "Cache & Performance"
    )]
    pub jobs: usize,

    /// Enable AST-level dead code elimination / tree-shaking
    #[arg(long = "tree-shake", action = ArgAction::SetTrue, help_heading = "Cache & Performance")]
    pub tree_shake: bool,

    /// Disable AST-level dead code elimination / tree-shaking
    #[arg(long = "no-tree-shake", action = ArgAction::SetTrue, help_heading = "Cache & Performance")]
    pub no_tree_shake: bool,

    /// Run mode: compile and immediately execute the output binary
    #[arg(long = "run", action = ArgAction::SetTrue, hide = true)]
    pub run_mode: bool,

    /// Arguments to forward to the target binary in run mode
    #[arg(long = "run-arg", value_name = "ARG", action = ArgAction::Append, hide = true)]
    pub run_args: Vec<String>,

    /// Show module dependency build graph and codegen elements
    #[arg(long = "show-graph", action = ArgAction::SetTrue, help_heading = "Diagnostics")]
    pub show_graph: bool,

    /// Show build progress (defaults to on in interactive terminals)
    #[arg(long = "progress", action = ArgAction::SetTrue)]
    pub progress: bool,

    /// Disable build progress
    #[arg(long = "no-progress", action = ArgAction::SetTrue)]
    pub no_progress: bool,

    /// Test mode: compile and run tests
    #[arg(long = "test", action = ArgAction::SetTrue, hide = true)]
    pub test_mode: bool,

    /// Do not capture test output (print test stdout and stderr immediately)
    #[arg(long = "nocapture", action = ArgAction::SetTrue, help_heading = "Testing")]
    pub nocapture: bool,

    /// Generate and run test harness for #[test] functions
    #[arg(long = "test-harness", action = ArgAction::SetTrue, hide = true)]
    pub test_harness: bool,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, ValueEnum)]
pub(crate) enum EmitKind {
    /// Link an executable.
    Exe,
    /// Run frontend checks only (lex, parse, semantic, typeck, move/escape checks).
    Check,
    /// Emit an object file.
    Obj,
    /// Emit assembly.
    Asm,
    /// Emit LLVM IR (.ll).
    LlvmIr,
    /// (Future) Dump lexer tokens.
    Tokens,
    /// (Future) Dump parsed AST.
    Ast,
    /// Dump parser grammar.
    Grammar,
    /// Emit module interface artifact (.agm).
    Module,
}

#[derive(Debug, Clone)]
pub(crate) struct CompilePlan {
    pub(crate) emit: EmitKind,
    pub(crate) inputs: Vec<PathBuf>,
    pub(crate) output: PathBuf,
    pub(crate) package_root: PathBuf,
    pub(crate) package_roots: Vec<PathBuf>,
    pub(crate) package_imports: Vec<package::DependencyImport>,
    pub(crate) package_target_kind: Option<package::TargetKind>,
    pub(crate) package_target_name: Option<String>,
    pub(crate) include_dirs: Vec<PathBuf>,
    pub(crate) defines: Vec<String>,
    pub(crate) lib_dirs: Vec<PathBuf>,
    pub(crate) libs: Vec<String>,
    pub(crate) opt_level: Option<String>,
    pub(crate) debug_info: bool,
    pub(crate) target: Option<String>,
    pub(crate) sysroot: Option<PathBuf>,
    pub(crate) no_std: bool,
    pub(crate) static_runtime: bool,
    pub(crate) static_link: bool,
    pub(crate) shared: bool,
    pub(crate) verbose: bool,
    pub(crate) dry_run: bool,
    pub(crate) profile: bool,
    pub(crate) leak_check: bool,
    pub(crate) cfg_flags: Vec<String>,
    pub(crate) cache_dir: Option<PathBuf>,
    pub(crate) no_cache: bool,
    pub(crate) clean: bool,
    pub(crate) cache_info: bool,
    pub(crate) cache_prune: Option<String>,
    pub(crate) jobs: usize,
    pub(crate) run_mode: bool,
    pub(crate) run_args: Vec<String>,
    pub(crate) show_graph: bool,
    pub(crate) progress: bool,
    pub(crate) auto_output: bool,
    pub(crate) warning_config: crate::semantic::linter::WarningConfig,
    pub(crate) test_harness: bool,
    pub(crate) tree_shake: bool,
}

impl CompilePlan {
    fn describe_for_driver(&self) -> String {
        let mut parts: Vec<String> = Vec::new();
        parts.push(format!("emit={:?}", self.emit));
        parts.push(format!("output={}", self.output.display()));
        parts.push(format!("root={}", self.package_root.display()));
        if let Some(kind) = self.package_target_kind {
            let name = self.package_target_name.as_deref().unwrap_or("<default>");
            parts.push(format!("package_target={kind:?}:{name}"));
        }

        if let Some(t) = &self.target {
            parts.push(format!("target={t}"));
        }
        if let Some(s) = &self.sysroot {
            parts.push(format!("sysroot={}", s.display()));
        }
        if let Some(o) = &self.opt_level {
            parts.push(format!("opt={o}"));
        }
        if self.debug_info {
            parts.push("debug=true".to_string());
        }
        if self.no_std {
            parts.push("no_std=true".to_string());
        }
        if self.static_runtime {
            parts.push("static_runtime=true".to_string());
        }
        if self.leak_check {
            parts.push("leak_check=true".to_string());
        }
        if self.shared {
            parts.push("shared=true".to_string());
        }
        if !self.include_dirs.is_empty() {
            parts.push(format!("I={}", self.include_dirs.len()));
        }
        if !self.defines.is_empty() {
            parts.push(format!("D={}", self.defines.len()));
        }
        if !self.lib_dirs.is_empty() {
            parts.push(format!("L={}", self.lib_dirs.len()));
        }
        if !self.libs.is_empty() {
            parts.push(format!("l={}", self.libs.len()));
        }

        format!("agc {}", parts.join(" "))
    }
}

fn derive_emit(cli: &Cli) -> Result<EmitKind, String> {
    if cli.check_only {
        return Ok(EmitKind::Check);
    }
    if let Some(e) = cli.emit {
        return Ok(e);
    }

    let mut derived_flags = 0;
    if cli.emit_llvm {
        derived_flags += 1;
    }
    if cli.emit_asm {
        derived_flags += 1;
    }
    if cli.compile_only {
        derived_flags += 1;
    }
    if derived_flags > 1 {
        return Err(
            "conflicting flags: choose only one of --emit-llvm, -S, or -c (or use --emit=...)"
                .to_string(),
        );
    }

    let mut derived: Option<EmitKind> = None;
    if cli.emit_llvm {
        derived = Some(EmitKind::LlvmIr);
    }
    if cli.emit_asm {
        derived = Some(EmitKind::Asm);
    }
    if cli.compile_only {
        derived = Some(EmitKind::Obj);
    }

    Ok(derived.unwrap_or(EmitKind::Exe))
}

fn default_output_for(emit: EmitKind, inputs: &[PathBuf], target: Option<&str>) -> PathBuf {
    let windows = crate::codegen::abi::target_is_windows(target);
    match emit {
        EmitKind::Exe => {
            if windows {
                PathBuf::from("a.exe")
            } else {
                PathBuf::from("a.out")
            }
        }
        EmitKind::Check => PathBuf::from(""),
        EmitKind::Obj => with_ext_or_default(inputs, if windows { "obj" } else { "o" }),
        EmitKind::Asm => with_ext_or_default(inputs, if windows { "asm" } else { "s" }),
        EmitKind::LlvmIr => with_ext_or_default(inputs, "ll"),
        EmitKind::Tokens => with_ext_or_default(inputs, "tokens"),
        EmitKind::Ast => with_ext_or_default(inputs, "ast"),
        EmitKind::Grammar => with_ext_or_default(inputs, "grammar"),
        EmitKind::Module => with_ext_or_default(inputs, "agm"),
    }
}

fn with_ext_or_default(inputs: &[PathBuf], ext: &str) -> PathBuf {
    let Some(first) = inputs.first() else {
        return PathBuf::from(format!("out.{ext}"));
    };
    let stem = first.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    PathBuf::from(format!("{stem}.{ext}"))
}

fn derive_plan(cli: Cli) -> Result<CompilePlan, String> {
    if cli.lib.is_some() && cli.run_mode {
        return Err("`run --lib` is invalid: library targets are emitted as modules and cannot be executed".to_string());
    }

    let mut emit = derive_emit(&cli)?;
    if cli.lib.is_some() {
        if let Some(explicit_emit) = cli.emit
            && !matches!(explicit_emit, EmitKind::Check | EmitKind::Module)
        {
            return Err("`--lib` requires `--emit=module` (or `check`) rather than an executable/object emit".to_string());
        }
        if cli.emit.is_none() && (cli.compile_only || cli.emit_asm || cli.emit_llvm) {
            return Err("`--lib` requires module output; use `--emit=module` or `--emit=check`".to_string());
        }
        if matches!(emit, EmitKind::Exe) {
            emit = EmitKind::Module;
        }
    }

    let mut inputs = cli.inputs;
    let explicit_root = cli.root;
    let current_dir = env::current_dir()
        .map_err(|e| format!("failed to determine current working directory: {e}"))?;
    let mut package_root = explicit_root
        .clone()
        .unwrap_or_else(|| current_dir.clone());
    let mut package_roots = Vec::new();
    let mut package_imports = Vec::new();
    let mut package_target_kind = None;
    let mut package_target_name = None;
    let mut include_dirs = cli.include_dirs;
    let selector_package_manifest = if inputs.is_empty() {
        cli.bin
            .as_deref()
            .or(cli.lib.as_deref())
            .filter(|value| !value.is_empty())
            .and_then(|value| {
                let path = PathBuf::from(value);
                let manifest = if path.is_dir() {
                    path.join(package::MANIFEST_FILE)
                } else if path.file_name().and_then(|name| name.to_str())
                    == Some(package::MANIFEST_FILE)
                {
                    path
                } else {
                    return None;
                };
                manifest.is_file().then_some(manifest)
            })
    } else {
        None
    };

    let package_candidate = if matches!(
        emit,
        EmitKind::Exe
            | EmitKind::Check
            | EmitKind::Obj
            | EmitKind::Asm
            | EmitKind::LlvmIr
            | EmitKind::Module
    ) && !cli.clean
    {
        match inputs.as_slice() {
            [] => {
                if let Some(manifest) = selector_package_manifest.clone() {
                    Some(manifest)
                } else {
                    let current_manifest = current_dir.join(package::MANIFEST_FILE);
                    if current_manifest.is_file() {
                        Some(current_manifest)
                    } else {
                        explicit_root.as_deref().and_then(|root| {
                            let manifest = root.join(package::MANIFEST_FILE);
                            manifest.is_file().then_some(manifest)
                        })
                    }
                }
            }
            [input] if input.is_dir() => {
                let manifest = input.join(package::MANIFEST_FILE);
                manifest.is_file().then_some(manifest)
            }
            [input]
                if input
                    .file_name()
                    .and_then(|name| name.to_str())
                    == Some(package::MANIFEST_FILE)
                    && input.is_file() =>
            {
                Some(input.clone())
            }
            _ => None,
        }
    } else {
        None
    };

    let package_selection = match (&cli.bin, &cli.lib) {
        (Some(name), None) => package::TargetSelection::bin(
            (!name.is_empty() && selector_package_manifest.is_none()).then(|| name.clone()),
        ),
        (None, Some(name)) => package::TargetSelection::lib(
            (!name.is_empty() && selector_package_manifest.is_none()).then(|| name.clone()),
        ),
        (None, None) => package::TargetSelection::bin(None),
        (Some(_), Some(_)) => {
            return Err("only one of --bin or --lib may be specified".to_string());
        }
    };

    if (cli.bin.is_some() || cli.lib.is_some()) && package_candidate.is_none() {
        return Err("--bin and --lib require a package directory or silver.toml input".to_string());
    }

    if let Some(manifest_path) = package_candidate {
        let cache_root = match &cli.cache_dir {
            Some(path) => package::PackageResolver::with_cache_root(path.clone()),
            None => package::PackageResolver::new()
                .map_err(|error| format!("failed to initialize package cache: {error}"))?,
        };
        let mut resolver = cache_root;
        let graph = resolver.resolve(&manifest_path)?;
        let target = graph.select_target(&package_selection)?;
        let target_package = graph
            .package(target.package)
            .ok_or_else(|| "resolved package graph has no selected target package".to_string())?;
        let manifest_dir = target_package
            .manifest_path
            .parent()
            .ok_or_else(|| "selected package manifest has no parent directory".to_string())?;
        package_root = manifest_dir.to_path_buf();
        package_roots = graph.package_roots();
        package_imports = graph.dependency_imports()?;
        package_target_kind = Some(target.kind);
        package_target_name = Some(target.name.clone());
        if let Some(root) = explicit_root
            && root != package_root
            && !include_dirs.contains(&root)
        {
            // Preserve the historical --root search path when the package
            // manifest is supplied from a different directory.
            include_dirs.push(root);
        }
        inputs = vec![target.entry];
    }

    if inputs.is_empty() && emit != EmitKind::Grammar && !cli.clean && !cli.cache_info && cli.cache_prune.is_none() {
        return Err(
            "at least one input file is required (except for --emit=grammar, --clean, or cache commands)"
                .to_string(),
        );
    }

    // For now keep multi-input support limited to link stage, like most compilers.
    if inputs.len() > 1 {
        match emit {
            EmitKind::Exe | EmitKind::Tokens | EmitKind::Ast | EmitKind::Grammar => {}
            _ => {
                if cli.output.is_some() {
                    return Err("multiple input files with a single -o is not supported yet; omit -o or compile inputs individually".to_string());
                }
                return Err(
                    "multiple input files are only supported for linking (no -c/-S/--emit-llvm)"
                        .to_string(),
                );
            }
        }
    }

    let auto_output = cli.output.is_none();
    let output = if cli.run_mode && auto_output {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let mut bin = env::temp_dir().join(format!(
            "agc_run_{}_{}",
            std::process::id(),
            timestamp
        ));
        // CreateProcess only finds the image when the path ends in .exe
        if crate::codegen::abi::target_is_windows(cli.target.as_deref()) {
            bin.set_extension("exe");
        }
        bin
    } else if auto_output
        && emit == EmitKind::Module
        && package_target_kind == Some(package::TargetKind::Lib)
    {
        PathBuf::from(format!(
            "{}.agm",
            package_target_name.as_deref().unwrap_or("lib")
        ))
    } else {
        cli.output
            .unwrap_or_else(|| default_output_for(emit, &inputs, cli.target.as_deref()))
    };

    Ok(CompilePlan {
        emit,
        inputs,
        output,
        package_root,
        package_roots,
        package_imports,
        package_target_kind,
        package_target_name,
        include_dirs,
        defines: cli.defines,
        lib_dirs: cli.lib_dirs,
        libs: cli.libs,
        // DWARF by default in debug builds (no -O / -O0); stripped in release
        // (-O1+). An explicit -g/-g0 always wins.
        debug_info: if cli.no_debug_info {
            false
        } else if cli.debug_info {
            true
        } else {
            !matches!(
                cli.opt_level.as_deref(),
                Some("1" | "2" | "3" | "s" | "z" | "fast")
            )
        },
        opt_level: cli.opt_level,
        target: cli.target,
        sysroot: cli.sysroot,
        no_std: cli.no_std || cli.static_runtime,
        static_runtime: cli.static_runtime,
        static_link: cli.static_link,
        shared: cli.shared,
        verbose: cli.verbose,
        dry_run: cli.dry_run,
        profile: cli.profile,
        run_mode: cli.run_mode,
        run_args: cli.run_args,
        auto_output,
        leak_check: cli.leak_check,
        cfg_flags: cli.cfg_flags,
        cache_dir: cli.cache_dir,
        no_cache: cli.no_cache,
        clean: cli.clean,
        cache_info: cli.cache_info,
        cache_prune: cli.cache_prune,
        jobs: cli.jobs,
        show_graph: cli.show_graph,
        progress: {
            use std::io::IsTerminal;
            if cli.no_progress {
                false
            } else if cli.progress {
                true
            } else {
                (std::io::stderr().is_terminal() || std::io::stdout().is_terminal())
                    && !cli.verbose
                    && !cli.dry_run
            }
        },
        warning_config: crate::semantic::linter::WarningConfig::from_flags(&cli.warnings),
        test_harness: cli.test_harness,
        tree_shake: if cli.no_tree_shake {
            false
        } else if cli.tree_shake {
            true
        } else {
            matches!(emit, EmitKind::Exe | EmitKind::Obj)
        },
    })
}

fn build_module_loader(plan: &CompilePlan) -> ModuleLoader {
    let mut loader = ModuleLoader::new();
    loader.no_cache = plan.no_cache;
    loader.target = plan.target.clone();
    loader.opt_level = plan.opt_level.clone();
    loader.debug_info = plan.debug_info;
    loader.leak_check = plan.leak_check;
    loader.cfg_flags = plan.cfg_flags.clone();

    if !plan.no_cache {
        let cache_store = match &plan.cache_dir {
            Some(dir) => crate::cache_store::CacheStore::with_dir(dir.clone()),
            None => crate::cache_store::CacheStore::new(),
        };
        match cache_store {
            Ok(store) => {
                loader.add_search_dir(store.agm_dir());
                loader.cache_store = Some(std::sync::Arc::new(store));
            }
            Err(e) => {
                let dir = plan
                    .cache_dir
                    .clone()
                    .unwrap_or_else(crate::cache_store::CacheStore::default_cache_dir);
                eprintln!(
                    "agc: warning: failed to initialize build cache at {} ({e}); continuing without cache",
                    dir.display()
                );
            }
        }
    }
    // Search roots (checked after relative path and cwd): package roots,
    // explicit --root/-I paths, then sysroot. Every package gets its own
    // artifact directory so prebuilt dependency modules remain consumable.
    loader.add_search_dir(&plan.package_root);
    for package_root in &plan.package_roots {
        loader.add_search_dir(package_root);
        let package_lib = package_root.join("lib").join("silver");
        if package_lib.is_dir() {
            loader.add_search_dir(package_lib);
        }
    }

    for dir in &plan.include_dirs {
        loader.add_search_dir(dir);
    }

    for import in &plan.package_imports {
        loader.add_package_import(
            &import.name,
            &import.source,
            &import.owner_root,
        );
    }

    for dir in module_loader_default_dirs(plan.sysroot.as_deref()) {
        loader.add_search_dir(dir);
    }

    loader
}

fn module_path_from_source_path(plan: &CompilePlan, input: &Path) -> String {
    let path = input.strip_prefix(&plan.package_root).unwrap_or(input);
    let without_ext = path.with_extension("");
    without_ext
        .components()
        .filter_map(|component| component.as_os_str().to_str())
        .collect::<Vec<_>>()
        .join(".")
}

fn module_binary_output_path(manifest_path: &Path, shared: bool, target: Option<&str>) -> PathBuf {
    let ext = if shared {
        if crate::codegen::abi::target_is_windows(target) {
            "dll"
        } else {
            "so"
        }
    } else if crate::codegen::abi::target_is_windows(target) {
        "obj"
    } else {
        "o"
    };
    manifest_path.with_extension(ext)
}

fn artifact_compatibility_error(module: &ModuleArtifact, plan: &CompilePlan) -> Option<String> {
    let expected_target = plan.target.clone().unwrap_or_else(|| {
        TargetMachine::get_default_triple()
            .as_str()
            .to_str()
            .unwrap_or("<unknown>")
            .to_string()
    });

    if module.target_triple != "unknown" && module.target_triple != expected_target {
        return Some(format!(
            "module `{}` target `{}` is incompatible with current build `{expected_target}`",
            module.module_path, module.target_triple
        ));
    }

    let expected_version = env!("CARGO_PKG_VERSION");
    if module.compiler_version != "foreign" && module.compiler_version != expected_version {
        return Some(format!(
            "module `{}` was built by compiler version `{}` but current compiler is `{expected_version}`",
            module.module_path, module.compiler_version
        ));
    }

    for candidate in module.source_candidate_paths() {
        if !candidate.is_file() {
            continue;
        }
        let Ok(source_text) = std::fs::read_to_string(&candidate) else {
            continue;
        };
        let current_hash = hash_source_text(&source_text);
        if current_hash != module.source_hash_fnv1a64 {
            return Some(format!(
                "module `{}` is stale: source at `{}` has changed since `{}` was built",
                module.module_path,
                candidate.display(),
                module
                    .artifact_path
                    .as_ref()
                    .map(|path| path.display().to_string())
                    .unwrap_or_else(|| "its manifest".to_string())
            ));
        }
        break;
    }

    None
}

fn collect_dependency_link_artifacts(
    loader: &ModuleLoader,
    roots: &[ModuleArtifact],
    plan: &CompilePlan,
    shared: bool,
) -> Result<Vec<PathBuf>, String> {
    let closure = loader.resolve_module_closure(roots)?;
    let mut paths = Vec::new();
    for module in closure {
        if let Some(error) = artifact_compatibility_error(&module, plan) {
            return Err(error);
        }
        let path = if shared {
            module.shared_library_path()
        } else {
            module.static_library_path()
        };
        let Some(path) = path else {
            continue;
        };
        if !paths.contains(&path) {
            paths.push(path);
        }
    }
    Ok(paths)
}

/// Execute the full compile pipeline for a parsed CLI.
pub fn run(cli: Cli) {
    if cli.init {
        match run_init(&cli) {
            Ok(package) => {
                println!(
                    "Created package `{}` in {}",
                    package.name,
                    package.root.display()
                );
            }
            Err(error) => {
                eprintln!("agc: error: {error}");
                std::process::exit(2);
            }
        }
        return;
    }
    if cli.test_mode {
        run_tests(cli);
        return;
    }
    if cli.init_name.is_some() {
        eprintln!("agc: error: `--name` is only valid with `init`");
        std::process::exit(2);
    }

    match derive_plan(cli) {
        Ok(mut plan) => {
            if plan.verbose || std::env::var_os("AGC_VERBOSE").is_some() {
                eprintln!("{}", plan.describe_for_driver());
                for input in &plan.inputs {
                    eprintln!("  input: {}", input.display());
                }
                eprintln!("  --root {}", plan.package_root.display());
                for inc in &plan.include_dirs {
                    eprintln!("  -I {}", inc.display());
                }
                for def in &plan.defines {
                    eprintln!("  -D {def}");
                }
                for dir in &plan.lib_dirs {
                    eprintln!("  -L {}", dir.display());
                }
                for lib in &plan.libs {
                    eprintln!("  -l {lib}");
                }
            }

            // Temporary behavior: allow driver bring-up and scripting via -###/--dry-run.
            // Once codegen exists, this becomes an actual compile.
            if plan.dry_run || env::var_os("AGC_DRY_RUN").is_some() {
                println!("{}", plan.describe_for_driver());
                return;
            }

            if plan.clean {
                let store = match plan.cache_dir.clone() {
                    Some(dir) => crate::cache_store::CacheStore::with_dir(dir),
                    None => crate::cache_store::CacheStore::new(),
                };
                match store {
                    Ok(store) => {
                        let root = store.root_dir().to_path_buf();
                        if let Err(e) = std::fs::remove_dir_all(&root) {
                            eprintln!(
                                "agc: warning: failed to clean cache directory {} ({e})",
                                root.display()
                            );
                        }
                        if let Err(e) = store.ensure_dirs() {
                            eprintln!(
                                "agc: warning: failed to recreate cache directory {} ({e})",
                                store.root_dir().display()
                            );
                        }
                        if plan.verbose {
                            eprintln!("agc: cleaned cache directory {}", root.display());
                        }
                    }
                    Err(e) => {
                        eprintln!("agc: warning: failed to open build cache for cleaning ({e})");
                    }
                }
                if plan.inputs.is_empty() {
                    return;
                }
            }

            if plan.cache_info {
                let store = match plan.cache_dir.clone() {
                    Some(dir) => crate::cache_store::CacheStore::with_dir(dir),
                    None => crate::cache_store::CacheStore::new(),
                };
                match store.and_then(|s| s.stats()) {
                    Ok(stats) => {
                        use owo_colors::OwoColorize;
                        println!("{}", "=== Silver Compiler Cache ===".bold().cyan());
                        println!("  {:<18} {}", "Location:".bold(), stats.root_dir.display());
                        let total_mb = stats.total_bytes as f64 / (1024.0 * 1024.0);
                        let agm_kb = stats.agm_bytes as f64 / 1024.0;
                        let obj_mb = stats.obj_bytes as f64 / (1024.0 * 1024.0);
                        println!(
                            "  {:<18} {} files ({:.2} MB)",
                            "Total Size:".bold(),
                            stats.total_files.to_string().bold(),
                            total_mb
                        );
                        println!(
                            "    • {:<14} {} files ({:.1} KB)",
                            "Metadata (.agm):",
                            stats.agm_count,
                            agm_kb
                        );
                        println!(
                            "    • {:<14} {} files ({:.2} MB)",
                            "Objects (.o):",
                            stats.obj_count,
                            obj_mb
                        );
                        if let Some(oldest) = stats.oldest_entry_age_secs {
                            let oldest_hours = oldest / 3600;
                            println!("  {:<18} {} hours ago", "Oldest entry:".dimmed(), oldest_hours);
                        }
                    }
                    Err(e) => {
                        eprintln!("agc: error reading cache statistics: {e}");
                    }
                }
                if plan.inputs.is_empty() {
                    return;
                }
            }

            if let Some(target_size_str) = &plan.cache_prune {
                let max_bytes = crate::cache_store::parse_size_to_bytes(target_size_str).unwrap_or(500 * 1024 * 1024);
                let store = match plan.cache_dir.clone() {
                    Some(dir) => crate::cache_store::CacheStore::with_dir(dir),
                    None => crate::cache_store::CacheStore::new(),
                };
                match store {
                    Ok(s) => {
                        match s.prune_to_max_size(max_bytes) {
                            Ok((deleted, freed)) => {
                                let freed_mb = freed as f64 / (1024.0 * 1024.0);
                                eprintln!(
                                    "agc: pruned {} files, freed {:.2} MB",
                                    deleted, freed_mb
                                );
                            }
                            Err(e) => eprintln!("agc: error pruning cache: {e}"),
                        }
                    }
                    Err(e) => eprintln!("agc: error opening cache for pruning: {e}"),
                }
                if plan.inputs.is_empty() {
                    return;
                }
            }

            if let Some(target) = plan.target.as_deref()
                && matches!(
                    plan.emit,
                    EmitKind::Exe | EmitKind::Obj | EmitKind::Asm | EmitKind::LlvmIr
                )
                && let Err(e) = validate_target_triple_with_help(target)
            {
                eprintln!("agc: {}: {e}", "error".red().bold());
                std::process::exit(2);
            }

            if plan.emit == EmitKind::Tokens {
                for input in &plan.inputs {
                    let src = match std::fs::read_to_string(input) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!(
                                "agc: {}: failed to read {}: {e}",
                                "error".red().bold(),
                                input.display()
                            );
                            std::process::exit(2);
                        }
                    };

                    match lexer::lex(&src) {
                        Ok(tokens) => {
                            if plan.inputs.len() > 1 {
                                println!("== {} ==", input.display());
                            }
                            for t in tokens {
                                // Compact, stable-ish output.
                                println!(
                                    "{:?} [{}..{}] {}",
                                    t.kind, t.span.start, t.span.end, t.text
                                );
                            }
                        }
                        Err(errors) => {
                            eprintln!(
                                "agc: {}: lexer errors in {}",
                                "error".red().bold(),
                                input.display()
                            );
                            for e in errors {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        e.span,
                                        &format!("{:?}", e.kind),
                                        diagnostics::Severity::Error,
                                    )
                                );
                            }
                            std::process::exit(2);
                        }
                    }
                }
                return;
            }

            if plan.emit == EmitKind::Grammar {
                let prt_parser = parser::prt_parser::PRT_Parser::new(None);
                let grammar = prt_parser.render_grammar_pretty();

                if plan.inputs.len() > 1 {
                    for input in &plan.inputs {
                        println!("== {} ==", input.display());
                        println!("{grammar}");
                    }
                } else {
                    println!("{grammar}");
                }
                return;
            }

            if plan.emit == EmitKind::Ast {
                for input in &plan.inputs {
                    let src = match std::fs::read_to_string(input) {
                        Ok(s) => s,
                        Err(e) => {
                            eprintln!(
                                "agc: {}: failed to read {}: {e}",
                                "error".red().bold(),
                                input.display()
                            );
                            std::process::exit(2);
                        }
                    };

                    let graph = crate::grammar::parse_ag(&src);
                    let ast = crate::grammar::lower_source_graph(&graph, 0);

                    if graph.has_errors() {
                        for error in graph.errors() {
                            let span = lexer::Span {
                                start: error.start as usize,
                                end: error.end as usize,
                                ..Default::default()
                            };
                            eprintln!(
                                "{}",
                                diagnostics::render(
                                    span,
                                    &error.message,
                                    diagnostics::Severity::Error,
                                )
                            );
                        }
                        if ast.items.is_empty() {
                            std::process::exit(2);
                        }
                        eprintln!("agc: continuing with partial parse...");
                    }

                    let mut symbol_table = CompilerSymbolTable::new();
                    symbol_table.touch_phase(CompilerPhase::Parse, "parse complete");
                    symbol_table.record_program_symbols(&ast, CompilerPhase::Parse);

                    if plan.inputs.len() > 1 {
                        println!("== {} ==", input.display());
                    }
                    if plan.verbose {
                        eprintln!(
                            "agc: symbol table [{}]: {}",
                            input.display(),
                            symbol_table.summary_line()
                        );
                    }
                    println!("{}", ast_tree::render_program(&ast));
                }
                return;
            }

            let loader = build_module_loader(&plan);
            profiler::install(profiler::Profiler::new(
                plan.profile,
                plan.profile && plan.verbose,
            ));

            let show_graph = plan.show_graph;
            let progress_enabled = plan.progress;
            let dep_graph = match crate::build_graph::DependencyGraph::build(&loader, &plan.inputs) {
                Ok(graph) => graph,
                Err(_) => {
                    std::process::exit(2);
                }
            };
            let total_graph_elements = dep_graph.total_codegen_elements();
            let total_modules = dep_graph.nodes.len();
            if show_graph || plan.verbose {
                dep_graph.display_graph();
            }

            let dep_nodes_count = dep_graph
                .nodes
                .values()
                .filter(|n| {
                    let canon_n = std::fs::canonicalize(&n.source_path).unwrap_or_else(|_| n.source_path.clone());
                    !plan.inputs.iter().any(|inp| {
                        let canon_inp = std::fs::canonicalize(inp).unwrap_or_else(|_| inp.clone());
                        canon_inp == canon_n
                    })
                })
                .count();
            let has_link_step = matches!(plan.emit, EmitKind::Exe);
            let total_steps = dep_nodes_count
                + if matches!(plan.emit, EmitKind::Exe | EmitKind::Obj) {
                    plan.inputs.len()
                } else {
                    0
                }
                + if has_link_step { 1 } else { 0 };
            let progress = std::sync::Arc::new(crate::build_graph::BuildProgress::new(
                total_steps,
                progress_enabled,
                plan.verbose,
            ));

            let mut cached_count = 0;
            let mut compiled_count = 0;
            if !plan.no_cache && matches!(plan.emit, EmitKind::Exe | EmitKind::Obj) {
                if let Some(store) = &loader.cache_store {
                    let executor = crate::build_graph::ParallelGraphExecutor::new(
                        &dep_graph,
                        &loader,
                        store,
                        plan.jobs,
                        Some(progress.clone()),
                        &plan.inputs,
                    );
                    match executor.execute() {
                        Ok(report) => {
                            cached_count = report.cache_hits;
                            compiled_count = report.compiled_modules;
                        }
                        Err(_) => {
                            std::process::exit(2);
                        }
                    }
                }
            }
            let active_progress = Some(progress);

            let mut llvm_units: Vec<(PathBuf, String)> = Vec::new();
            let mut exe_object_files: Vec<PathBuf> = Vec::new();
            let exe_temp_dir = if plan.emit == EmitKind::Exe {
                let pid = std::process::id();
                let nonce = match SystemTime::now().duration_since(UNIX_EPOCH) {
                    Ok(d) => d.as_nanos(),
                    Err(e) => {
                        eprintln!(
                            "agc: {}: failed to compute temp dir nonce: {e}",
                            "error".red().bold()
                        );
                        std::process::exit(2);
                    }
                };
                let dir = std::env::temp_dir().join(format!("agc-exe-{pid}-{nonce}"));
                if let Err(e) = std::fs::create_dir_all(&dir) {
                    eprintln!(
                        "agc: {}: failed to create temp dir {}: {e}",
                        "error".red().bold(),
                        dir.display()
                    );
                    std::process::exit(2);
                }
                Some(dir)
            } else {
                None
            };
            let mut native_libs = plan.libs.clone();
            let mut dependency_link_artifacts: Vec<PathBuf> = Vec::new();
            let mut dependency_artifact_set: rustc_hash::FxHashSet<PathBuf> =
                rustc_hash::FxHashSet::default();

            for input in &plan.inputs {
                profiler::begin_phase("read source");
                let src = match std::fs::read_to_string(input) {
                    Ok(s) => s,
                    Err(e) => {
                        eprintln!(
                            "agc: {}: failed to read {}: {e}",
                            "error".red().bold(),
                            input.display()
                        );
                        std::process::exit(2);
                    }
                };
                profiler::end_phase("read source");

                // Register the input source so diagnostic spans resolve to this
                // file (imported modules register their own files).
                let input_path = input.display().to_string();
                let input_file = lexer::register_source(&input_path, &src);

                profiler::begin_phase("parse");
                let graph = crate::grammar::parse_ag(&src);
                let mut ast = crate::grammar::lower_source_graph(&graph, input_file as usize);
                profiler::end_phase("parse");

                if graph.has_errors() {
                    for error in graph.errors() {
                        let span = lexer::Span {
                            start: error.start as usize,
                            end: error.end as usize,
                            file: input_file,
                            ..Default::default()
                        };
                        eprintln!(
                            "{}",
                            diagnostics::render(span, &error.message, diagnostics::Severity::Error,)
                        );
                    }
                    // A truncated AST must never reach codegen: the missing
                    // functions surface later as baffling link errors (e.g.
                    // `undefined symbol: main`) far from the real cause.
                    std::process::exit(2);
                }

                let pre_lowering_link_libs = match collect_program_link_libraries(&ast) {
                    Ok(libs) => libs,
                    Err(error) => {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                        std::process::exit(2);
                    }
                };

                if plan.test_harness {
                    ast.items.insert(
                        0,
                        ast::Item {
                            kind: ast::ItemKind::Import(ast::ImportItem {
                                path: vec![
                                    ast::Identifier {
                                        name: "std".to_string(),
                                        span: lexer::Span::default(),
                                    },
                                    ast::Identifier {
                                        name: "test".to_string(),
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

                let base_dir = input.parent();
                profiler::begin_phase("import lowering");
                let import_lowering = match parser::FileImportResolverHook::new(&loader)
                    .with_entry_import(
                        plan.package_target_kind != Some(package::TargetKind::Lib)
                            && !matches!(plan.emit, EmitKind::Module),
                    )
                    .lower_program_imports(&mut ast, base_dir, Some(input))
                {
                    Ok(result) => result,
                    Err(error) => {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                };
                profiler::end_phase("import lowering");
                let module_dependencies = import_lowering.module_dependencies;
                let transitive_module_deps = import_lowering.transitive_module_deps;
                let imported_modules = import_lowering.module_artifacts;
                for module in &imported_modules {
                    if let Some(error) = artifact_compatibility_error(module, &plan) {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                }

                // Compile-time cfg gate: drop #[cfg(...)]-rejected items, then
                // fold @cfg(...) and prune dead branches, before any symbol
                // registration, semantic analysis, or type checking sees them.
                profiler::begin_phase("cfg");
                let mut cfg_set = cfg::CfgSet::parse(&plan.cfg_flags);
                cfg::add_derived_cfgs(
                    &mut cfg_set,
                    plan.opt_level.as_deref(),
                    plan.target.as_deref(),
                );
                let cfg_errors = cfg::gate_items(&mut ast, &cfg_set);
                if !cfg_errors.is_empty() {
                    for error in &cfg_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }

                semantic::cfg_hook::fold_and_prune(&mut ast, &cfg_set);
                profiler::end_phase("cfg");

                profiler::begin_phase("serialize");
                crate::semantic::serialize::synthesize_serialization_for_program(&mut ast);
                profiler::end_phase("serialize");

                profiler::begin_phase("macro expansion");
                crate::semantic::macro_expand::expand_macros_in_program(&mut ast);
                profiler::end_phase("macro expansion");

                if plan.test_harness {
                    let test_fns: Vec<String> = ast
                        .items
                        .iter()
                        .filter_map(|item| {
                            if let ast::ItemKind::Function(func) = &item.kind {
                                if item.span.file == input_file
                                    && crate::attributes::is_test_function(&item.attributes)
                                {
                                    return Some(func.name.name.clone());
                                }
                            }
                            None
                        })
                        .collect();

                    // Strip any existing `main` function.
                    ast.items.retain(|item| {
                        if let ast::ItemKind::Function(func) = &item.kind {
                            func.name.name != "main"
                        } else {
                            true
                        }
                    });

                    let mut harness_code = String::from("i32 main() {\n");
                    for name in &test_fns {
                        harness_code
                            .push_str(&format!("    test_start(\"{name}\");\n    {name}();\n"));
                    }
                    harness_code.push_str("    return done();\n}\n");

                    let harness_file = lexer::register_source("<test_harness>", &harness_code);
                    let harness_graph = crate::grammar::parse_ag(&harness_code);
                    let harness_ast =
                        crate::grammar::lower_source_graph(&harness_graph, harness_file as usize);
                    ast.items.extend(harness_ast.items);
                } else {
                    ast.items
                        .retain(|item| !crate::attributes::is_test_function(&item.attributes));
                }

                profiler::begin_phase("symbol_table");
                let mut symbol_table = CompilerSymbolTable::new();
                symbol_table.touch_phase(CompilerPhase::Parse, "parse complete");
                symbol_table.record_program_symbols(&ast, CompilerPhase::Parse);
                profiler::end_phase("symbol_table");

                profiler::begin_phase("semantic");
                let semantic_errors =
                    run_semantic_hooks(&mut ast, &mut symbol_table, &imported_modules);
                profiler::end_phase("semantic");
                if !semantic_errors.is_empty() {
                    for error in &semantic_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }

                profiler::begin_phase("type check");
                TypeChecker::resolve_type_aliases_in_program_with_imports(
                    &mut ast,
                    &imported_modules,
                );
                let mut checker = TypeChecker::new().with_imported_modules(&imported_modules);
                let (type_errors, mut monomorphs) =
                    checker.check_program_with_table(&ast, &mut symbol_table);
                // Populate ForIn iterator_type from typeck-resolved types
                let resolved_iter_types = checker.take_resolved_iter_types();
                if !resolved_iter_types.is_empty() {
                    crate::semantic::typeck::populate_for_in_iterator_types(
                        &mut ast,
                        &resolved_iter_types,
                    );
                }
                // Rewrite bare enum constructors (Some(x)/None/Ok(x)/Err(x))
                // into typed Enum.Variant(...) constructions using the
                // expected-type inference recorded during typeck.
                let bare_constructors = checker.take_bare_constructors();
                if !bare_constructors.is_empty() {
                    crate::semantic::typeck::rewrite_bare_constructors(
                        &mut ast,
                        &bare_constructors,
                    );
                }
                // Materialize inferred `let x = expr;` bindings as annotated
                // lets so downstream passes see plain declarations.
                let inferred_lets = checker.take_inferred_lets();
                if !inferred_lets.is_empty() {
                    crate::semantic::typeck::populate_inferred_let_types(&mut ast, &inferred_lets);
                }
                // Monomorph request bodies were cloned before these rewrites;
                // refresh them from the populated AST so generic instances
                // carry iterator types and materialized let annotations.
                crate::semantic::monomorph::refresh_monomorph_bodies(&mut monomorphs, &ast);
                if !type_errors.is_empty() {
                    for error in &type_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }
                profiler::end_phase("type check");

                profiler::begin_phase("safety");
                // Move-out checker: use-after-move of non-copyable values is
                // a use-after-free, reported alongside type errors.
                let escape_errors = crate::semantic::escape_check::check_program(&ast);
                if !escape_errors.is_empty() {
                    for error in &escape_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }

                let move_errors = crate::semantic::move_check::check_program(&ast);
                if !move_errors.is_empty() {
                    for error in &move_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render_with_note(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                                error.note_span,
                                error.note_message.as_deref(),
                            )
                        );
                    }
                    std::process::exit(2);
                }

                let borrow_errors = crate::semantic::borrow_check::check_program(&ast);
                if !borrow_errors.is_empty() {
                    for error in &borrow_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render_with_note(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                                error.note_span,
                                error.note_message.as_deref(),
                            )
                        );
                    }
                    std::process::exit(2);
                }

                let warnings = crate::semantic::linter::lint_program(&ast, &plan.warning_config);
                let mut had_warning_error = false;
                for w in &warnings {
                    let severity = if plan.warning_config.warnings_as_errors {
                        had_warning_error = true;
                        diagnostics::Severity::Error
                    } else {
                        diagnostics::Severity::Warning
                    };
                    eprintln!("{}", diagnostics::render(w.span, &w.message, severity));
                }
                if had_warning_error {
                    std::process::exit(2);
                }
                profiler::end_phase("safety");

                if plan.emit == EmitKind::Check {
                    if plan.verbose {
                        eprintln!("agc: check passed for {}", input.display());
                    }
                    continue;
                }

                let program_link_libs = match collect_program_link_libraries(&ast) {
                    Ok(libs) => libs,
                    Err(error) => {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                        std::process::exit(2);
                    }
                };
                extend_unique_libs(&mut native_libs, &pre_lowering_link_libs);
                extend_unique_libs(&mut native_libs, &program_link_libs);
                for module in &imported_modules {
                    extend_unique_libs(&mut native_libs, &module.native_libs);
                    for path in &module.native_lib_paths {
                        let path = PathBuf::from(path);
                        if !plan.lib_dirs.contains(&path) {
                            plan.lib_dirs.push(path);
                        }
                    }
                }
                match collect_dependency_link_artifacts(
                    &loader,
                    &imported_modules,
                    &plan,
                    plan.shared,
                ) {
                    Ok(paths) => {
                        for path in paths {
                            if dependency_artifact_set.insert(path.clone()) {
                                dependency_link_artifacts.push(path);
                            }
                        }
                    }
                    Err(error) => {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                }

                if plan.tree_shake {
                    profiler::begin_phase("tree-shake");
                    let before_items = ast.items.len();
                    let pruned = semantic::tree_shake::eliminate_dead_ast_items(
                        &mut ast,
                        Some(input_file),
                        matches!(plan.emit, EmitKind::Module),
                        &monomorphs,
                    );
                    if plan.verbose {
                        eprintln!(
                            "agc: tree-shaking eliminated {} unused items ({} -> {})",
                            pruned,
                            before_items,
                            ast.items.len()
                        );
                    }
                    profiler::end_phase("tree-shake");
                }

                profiler::begin_phase("monomorph");
                profiler::begin_phase("append_monomorphs fixpoint");
                let new_items = semantic::monomorph::append_monomorphs(&mut ast, &monomorphs, &imported_modules);
                profiler::end_phase("append_monomorphs fixpoint");
                if !new_items.is_empty() {
                    profiler::begin_phase("post-monomorph typeck");
                    let temp_prog = ast::Program {
                        attributes: Vec::new(),
                        items: new_items,
                        comments: Vec::new(),
                        span: ast.span,
                    };
                    let _ = checker.check_program_with_table(&temp_prog, &mut symbol_table);
                    let post_bare = checker.take_bare_constructors();
                    if !post_bare.is_empty() {
                        crate::semantic::typeck::rewrite_bare_constructors(&mut ast, &post_bare);
                    }
                    profiler::end_phase("post-monomorph typeck");
                }
                // Module emits: monomorphized instances of the library's own
                // generic functions/impls (e.g. identity__i64_i64) must be
                // externally linkable — consumers reference the mangled
                // symbols defined here. Flip lib-file instances to Public so
                // collect_exports includes them and codegen emits them with
                // external linkage. Instances originating from inlined std
                // carry other file ids and stay private, avoiding duplicate
                // symbol collisions with consumer-side instantiations.
                if matches!(plan.emit, EmitKind::Module) {
                    let lib_file =
                        crate::lexer::register_source(input.to_str().unwrap_or_default(), &src);
                    for item in &mut ast.items {
                        if item.span.file == lib_file {
                            item.visibility = ast::Visibility::Public;
                            if let ast::ItemKind::Impl(impl_item) = &mut item.kind {
                                for member in &mut impl_item.items {
                                    if let ast::ImplItemKind::Function(func) = member {
                                        func.visibility = ast::Visibility::Public;
                                    }
                                }
                            }
                        } else {
                            item.visibility = ast::Visibility::Private;
                            if let ast::ItemKind::Impl(impl_item) = &mut item.kind {
                                for member in &mut impl_item.items {
                                    if let ast::ImplItemKind::Function(func) = member {
                                        func.visibility = ast::Visibility::Private;
                                    }
                                }
                            }
                        }
                    }
                }
                profiler::end_phase("monomorph");
                symbol_table.touch_phase(
                    CompilerPhase::Monomorphize,
                    format!("monomorph requests applied: {}", monomorphs.len()),
                );

                if matches!(plan.emit, EmitKind::Module) {
                    let target_triple = plan.target.clone().unwrap_or_else(|| {
                        TargetMachine::get_default_triple()
                            .as_str()
                            .to_str()
                            .unwrap_or("<unknown>")
                            .to_string()
                    });
                    let artifact = ModuleArtifact::from_program(
                        module_name_from_path(input),
                        module_path_from_source_path(&plan, input),
                        input.display().to_string(),
                        &src,
                        &ast,
                        target_triple,
                        ModuleCodeArtifacts {
                            has_static_library: !plan.shared,
                            has_shared_library: plan.shared,
                        },
                        module_dependencies,
                        transitive_module_deps,
                        native_libs.clone(),
                    );
                    let bytes = match artifact.to_bytes() {
                        Ok(bytes) => bytes,
                        Err(e) => {
                            eprintln!(
                                "agc: {}: failed to encode module artifact: {e}",
                                "error".red().bold()
                            );
                            std::process::exit(2);
                        }
                    };
                    if let Err(e) = std::fs::write(&plan.output, bytes) {
                        eprintln!(
                            "agc: {}: failed to write {}: {e}",
                            "error".red().bold(),
                            plan.output.display()
                        );
                        std::process::exit(2);
                    }
                    let binary_output =
                        module_binary_output_path(&plan.output, plan.shared, plan.target.as_deref());
                    if plan.shared {
                        let temp_object = plan.output.with_extension("module.tmp.o");
                        let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table_and_source_with_leak_check(
                                &ast,
                                &imported_modules,
                                &temp_object,
                                plan.target.as_deref(),
                                plan.opt_level.as_deref(),
                                &mut symbol_table,
                                Some(input),
                                Some(&src),
                                plan.debug_info,
                                plan.leak_check,
                            );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        span,
                                        &error.message,
                                        diagnostics::Severity::Error,
                                    )
                                );
                            } else {
                                eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                            }
                            std::process::exit(2);
                        }
                        if let Err(error) = link_shared_module(
                            &plan,
                            &temp_object,
                            &binary_output,
                            &dependency_link_artifacts,
                            &native_libs,
                        ) {
                            eprintln!("agc: {}: {error}", "error".red().bold());
                            std::process::exit(2);
                        }
                        let _ = std::fs::remove_file(&temp_object);
                    } else {
                        let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table_and_source_with_leak_check(
                                &ast,
                                &imported_modules,
                                &binary_output,
                                plan.target.as_deref(),
                                plan.opt_level.as_deref(),
                                &mut symbol_table,
                                Some(input),
                                Some(&src),
                                plan.debug_info,
                                plan.leak_check,
                            );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        span,
                                        &error.message,
                                        diagnostics::Severity::Error,
                                    )
                                );
                            } else {
                                eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                            }
                            std::process::exit(2);
                        }
                    }
                    continue;
                }

                if matches!(
                    plan.emit,
                    EmitKind::LlvmIr | EmitKind::Exe | EmitKind::Obj | EmitKind::Asm
                ) {
                    symbol_table.touch_phase(CompilerPhase::Codegen, "LLVM codegen");
                    symbol_table.record_program_symbols(&ast, CompilerPhase::Codegen);
                    if matches!(plan.emit, EmitKind::Obj) {
                        profiler::begin_phase("codegen");
                        let stem = input
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("input");
                        let root_elements = crate::build_graph::CodegenElements::from_program(&ast);
                        let start_time = std::time::Instant::now();

                        let mut root_cache_hit = false;
                        let mut root_key_opt = None;
                        if !plan.no_cache {
                            if let Some(store) = &loader.cache_store {
                                let root_deps: Vec<(String, String)> = dep_graph.nodes
                                    .iter()
                                    .filter_map(|(name, node)| {
                                        loader.lookup_computed_cache_key(&node.source_path)
                                            .map(|k| (name.clone(), k.hash_hex))
                                    })
                                    .collect();
                                if let Some(key) = loader.compute_cache_key_with_deps(input, stem, &root_deps) {
                                    if let Some(cached_o) = store.get_obj(&key) {
                                        if std::fs::copy(&cached_o, &plan.output).is_ok() {
                                            root_cache_hit = true;
                                            cached_count += 1;
                                            if let Some(p) = &active_progress {
                                                p.on_finish(stem, &root_elements, true, std::time::Duration::ZERO);
                                            }
                                        }
                                    }
                                    root_key_opt = Some(key);
                                }
                            }
                        }

                        if !root_cache_hit {
                            if let Some(p) = &active_progress {
                                p.on_start(stem, &root_elements);
                            }
                            let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table_and_source_with_leak_check(
                                    &ast,
                                    &imported_modules,
                                    &plan.output,
                                    plan.target.as_deref(),
                                    plan.opt_level.as_deref(),
                                    &mut symbol_table,
                                    Some(input),
                                    Some(&src),
                                    plan.debug_info,
                                    plan.leak_check,
                                );
                            if let Some(p) = &active_progress {
                                if result.is_ok() {
                                    p.on_finish(stem, &root_elements, false, start_time.elapsed());
                                }
                            }
                            if let Err(error) = result {
                                if let Some(span) = error.span {
                                    eprintln!(
                                        "{}",
                                        diagnostics::render(
                                            span,
                                            &error.message,
                                            diagnostics::Severity::Error,
                                        )
                                    );
                                } else {
                                    eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                                }
                                std::process::exit(2);
                            }
                            if !plan.no_cache {
                                if let (Some(store), Some(root_key)) = (&loader.cache_store, &root_key_opt) {
                                    if let Ok(bytes) = std::fs::read(&plan.output) {
                                        let _ = store.put_obj(root_key, &bytes);
                                    }
                                }
                            }
                            compiled_count += 1;
                        }
                        profiler::end_phase("codegen");
                    } else if matches!(plan.emit, EmitKind::Asm) {
                        profiler::begin_phase("codegen");
                        let result =
                                codegen::llvm_ir::LlvmIrGenerator::emit_assembly_file_with_imports_and_table_and_source_with_leak_check(
                                    &ast,
                                    &imported_modules,
                                    &plan.output,
                                    plan.target.as_deref(),
                                    plan.opt_level.as_deref(),
                                    &mut symbol_table,
                                    Some(input),
                                    Some(&src),
                                    plan.debug_info,
                                    plan.leak_check,
                                );
                        profiler::end_phase("codegen");
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        span,
                                        &error.message,
                                        diagnostics::Severity::Error,
                                    )
                                );
                            } else {
                                eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                            }
                            std::process::exit(2);
                        }
                    } else if matches!(plan.emit, EmitKind::LlvmIr) {
                        profiler::begin_phase("codegen");
                        let output = codegen::llvm_ir::LlvmIrGenerator::generate_with_imports_and_table_and_source_with_leak_check(
                                &ast,
                                &imported_modules,
                                &mut symbol_table,
                                Some(input),
                                Some(&src),
                                plan.debug_info,
                                plan.leak_check,
                                plan.target.as_deref(),
                            );
                        profiler::end_phase("codegen");
                        match output {
                            Ok(ir) => {
                                llvm_units.push((
                                    input.clone(),
                                    apply_llvm_target_metadata(ir, plan.target.as_deref()),
                                ));
                            }
                            Err(error) => {
                                if let Some(span) = error.span {
                                    eprintln!(
                                        "{}",
                                        diagnostics::render(
                                            span,
                                            &error.message,
                                            diagnostics::Severity::Error,
                                        )
                                    );
                                } else {
                                    eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                                }
                                std::process::exit(2);
                            }
                        }
                    } else if matches!(plan.emit, EmitKind::Exe) {
                        profiler::begin_phase("codegen");
                        let temp_dir = exe_temp_dir.as_ref().unwrap();
                        let stem = input
                            .file_stem()
                            .and_then(|s| s.to_str())
                            .unwrap_or("input");
                        let temp_o = temp_dir.join(format!("{stem}.o"));
                        let root_elements = crate::build_graph::CodegenElements::from_program(&ast);
                        let start_time = std::time::Instant::now();

                        let mut root_cache_hit = false;
                        let mut root_key_opt = None;
                        if !plan.no_cache {
                            if let Some(store) = &loader.cache_store {
                                let root_deps: Vec<(String, String)> = dep_graph.nodes
                                    .iter()
                                    .filter_map(|(name, node)| {
                                        loader.lookup_computed_cache_key(&node.source_path)
                                            .map(|k| (name.clone(), k.hash_hex))
                                    })
                                    .collect();
                                if let Some(key) = loader.compute_cache_key_with_deps(input, stem, &root_deps) {
                                    if let Some(cached_o) = store.get_obj(&key) {
                                        if std::fs::copy(&cached_o, &temp_o).is_ok() {
                                            root_cache_hit = true;
                                            cached_count += 1;
                                            if let Some(p) = &active_progress {
                                                p.on_finish(stem, &root_elements, true, std::time::Duration::ZERO);
                                            }
                                        }
                                    }
                                    root_key_opt = Some(key);
                                }
                            }
                        }

                        if !root_cache_hit {
                            if let Some(p) = &active_progress {
                                p.on_start(stem, &root_elements);
                            }
                            let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table_and_source_with_leak_check(
                                    &ast,
                                    &imported_modules,
                                    &temp_o,
                                    plan.target.as_deref(),
                                    plan.opt_level.as_deref(),
                                    &mut symbol_table,
                                    Some(input),
                                    Some(&src),
                                    plan.debug_info,
                                    plan.leak_check,
                                );
                            if let Some(p) = &active_progress {
                                if result.is_ok() {
                                    p.on_finish(stem, &root_elements, false, start_time.elapsed());
                                }
                            }
                            if let Err(error) = result {
                                if let Some(span) = error.span {
                                    eprintln!(
                                        "{}",
                                        diagnostics::render(
                                            span,
                                            &error.message,
                                            diagnostics::Severity::Error,
                                        )
                                    );
                                } else {
                                    eprintln!("agc: {}: {}", "error".red().bold(), error.message);
                                }
                                std::process::exit(2);
                            }
                            if !plan.no_cache {
                                if let (Some(store), Some(root_key)) = (&loader.cache_store, &root_key_opt) {
                                    if let Ok(bytes) = std::fs::read(&temp_o) {
                                        let _ = store.put_obj(root_key, &bytes);
                                    }
                                }
                            }
                            compiled_count += 1;
                        }
                        profiler::end_phase("codegen");
                        exe_object_files.push(temp_o);
                    }
                }
                if plan.verbose {
                    eprintln!(
                        "agc: symbol table [{}]: {}",
                        input.display(),
                        symbol_table.summary_line()
                    );
                }
            }

            if plan.emit == EmitKind::LlvmIr {
                let Some((_, ir)) = llvm_units.first() else {
                    eprintln!("agc: {}: no LLVM IR units generated", "error".red().bold());
                    std::process::exit(2);
                };
                if let Err(e) = std::fs::write(&plan.output, ir) {
                    eprintln!(
                        "agc: {}: failed to write {}: {e}",
                        "error".red().bold(),
                        plan.output.display()
                    );
                    std::process::exit(2);
                }
                profiler::print_report();
                return;
            }

            if matches!(plan.emit, EmitKind::Obj | EmitKind::Asm | EmitKind::Module) {
                profiler::print_report();
                return;
            }
            if plan.emit == EmitKind::Check {
                profiler::print_report();
                return;
            }

            if matches!(plan.emit, EmitKind::Exe) {
                if exe_object_files.is_empty() {
                    eprintln!("agc: {}: no object files to link", "error".red().bold());
                    std::process::exit(2);
                }
                if let Some(p) = &active_progress {
                    p.on_link(&plan.output);
                }
                profiler::begin_phase("link");
                if let Err(e) = link_exe(
                    &plan,
                    &exe_object_files,
                    &dependency_link_artifacts,
                    &native_libs,
                ) {
                    eprintln!("agc: {}: {e}", "error".red().bold());
                    std::process::exit(2);
                }
                profiler::end_phase("link");
                if let Some(p) = &active_progress {
                    p.on_complete(
                        total_modules,
                        &total_graph_elements,
                        cached_count,
                        compiled_count + plan.inputs.len(),
                    );
                }
                // Clean up temp dir
                if let Some(dir) = &exe_temp_dir {
                    let _ = std::fs::remove_dir_all(dir);
                }
                if plan.run_mode {
                    let mut cmd = std::process::Command::new(&plan.output);
                    cmd.args(&plan.run_args);
                    let status = cmd.status();
                    if plan.auto_output {
                        let _ = std::fs::remove_file(&plan.output);
                    }
                    match status {
                        Ok(s) => {
                            std::process::exit(s.code().unwrap_or(1));
                        }
                        Err(e) => {
                            eprintln!(
                                "agc: {}: failed to execute {}: {e}",
                                "error".red().bold(),
                                plan.output.display()
                            );
                            std::process::exit(1);
                        }
                    }
                }
                profiler::print_report();
                return;
            }

            eprintln!(
                "agc: {}: unsupported emit mode {:?}",
                "error".red().bold(),
                plan.emit
            );
            std::process::exit(2);
        }
        Err(e) => {
            eprintln!("agc: {}: {e}", "error".red().bold());
            std::process::exit(2);
        }
    }
}

fn run_init(cli: &Cli) -> Result<package::InitializedPackage, String> {
    let target_dir = match cli.inputs.as_slice() {
        [] => env::current_dir()
            .map_err(|error| format!("failed to determine current directory: {error}"))?,
        [target_dir] => target_dir.clone(),
        _ => return Err("`init` accepts at most one target directory".to_string()),
    };
    package::initialize_package(&target_dir, cli.init_name.as_deref())
}

#[derive(Debug, Clone)]
struct TestTarget {
    name: String,
    path: PathBuf,
    is_source_test: bool,
}

#[derive(Debug)]
struct TestExecutionResult {
    name: String,
    passed: bool,
    duration: std::time::Duration,
    error_message: Option<String>,
    output: String,
}

fn expected_exit_code(_test_path: &Path, content: &str) -> i32 {
    for line in content.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("//") {
            let rest = rest.trim();
            if let Some(code_str) = rest
                .strip_prefix("expected_exit:")
                .or_else(|| rest.strip_prefix("expected_exit ="))
                .or_else(|| rest.strip_prefix("exit:"))
            {
                if let Ok(code) = code_str.trim().parse::<i32>() {
                    return code;
                }
            }
        }
    }

    0
}

fn test_specific_flags(_test_path: &Path, content: &str) -> Vec<String> {
    let mut flags = Vec::new();
    for line in content.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("//") {
            let rest = rest.trim();
            if let Some(f_str) = rest
                .strip_prefix("compile_flags:")
                .or_else(|| rest.strip_prefix("flags:"))
            {
                for part in f_str.split_whitespace() {
                    flags.push(part.to_string());
                }
            }
        }
    }
    flags
}

fn scan_dir_for_ag_files(dir: &Path, acc: &mut Vec<PathBuf>) {
    if let Ok(entries) = std::fs::read_dir(dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                scan_dir_for_ag_files(&path, acc);
            } else if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("ag") {
                acc.push(path);
            }
        }
    }
}

fn discover_tests(cli: &Cli) -> Result<Vec<TestTarget>, String> {
    let mut targets = Vec::new();
    let current_dir = env::current_dir()
        .map_err(|e| format!("failed to determine current directory: {e}"))?;
    let root_dir = cli.root.clone().unwrap_or(current_dir);

    let mut explicit_ag_inputs = Vec::new();
    let mut filter_terms = Vec::new();

    for input in &cli.inputs {
        if input.extension().and_then(|e| e.to_str()) == Some("ag") && input.is_file() {
            explicit_ag_inputs.push(input.clone());
        } else if let Some(s) = input.to_str() {
            filter_terms.push(s.to_string());
        }
    }

    if !explicit_ag_inputs.is_empty() {
        for path in explicit_ag_inputs {
            let is_source_test = std::fs::read_to_string(&path)
                .map(|s| s.contains("#[test]"))
                .unwrap_or(false);
            let stem = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("test")
                .to_string();
            targets.push(TestTarget {
                name: stem,
                path,
                is_source_test,
            });
        }
    } else {
        let manifest_path = root_dir.join(package::MANIFEST_FILE);
        let mut manifest_targets = std::collections::HashSet::new();

        if manifest_path.is_file() {
            if let Ok(mut resolver) = package::PackageResolver::new() {
                if let Ok(graph) = resolver.resolve(&manifest_path) {
                    if let Ok(resolved_tests) = graph.test_targets(graph.root()) {
                        for t in resolved_tests {
                            let canon = std::fs::canonicalize(&t.entry).unwrap_or_else(|_| t.entry.clone());
                            manifest_targets.insert(canon);
                            let is_source_test = std::fs::read_to_string(&t.entry)
                                .map(|s| s.contains("#[test]"))
                                .unwrap_or(false);
                            targets.push(TestTarget {
                                name: t.name,
                                path: t.entry,
                                is_source_test,
                            });
                        }
                    }
                }
            }
        }

        let mut source_seen = manifest_targets;
        for t in &targets {
            if let Ok(canon) = std::fs::canonicalize(&t.path) {
                source_seen.insert(canon);
            }
        }

        let src_dir = root_dir.join("src");
        if src_dir.is_dir() {
            let mut src_files = Vec::new();
            scan_dir_for_ag_files(&src_dir, &mut src_files);
            for path in src_files {
                let canon = std::fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
                if !source_seen.insert(canon) {
                    continue;
                }
                if let Ok(content) = std::fs::read_to_string(&path) {
                    if content.contains("#[test]") {
                        let name = path
                            .strip_prefix(&root_dir)
                            .unwrap_or(&path)
                            .display()
                            .to_string();
                        targets.push(TestTarget {
                            name,
                            path,
                            is_source_test: true,
                        });
                    }
                }
            }
        }

        if let Ok(entries) = std::fs::read_dir(&root_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_file() && path.extension().and_then(|e| e.to_str()) == Some("ag") {
                    let canon = std::fs::canonicalize(&path).unwrap_or_else(|_| path.clone());
                    if !source_seen.insert(canon) {
                        continue;
                    }
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        if content.contains("#[test]") {
                            let stem = path
                                .file_stem()
                                .and_then(|s| s.to_str())
                                .unwrap_or("test")
                                .to_string();
                            targets.push(TestTarget {
                                name: stem,
                                path,
                                is_source_test: true,
                            });
                        }
                    }
                }
            }
        }
    }

    if !filter_terms.is_empty() {
        targets.retain(|target| {
            filter_terms.iter().any(|term| {
                target.name.to_lowercase().contains(&term.to_lowercase())
                    || target.path.to_string_lossy().to_lowercase().contains(&term.to_lowercase())
            })
        });
    }

    targets.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(targets)
}

fn execute_test(
    cli: &Cli,
    target: &TestTarget,
    agc_exe: &Path,
    temp_dir: &Path,
    index: usize,
) -> TestExecutionResult {
    let start = std::time::Instant::now();
    let mut temp_bin = temp_dir.join(format!("test_bin_{index}"));
    // CreateProcess only resolves extensionless images by appending .exe
    if cfg!(target_os = "windows") {
        temp_bin.set_extension("exe");
    }
    let content = std::fs::read_to_string(&target.path).unwrap_or_default();
    let expected_code = expected_exit_code(&target.path, &content);
    let extra_flags = test_specific_flags(&target.path, &content);

    let mut compile_cmd = std::process::Command::new(agc_exe);
    compile_cmd.arg(&target.path);
    compile_cmd.arg("-o");
    compile_cmd.arg(&temp_bin);
    compile_cmd.arg("--no-progress");
    if target.is_source_test {
        compile_cmd.arg("--test-harness");
    }

    if let Some(opt) = &cli.opt_level {
        compile_cmd.arg(format!("-O{opt}"));
    }
    if cli.leak_check {
        compile_cmd.arg("--leak-check");
    }
    if let Some(target_triple) = &cli.target {
        compile_cmd.arg("--target");
        compile_cmd.arg(target_triple);
    }
    if let Some(sysroot) = &cli.sysroot {
        compile_cmd.arg("--sysroot");
        compile_cmd.arg(sysroot);
    }
    if let Some(root) = &cli.root {
        compile_cmd.arg("--root");
        compile_cmd.arg(root);
    }
    for inc in &cli.include_dirs {
        compile_cmd.arg("-I");
        compile_cmd.arg(inc);
    }
    for def in &cli.defines {
        compile_cmd.arg("-D");
        compile_cmd.arg(def);
    }
    for lib_dir in &cli.lib_dirs {
        compile_cmd.arg("-L");
        compile_cmd.arg(lib_dir);
    }
    for lib in &cli.libs {
        compile_cmd.arg("-l");
        compile_cmd.arg(lib);
    }
    for cfg in &cli.cfg_flags {
        compile_cmd.arg("--cfg");
        compile_cmd.arg(cfg);
    }
    if cli.no_cache {
        compile_cmd.arg("--no-cache");
    }
    if let Some(cache_dir) = &cli.cache_dir {
        compile_cmd.arg("--cache-dir");
        compile_cmd.arg(cache_dir);
    }

    for flag in &extra_flags {
        compile_cmd.arg(flag);
    }

    let compile_output = match compile_cmd.output() {
        Ok(out) => out,
        Err(e) => {
            return TestExecutionResult {
                name: target.name.clone(),
                passed: false,
                duration: start.elapsed(),
                error_message: Some(format!("failed to spawn compiler: {e}")),
                output: String::new(),
            };
        }
    };

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr).to_string();
        let stdout = String::from_utf8_lossy(&compile_output.stdout).to_string();
        let mut output = String::new();
        if !stdout.is_empty() {
            output.push_str(&stdout);
            if !stdout.ends_with('\n') {
                output.push('\n');
            }
        }
        if !stderr.is_empty() {
            output.push_str(&stderr);
        }
        return TestExecutionResult {
            name: target.name.clone(),
            passed: false,
            duration: start.elapsed(),
            error_message: Some("compilation failed".to_string()),
            output,
        };
    }

    let mut run_cmd = std::process::Command::new(&temp_bin);
    run_cmd.args(&cli.run_args);

    let run_output = match run_cmd.output() {
        Ok(out) => out,
        Err(e) => {
            let _ = std::fs::remove_file(&temp_bin);
            return TestExecutionResult {
                name: target.name.clone(),
                passed: false,
                duration: start.elapsed(),
                error_message: Some(format!("failed to execute test binary: {e}")),
                output: String::new(),
            };
        }
    };

    let _ = std::fs::remove_file(&temp_bin);

    let stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&run_output.stderr).to_string();
    let mut output = String::new();
    if !stdout.is_empty() {
        output.push_str(&stdout);
        if !stdout.ends_with('\n') {
            output.push('\n');
        }
    }
    if !stderr.is_empty() {
        output.push_str(&stderr);
    }

    #[cfg(unix)]
    let actual_code = {
        use std::os::unix::process::ExitStatusExt;
        run_output
            .status
            .code()
            .unwrap_or_else(|| run_output.status.signal().map(|s| 128 + s).unwrap_or(-1))
    };
    #[cfg(not(unix))]
    let actual_code = run_output.status.code().unwrap_or(-1);
    let mut passed = actual_code == expected_code;
    let mut error_message = None;

    if !passed {
        error_message = Some(format!(
            "exited with code {actual_code}, expected {expected_code}"
        ));
    } else {
        let stem = target.path.file_stem().and_then(|s| s.to_str()).unwrap_or("");
        if stem == "backtrace_test" && (!stderr.contains("level3") || !stderr.contains("args: x=")) {
            passed = false;
            error_message = Some("backtrace_test stderr missing expected frame markers".to_string());
        } else if stem == "assert_fail_test" && !stderr.contains("assertion failed") {
            passed = false;
            error_message = Some("assert_fail_test stderr missing 'assertion failed'".to_string());
        }
    }

    TestExecutionResult {
        name: target.name.clone(),
        passed,
        duration: start.elapsed(),
        error_message,
        output,
    }
}

fn run_tests(cli: Cli) {
    let targets = match discover_tests(&cli) {
        Ok(t) => t,
        Err(e) => {
            eprintln!("agc: {}: {e}", "error".red().bold());
            std::process::exit(1);
        }
    };

    if targets.is_empty() {
        eprintln!("agc: {}: no tests found", "error".red().bold());
        std::process::exit(1);
    }

    let agc_exe = env::current_exe().unwrap_or_else(|_| PathBuf::from("agc"));
    let temp_root = env::temp_dir().join(format!(
        "agc-tests-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0)
    ));
    let _ = std::fs::create_dir_all(&temp_root);

    let count = targets.len();
    println!("\nrunning {count} test{}", if count == 1 { "" } else { "s" });

    let overall_start = std::time::Instant::now();
    let mut passed_count = 0;
    let mut failed_count = 0;
    let mut failures: Vec<(String, String)> = Vec::new();

    let jobs = if cli.jobs == 0 {
        std::thread::available_parallelism().map(|n| n.get()).unwrap_or(4)
    } else {
        cli.jobs
    };

    if jobs <= 1 || targets.len() <= 1 {
        for (i, target) in targets.iter().enumerate() {
            let res = execute_test(&cli, target, &agc_exe, &temp_root, i);
            if cli.nocapture && !res.output.is_empty() {
                print!("{}", res.output);
            }
            let ms = res.duration.as_millis();
            if res.passed {
                passed_count += 1;
                println!("test {} ... {} ({} ms)", res.name, "ok".green(), ms);
            } else {
                failed_count += 1;
                let msg = res.error_message.as_deref().unwrap_or("failed");
                println!("test {} ... {} ({msg})", res.name, "FAILED".red().bold());
                failures.push((res.name, res.output));
            }
        }
    } else {
        use rayon::prelude::*;
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(jobs)
            .build()
            .unwrap_or_else(|_| rayon::ThreadPoolBuilder::new().num_threads(1).build().unwrap());

        let results: Vec<TestExecutionResult> = pool.install(|| {
            targets
                .par_iter()
                .enumerate()
                .map(|(i, target)| execute_test(&cli, target, &agc_exe, &temp_root, i))
                .collect()
        });

        for res in results {
            if cli.nocapture && !res.output.is_empty() {
                print!("{}", res.output);
            }
            let ms = res.duration.as_millis();
            if res.passed {
                passed_count += 1;
                println!("test {} ... {} ({} ms)", res.name, "ok".green(), ms);
            } else {
                failed_count += 1;
                let msg = res.error_message.as_deref().unwrap_or("failed");
                println!("test {} ... {} ({msg})", res.name, "FAILED".red().bold());
                failures.push((res.name, res.output));
            }
        }
    }

    let _ = std::fs::remove_dir_all(&temp_root);

    let total_secs = overall_start.elapsed().as_secs_f64();

    if !failures.is_empty() {
        println!("\nfailures:\n");
        for (name, output) in &failures {
            println!("---- {name} stdout/stderr ----");
            if output.is_empty() {
                println!("<no output>");
            } else {
                println!("{output}");
            }
        }
        println!("failures:");
        for (name, _) in &failures {
            println!("    {name}");
        }
        println!(
            "\ntest result: {}. {passed_count} passed; {failed_count} failed; finished in {total_secs:.2}s\n",
            "FAILED".red().bold()
        );
        std::process::exit(1);
    } else {
        println!(
            "\ntest result: {}. {passed_count} passed; 0 failed; finished in {total_secs:.2}s\n",
            "ok".green()
        );
        std::process::exit(0);
    }
}

#[allow(dead_code)]
fn _is_ag_file(path: &Path) -> bool {
    path.extension().and_then(|e| e.to_str()) == Some("ag")
}

pub(crate) fn run_semantic_hooks(
    program: &mut parser::Program,
    symbol_table: &mut CompilerSymbolTable,
    imported_modules: &[ModuleArtifact],
) -> Vec<semantic::analyzer::SemanticError> {
    let mut analyzer = Analyzer::new();
    analyzer.inject_imported_modules(imported_modules);
    let mut comptime_cast_hook = ComptimeCastHook::new();
    let mut hooks: [&mut dyn SemanticAnalyzerHook; 1] = [&mut comptime_cast_hook];
    analyzer.analyze_program_with_hooks_and_table(program, &mut hooks, symbol_table)
}

fn apply_llvm_target_metadata(ir: String, target: Option<&str>) -> String {
    let Some(target) = target else {
        return ir;
    };
    if ir.contains("target triple =") {
        return ir;
    }
    let escaped_target = target.replace('\\', "\\\\").replace('"', "\\\"");
    if let Some(first_newline) = ir.find('\n') {
        let (first_line, rest) = ir.split_at(first_newline + 1);
        format!("{first_line}target triple = \"{escaped_target}\"\n{rest}")
    } else {
        format!("target triple = \"{escaped_target}\"\n{ir}")
    }
}

fn validate_target_triple_with_help(target: &str) -> Result<(), String> {
    Target::initialize_all(&InitializationConfig::default());
    let triple = TargetTriple::create(target);
    if Target::from_triple(&triple).is_ok() {
        return Ok(());
    }

    let host = TargetMachine::get_default_triple();
    let host = host.as_str().to_str().unwrap_or("<unknown>");
    let available = list_available_llvm_targets();
    let available_text = if available.is_empty() {
        "  - <none>".to_string()
    } else {
        format!("  - {}", available.join("\n  - "))
    };
    Err(format!(
        "unknown target triple `{target}`.\n\
         host triple: `{host}`\n\
         available LLVM targets:\n{available_text}"
    ))
}

fn list_available_llvm_targets() -> Vec<String> {
    let mut out = Vec::new();
    let mut current = Target::get_first();
    while let Some(target) = current {
        let name = target.get_name().to_str().unwrap_or("<invalid>");
        let desc = target.get_description().to_str().unwrap_or("");
        if desc.is_empty() {
            out.push(name.to_string());
        } else {
            out.push(format!("{name} ({desc})"));
        }
        current = target.get_next();
    }
    out.sort();
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn test_plan() -> CompilePlan {
        CompilePlan {
            emit: EmitKind::Exe,
            inputs: Vec::new(),
            output: PathBuf::from("a.out"),
            package_root: PathBuf::from("."),
            package_roots: Vec::new(),
            package_imports: Vec::new(),
            package_target_kind: None,
            package_target_name: None,
            include_dirs: Vec::new(),
            defines: Vec::new(),
            lib_dirs: Vec::new(),
            libs: Vec::new(),
            opt_level: None,
            debug_info: false,
            target: None,
            sysroot: None,
            no_std: false,
            static_runtime: true,
            static_link: false,
            shared: false,
            verbose: false,
            dry_run: false,
            profile: false,
            leak_check: false,
            cfg_flags: Vec::new(),
            cache_dir: None,
            no_cache: false,
            clean: false,
            cache_info: false,
            cache_prune: None,
            jobs: 0,
            run_mode: false,
            run_args: Vec::new(),
            show_graph: false,
            progress: false,
            auto_output: false,
            warning_config: crate::semantic::linter::WarningConfig::default(),
            test_harness: false,
            tree_shake: false,
        }
    }

    fn unique_temp_dir(label: &str) -> PathBuf {
        let nonce = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("agc-main-{label}-{nonce}"))
    }

    fn test_module_artifact() -> ModuleArtifact {
        ModuleArtifact {
            module_name: "sample".to_string(),
            module_path: "sample".to_string(),
            source_path: String::new(),
            source_hash_fnv1a64: hash_source_text("pub i32 answer() { return 42; }\n"),
            compiler_version: env!("CARGO_PKG_VERSION").to_string(),
            target_triple: "unknown".to_string(),
            code_artifacts: ModuleCodeArtifacts {
                has_static_library: true,
                has_shared_library: false,
            },
            module_deps: Vec::new(),
            transitive_deps: Vec::new(),
            exports: Vec::new(),
            native_libs: Vec::new(),
            native_lib_paths: Vec::new(),
            generic_templates: Vec::new(),
            artifact_path: None,
        }
    }

    #[test]
    fn rejects_module_artifact_from_different_compiler_version() {
        let mut artifact = test_module_artifact();
        artifact.compiler_version = "0.0.0-test".to_string();

        let error = artifact_compatibility_error(&artifact, &test_plan())
            .expect("expected compiler version mismatch");
        assert!(error.contains("compiler version"));
    }

    #[test]
    fn package_target_flags_accept_default_selection() {
        let cli = Cli::try_parse_from(["agc", "--lib"]).expect("bare --lib parses");
        assert_eq!(cli.lib.as_deref(), Some(""));
        assert!(cli.bin.is_none());

        let cli = Cli::try_parse_from(["agc", "--bin", "worker"]).expect("named --bin parses");
        assert_eq!(cli.bin.as_deref(), Some("worker"));
        assert!(cli.lib.is_none());
    }

    #[test]
    fn init_flags_accept_a_target_directory_and_name_override() {
        let cli = Cli::try_parse_from([
            "agc",
            "--init",
            "/tmp/example",
            "--name",
            "custom",
        ])
        .expect("init parses");
        assert!(cli.init);
        assert_eq!(cli.inputs, vec![PathBuf::from("/tmp/example")]);
        assert_eq!(cli.init_name.as_deref(), Some("custom"));
    }

    #[test]
    fn rejects_stale_module_artifact_when_source_changed() {
        let root = unique_temp_dir("stale-artifact");
        std::fs::create_dir_all(&root).unwrap();
        let source_path = root.join("sample.ag");
        let manifest_path = root.join("sample.agm");
        std::fs::write(&source_path, "pub i32 answer() { return 7; }\n").unwrap();

        let mut artifact = test_module_artifact();
        artifact.artifact_path = Some(manifest_path);

        let error = artifact_compatibility_error(&artifact, &test_plan())
            .expect("expected stale source mismatch");
        assert!(error.contains("is stale"));

        let _ = std::fs::remove_dir_all(root);
    }
}
