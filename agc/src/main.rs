use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use std::{env, ffi::OsString};

use crate::attributes::{collect_program_link_libraries, extend_unique_libs};
use crate::module_artifact::{
    ModuleArtifact, ModuleCodeArtifacts, hash_source_text, module_name_from_path,
};
use crate::module_loader::{ModuleLoader, module_loader_default_dirs};
use clap::{ArgAction, Parser, ValueEnum};
use inkwell::targets::{InitializationConfig, Target, TargetMachine, TargetTriple};
use owo_colors::OwoColorize;
use semantic::analyzer::{Analyzer, SemanticAnalyzerHook};
use semantic::comptime_cast_hook::ComptimeCastHook;
use semantic::typeck::TypeChecker;
use symbol_table::{CompilerPhase, CompilerSymbolTable};

mod ast_tree;
mod attributes;
mod codegen;
mod diagnostics;
mod lexer;
mod module_artifact;
mod module_loader;
mod parser;
mod semantic;
mod symbol_table;
mod traits;
mod types;

#[derive(Debug, Copy, Clone, Eq, PartialEq, ValueEnum)]
enum EmitKind {
    /// Link an executable.
    Exe,
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
struct CompilePlan {
    emit: EmitKind,
    inputs: Vec<PathBuf>,
    output: PathBuf,
    package_root: PathBuf,
    include_dirs: Vec<PathBuf>,
    defines: Vec<String>,
    lib_dirs: Vec<PathBuf>,
    libs: Vec<String>,
    opt_level: Option<String>,
    debug_info: bool,
    target: Option<String>,
    sysroot: Option<PathBuf>,
    no_std: bool,
    shared: bool,
    verbose: bool,
    dry_run: bool,
}

impl CompilePlan {
    fn describe_for_driver(&self) -> String {
        let mut parts: Vec<String> = Vec::new();
        parts.push(format!("emit={:?}", self.emit));
        parts.push(format!("output={}", self.output.display()));
        parts.push(format!("root={}", self.package_root.display()));

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

#[derive(Parser, Debug)]
#[command(
    name = "agc",
    version,
    about = "Silver compiler (clang-like driver)",
    long_about = "A clang-like driver for the Silver compiler"
)]
struct Cli {
    /// Input source files (.ag). Optional for --emit=grammar.
    #[arg(value_name = "FILE")]
    inputs: Vec<PathBuf>,

    /// Write output to <file>
    #[arg(short = 'o', value_name = "FILE")]
    output: Option<PathBuf>,

    /// Compile and assemble, but do not link (emit .o)
    #[arg(short = 'c', action = ArgAction::SetTrue)]
    compile_only: bool,

    /// Compile only and emit assembly (.s)
    #[arg(short = 'S', action = ArgAction::SetTrue)]
    emit_asm: bool,

    /// Emit LLVM IR instead of native output (.ll)
    #[arg(long = "emit-llvm", action = ArgAction::SetTrue)]
    emit_llvm: bool,

    /// Explicitly select the output kind (overrides -c/-S/--emit-llvm)
    #[arg(long = "emit", value_enum)]
    emit: Option<EmitKind>,

    /// Optimization level: 0,1,2,3,s,z,fast (accepts clang-style -O2)
    #[arg(short = 'O', value_name = "LEVEL", default_missing_value = "2", num_args = 0..=1)]
    opt_level: Option<String>,

    /// Generate debug information
    #[arg(short = 'g', action = ArgAction::SetTrue)]
    debug_info: bool,

    /// Add directory to include search path
    #[arg(short = 'I', value_name = "DIR", action = ArgAction::Append)]
    include_dirs: Vec<PathBuf>,

    /// Add a primary module include root (defaults to current working directory)
    #[arg(long = "root", value_name = "DIR")]
    root: Option<PathBuf>,

    /// Define a preprocessor symbol (accepted for clang-compat; not yet used)
    #[arg(short = 'D', value_name = "NAME[=VALUE]", action = ArgAction::Append)]
    defines: Vec<String>,

    /// Add directory to library search path
    #[arg(short = 'L', value_name = "DIR", action = ArgAction::Append)]
    lib_dirs: Vec<PathBuf>,

    /// Link with library
    #[arg(short = 'l', value_name = "LIB", action = ArgAction::Append)]
    libs: Vec<String>,

    /// Compile for the given target triple
    #[arg(long = "target", value_name = "TRIPLE")]
    target: Option<String>,

    /// Use the given sysroot
    #[arg(long = "sysroot", value_name = "DIR")]
    sysroot: Option<PathBuf>,

    /// Do not link the standard library (accepted for clang-compat; not yet used)
    #[arg(long = "no-std", action = ArgAction::SetTrue)]
    no_std: bool,

    /// Prefer shared module artifacts and emit shared libraries for module packaging
    #[arg(long = "shared", action = ArgAction::SetTrue)]
    shared: bool,

    /// Verbose output
    #[arg(short = 'v', long = "verbose", action = ArgAction::SetTrue)]
    verbose: bool,

    /// Print commands/plan but do not execute (clang-style: also accepts -###)
    #[arg(long = "dry-run", action = ArgAction::SetTrue)]
    dry_run: bool,
}

fn derive_emit(cli: &Cli) -> Result<EmitKind, String> {
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

fn default_output_for(emit: EmitKind, inputs: &[PathBuf]) -> PathBuf {
    match emit {
        EmitKind::Exe => PathBuf::from("a.out"),
        EmitKind::Obj => with_ext_or_default(inputs, "o"),
        EmitKind::Asm => with_ext_or_default(inputs, "s"),
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
    let emit = derive_emit(&cli)?;

    if cli.inputs.is_empty() && emit != EmitKind::Grammar {
        return Err("at least one input file is required (except for --emit=grammar)".to_string());
    }

    // For now keep multi-input support limited to link stage, like most compilers.
    if cli.inputs.len() > 1 {
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

    let output = cli
        .output
        .unwrap_or_else(|| default_output_for(emit, &cli.inputs));
    let package_root = match cli.root {
        Some(root) => root,
        None => env::current_dir()
            .map_err(|e| format!("failed to determine current working directory: {e}"))?,
    };

    Ok(CompilePlan {
        emit,
        inputs: cli.inputs,
        output,
        package_root,
        include_dirs: cli.include_dirs,
        defines: cli.defines,
        lib_dirs: cli.lib_dirs,
        libs: cli.libs,
        opt_level: cli.opt_level,
        debug_info: cli.debug_info,
        target: cli.target,
        sysroot: cli.sysroot,
        no_std: cli.no_std,
        shared: cli.shared,
        verbose: cli.verbose,
        dry_run: cli.dry_run,
    })
}

fn build_module_loader(plan: &CompilePlan) -> ModuleLoader {
    let mut loader = ModuleLoader::new();
    // Search roots (checked after relative path and cwd): --root, then -I, then sysroot.
    loader.add_search_dir(&plan.package_root);

    for dir in &plan.include_dirs {
        loader.add_search_dir(dir);
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

fn module_binary_output_path(manifest_path: &Path, shared: bool) -> PathBuf {
    manifest_path.with_extension(if shared { "so" } else { "o" })
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
    if module.compiler_version != expected_version {
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

fn main() {
    let argv = normalize_argv_for_clap(env::args_os().collect());
    let cli = Cli::parse_from(argv);
    match derive_plan(cli) {
        Ok(plan) => {
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
                                        &src,
                                        &input.display().to_string(),
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

                    match lexer::lex(&src) {
                        Ok(tokens) => {
                            let mut parser = parser::Parser::new_with_source(
                                tokens,
                                input.display().to_string(),
                            );
                            let (ast, errors) = parser.parse_program();

                            if !errors.is_empty() {
                                eprintln!("agc: parser errors:");
                                for error in &errors {
                                    eprintln!(
                                        "{}",
                                        diagnostics::render(
                                            &src,
                                            &input.display().to_string(),
                                            error.span().clone(),
                                            &error.format_with_help(),
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
                                        &src,
                                        &input.display().to_string(),
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

            let loader = build_module_loader(&plan);

            let mut llvm_units: Vec<(PathBuf, String)> = Vec::new();
            let mut native_libs = plan.libs.clone();
            let mut dependency_link_artifacts: Vec<PathBuf> = Vec::new();

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

                let tokens = match lexer::lex(&src) {
                    Ok(tokens) => tokens,
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
                                    &src,
                                    &input.display().to_string(),
                                    e.span,
                                    &format!("{:?}", e.kind),
                                    diagnostics::Severity::Error,
                                )
                            );
                        }
                        std::process::exit(2);
                    }
                };

                let mut parser =
                    parser::Parser::new_with_source(tokens, input.display().to_string());
                let (mut ast, errors) = parser.parse_program();

                if !errors.is_empty() {
                    eprintln!("agc: parser errors:");
                    for error in &errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                &src,
                                &input.display().to_string(),
                                error.span().clone(),
                                &error.format_with_help(),
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    if ast.items.is_empty() {
                        std::process::exit(2);
                    }
                }

                let pre_lowering_link_libs = match collect_program_link_libraries(&ast) {
                    Ok(libs) => libs,
                    Err(error) => {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                &src,
                                &input.display().to_string(),
                                error.span,
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                        std::process::exit(2);
                    }
                };

                let base_dir = input.parent();
                let import_lowering = match parser::FileImportResolverHook::new(&loader)
                    .lower_program_imports(&mut ast, base_dir, Some(input))
                {
                    Ok(result) => result,
                    Err(error) => {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                };
                let module_dependencies = import_lowering.module_dependencies;
                let imported_modules = import_lowering.module_artifacts;
                for module in &imported_modules {
                    if let Some(error) = artifact_compatibility_error(module, &plan) {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                }

                let mut symbol_table = CompilerSymbolTable::new();
                symbol_table.touch_phase(CompilerPhase::Parse, "parse complete");
                symbol_table.record_program_symbols(&ast, CompilerPhase::Parse);

                let semantic_errors =
                    run_semantic_hooks(&mut ast, &mut symbol_table, &imported_modules);
                if !semantic_errors.is_empty() {
                    eprintln!("agc: semantic errors:");
                    for error in &semantic_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                &src,
                                &input.display().to_string(),
                                error.span.clone(),
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }

                let (type_errors, monomorphs) = TypeChecker::new()
                    .with_imported_modules(&imported_modules)
                    .check_program_with_table(&ast, &mut symbol_table);
                if !type_errors.is_empty() {
                    eprintln!("agc: type errors:");
                    for error in &type_errors {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                &src,
                                &input.display().to_string(),
                                error.span.clone(),
                                &error.message,
                                diagnostics::Severity::Error,
                            )
                        );
                    }
                    std::process::exit(2);
                }

                let program_link_libs = match collect_program_link_libraries(&ast) {
                    Ok(libs) => libs,
                    Err(error) => {
                        eprintln!(
                            "{}",
                            diagnostics::render(
                                &src,
                                &input.display().to_string(),
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
                }
                match collect_dependency_link_artifacts(&loader, &imported_modules, &plan, plan.shared) {
                    Ok(paths) => {
                        for path in paths {
                            if !dependency_link_artifacts.contains(&path) {
                                dependency_link_artifacts.push(path);
                            }
                        }
                    }
                    Err(error) => {
                        eprintln!("agc: {}: {error}", "error".red().bold());
                        std::process::exit(2);
                    }
                }

                semantic::monomorph::append_monomorphs(&mut ast, &monomorphs);
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
                    let binary_output = module_binary_output_path(&plan.output, plan.shared);
                    if plan.shared {
                        let temp_object = plan.output.with_extension("module.tmp.o");
                        let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table(
                            &ast,
                            &imported_modules,
                            &temp_object,
                            plan.target.as_deref(),
                            plan.opt_level.as_deref(),
                            &mut symbol_table,
                        );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        &src,
                                        &input.display().to_string(),
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
                        let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table(
                            &ast,
                            &imported_modules,
                            &binary_output,
                            plan.target.as_deref(),
                            plan.opt_level.as_deref(),
                            &mut symbol_table,
                        );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        &src,
                                        &input.display().to_string(),
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
                        let result = codegen::llvm_ir::LlvmIrGenerator::emit_object_file_with_imports_and_table(
                            &ast,
                            &imported_modules,
                            &plan.output,
                            plan.target.as_deref(),
                            plan.opt_level.as_deref(),
                            &mut symbol_table,
                        );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        &src,
                                        &input.display().to_string(),
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
                    } else if matches!(plan.emit, EmitKind::Asm) {
                        let result =
                            codegen::llvm_ir::LlvmIrGenerator::emit_assembly_file_with_imports_and_table(
                                &ast,
                                &imported_modules,
                                &plan.output,
                                plan.target.as_deref(),
                                plan.opt_level.as_deref(),
                                &mut symbol_table,
                            );
                        if let Err(error) = result {
                            if let Some(span) = error.span {
                                eprintln!(
                                    "{}",
                                    diagnostics::render(
                                        &src,
                                        &input.display().to_string(),
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
                    } else {
                        let output = codegen::llvm_ir::LlvmIrGenerator::generate_with_imports_and_table(
                            &ast,
                            &imported_modules,
                            &mut symbol_table,
                        );
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
                                            &src,
                                            &input.display().to_string(),
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
                return;
            }

            if matches!(plan.emit, EmitKind::Obj | EmitKind::Asm | EmitKind::Module) {
                return;
            }

            if matches!(plan.emit, EmitKind::Exe) {
                if let Err(e) = build_with_llvm_tools(
                    &plan,
                    &llvm_units,
                    &dependency_link_artifacts,
                    &native_libs,
                ) {
                    eprintln!("agc: {}: {e}", "error".red().bold());
                    std::process::exit(2);
                }
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

#[allow(dead_code)]
fn _is_ag_file(path: &Path) -> bool {
    path.extension().and_then(|e| e.to_str()) == Some("ag")
}

fn run_semantic_hooks(
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

fn run_tool(mut command: Command, label: &str) -> Result<(), String> {
    let output = command
        .output()
        .map_err(|e| format!("failed to run {label}: {e}"))?;
    if output.status.success() {
        return Ok(());
    }
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let details = if !stderr.trim().is_empty() {
        stderr.trim().to_string()
    } else {
        stdout.trim().to_string()
    };
    Err(format!(
        "{label} failed: {}",
        if details.is_empty() {
            "<no tool output>".to_string()
        } else {
            details
        }
    ))
}

fn map_llc_opt_level(level: Option<&str>) -> &'static str {
    match level.unwrap_or("0") {
        "0" => "0",
        "1" => "1",
        "2" | "s" | "z" => "2",
        "3" | "fast" => "3",
        _ => "2",
    }
}

fn build_with_llvm_tools(
    plan: &CompilePlan,
    units: &[(PathBuf, String)],
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    if units.is_empty() {
        return Err("no LLVM units generated".to_string());
    }

    let pid = std::process::id();
    let nonce = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map_err(|e| format!("failed to compute temp dir nonce: {e}"))?
        .as_nanos();
    let temp_dir = std::env::temp_dir().join(format!("agc-llvm-{pid}-{nonce}"));
    std::fs::create_dir_all(&temp_dir)
        .map_err(|e| format!("failed to create temp dir {}: {e}", temp_dir.display()))?;

    let mut ll_paths = Vec::with_capacity(units.len());
    for (index, (input, ir)) in units.iter().enumerate() {
        let stem = input.file_stem().and_then(|s| s.to_str()).unwrap_or("unit");
        let ll_path = temp_dir.join(format!("{index:03}_{stem}.ll"));
        std::fs::write(&ll_path, ir)
            .map_err(|e| format!("failed to write {}: {e}", ll_path.display()))?;
        ll_paths.push(ll_path);
    }

    let linked_ll = if ll_paths.len() == 1 {
        ll_paths[0].clone()
    } else {
        let merged = temp_dir.join("linked.ll");
        let mut llvm_link = Command::new("llvm-link");
        llvm_link.arg("-o").arg(&merged);
        for path in &ll_paths {
            llvm_link.arg(path);
        }
        run_tool(llvm_link, "llvm-link")?;
        merged
    };

    let object_path = temp_dir.join("linked.o");
    let mut llc = Command::new("llc");
    llc.arg("-filetype=obj")
        .arg(format!(
            "-O{}",
            map_llc_opt_level(plan.opt_level.as_deref())
        ))
        .arg("-o")
        .arg(&object_path);
    if let Some(target) = &plan.target {
        llc.arg("-mtriple").arg(target);
    }
    llc.arg(&linked_ll);
    run_tool(llc, "llc")?;

    let link_result = link_with_ld_lld(plan, &object_path, dependency_paths, native_libs).or_else(|ld_err| {
        // Keep a compatibility fallback for systems without lld installed.
        link_with_cc(plan, &object_path, dependency_paths, native_libs).map_err(|cc_err| {
            format!("ld.lld path failed: {ld_err}; fallback linker failed: {cc_err}")
        })
    });
    link_result?;

    let _ = std::fs::remove_dir_all(&temp_dir);
    Ok(())
}

fn command_exists(name: &str) -> bool {
    std::env::var_os("PATH")
        .map(|paths| std::env::split_paths(&paths).any(|p| p.join(name).is_file()))
        .unwrap_or(false)
}

fn cc_query(arg: &str) -> Result<String, String> {
    let output = Command::new("cc")
        .arg(arg)
        .output()
        .map_err(|e| format!("failed to query cc {arg}: {e}"))?;
    if !output.status.success() {
        return Err(format!(
            "cc {arg} failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn cc_library_dirs() -> Result<Vec<PathBuf>, String> {
    let output = cc_query("-print-search-dirs")?;
    let mut dirs = Vec::new();
    for line in output.lines() {
        if let Some(rest) = line.strip_prefix("libraries: =") {
            for raw in rest.split(':') {
                if raw.is_empty() {
                    continue;
                }
                dirs.push(PathBuf::from(raw));
            }
        }
    }
    Ok(dirs)
}

fn default_dynamic_linker(target: Option<&str>) -> Option<&'static str> {
    match target.unwrap_or("") {
        t if t.contains("aarch64") => Some("/lib/ld-linux-aarch64.so.1"),
        t if t.contains("x86_64") || t.is_empty() => Some("/lib64/ld-linux-x86-64.so.2"),
        _ => None,
    }
}

fn should_force_non_pie(target: Option<&str>) -> bool {
    match target {
        Some(triple) => triple.contains("linux"),
        None => cfg!(target_os = "linux"),
    }
}

fn dependency_library_dirs(dependency_paths: &[PathBuf]) -> Vec<PathBuf> {
    let mut dirs = Vec::new();
    for path in dependency_paths {
        let Some(parent) = path.parent() else {
            continue;
        };
        let parent = parent.to_path_buf();
        if !dirs.contains(&parent) {
            dirs.push(parent);
        }
    }
    dirs
}

fn link_with_ld_lld(
    plan: &CompilePlan,
    object_path: &Path,
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let lld_name = if command_exists("ld.lld") {
        "ld.lld"
    } else if command_exists("lld") {
        "lld"
    } else {
        return Err("ld.lld/lld not found in PATH".to_string());
    };

    let mut link = Command::new(lld_name);
    if lld_name == "lld" {
        link.arg("-flavor").arg("gnu");
    }
    link.arg("-o").arg(&plan.output);

    if let Some(target) = &plan.target {
        link.arg("-mtriple").arg(target);
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    if let Some(loader) = default_dynamic_linker(plan.target.as_deref()) {
        link.arg("-dynamic-linker").arg(loader);
    }

    if !plan.no_std {
        for crt in ["crt1.o", "crti.o", "crtbegin.o"] {
            let path = cc_query(&format!("-print-file-name={crt}"))?;
            if path != crt {
                link.arg(path);
            }
        }
    }

    link.arg(object_path);
    for dep in dependency_paths {
        link.arg(dep);
    }

    for dir in cc_library_dirs()? {
        link.arg("-L").arg(dir);
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg("-rpath").arg(&dir);
    }

    if !plan.no_std {
        link.arg("-lc").arg("-lgcc_s").arg("-lgcc");
        for crt in ["crtend.o", "crtn.o"] {
            let path = cc_query(&format!("-print-file-name={crt}"))?;
            if path != crt {
                link.arg(path);
            }
        }
    }
    for lib in native_libs {
        link.arg(format!("-l{lib}"));
    }

    run_tool(link, "ld.lld")
}

fn link_with_cc(
    plan: &CompilePlan,
    object_path: &Path,
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let mut link = Command::new("cc");
    link.arg("-o").arg(&plan.output).arg(object_path);
    for dep in dependency_paths {
        link.arg(dep);
    }
    if should_force_non_pie(plan.target.as_deref()) {
        link.arg("-no-pie");
    }
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    if plan.debug_info {
        link.arg("-g");
    }
    if plan.no_std {
        link.arg("-nostdlib");
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for lib in native_libs {
        link.arg(format!("-l{lib}"));
    }
    run_tool(link, "cc linker")
}

fn link_shared_module(
    plan: &CompilePlan,
    object_path: &Path,
    output_path: &Path,
    dependency_paths: &[PathBuf],
    native_libs: &[String],
) -> Result<(), String> {
    let mut link = Command::new("cc");
    link.arg("-shared").arg("-o").arg(output_path).arg(object_path);
    if let Some(sysroot) = &plan.sysroot {
        link.arg("--sysroot").arg(sysroot);
    }
    for dep in dependency_paths {
        link.arg(dep);
    }
    for dir in &plan.lib_dirs {
        link.arg("-L").arg(dir);
    }
    for dir in dependency_library_dirs(dependency_paths) {
        link.arg("-L").arg(&dir);
        link.arg(format!("-Wl,-rpath,{}", dir.display()));
    }
    for lib in native_libs {
        link.arg(format!("-l{lib}"));
    }
    run_tool(link, "cc shared linker")
}

fn normalize_argv_for_clap(argv: Vec<OsString>) -> Vec<OsString> {
    argv.into_iter()
        .map(|a| {
            if a == "-###" || a == "--###" {
                OsString::from("--dry-run")
            } else {
                a
            }
        })
        .collect()
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
            include_dirs: Vec::new(),
            defines: Vec::new(),
            lib_dirs: Vec::new(),
            libs: Vec::new(),
            opt_level: None,
            debug_info: false,
            target: None,
            sysroot: None,
            no_std: false,
            shared: false,
            verbose: false,
            dry_run: false,
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
            exports: Vec::new(),
            native_libs: Vec::new(),
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
