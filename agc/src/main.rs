//! Silver compiler driver binary: CLI parsing and entry point.
//!
//! The CLI definition ([`Cli`]) and argument normalization live here; the
//! compile pipeline is in `driver` and the linker in `link`.

mod driver;
mod link;

use std::path::PathBuf;
use std::{env, ffi::OsString};

use clap::{ArgAction, Parser};
use driver::EmitKind;

#[derive(Parser, Debug)]
#[command(
    name = "agc",
    version = concat!(
        env!("CARGO_PKG_VERSION"), "\n",
        "version: ", env!("GIT_DESCRIBE"), "\n",
        "commit: ", env!("GIT_SHA")
    ),
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

    /// Link statically with the no-libc runtime. Always enabled: Silver never
    /// links against libc, so every binary is fully static by default. The
    /// flag exists for explicitness and backwards compatibility.
    #[arg(long = "static-runtime", action = ArgAction::SetTrue, default_value_t = true)]
    static_runtime: bool,

    /// Prefer shared module artifacts and emit shared libraries for module packaging
    #[arg(long = "shared", action = ArgAction::SetTrue)]
    shared: bool,

    /// Verbose output
    #[arg(short = 'v', long = "verbose", action = ArgAction::SetTrue)]
    verbose: bool,

    /// Print commands/plan but do not execute (clang-style: also accepts -###)
    #[arg(long = "dry-run", action = ArgAction::SetTrue)]
    dry_run: bool,

    /// Enable time and memory profiling output
    #[arg(long = "profile", action = ArgAction::SetTrue)]
    profile: bool,
    /// Enable allocator leak-check, double-free, and buffer overflow diagnostics
    #[arg(long = "leak-check", action = ArgAction::SetTrue)]
    leak_check: bool,
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

fn main() {
    let argv = normalize_argv_for_clap(env::args_os().collect());
    let cli = Cli::parse_from(argv);
    driver::run(cli);
}
