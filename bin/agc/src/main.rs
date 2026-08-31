//! Silver compiler driver binary: argument normalization and entry point.
//!
//! The CLI definition ([`agc::driver::Cli`]) and the compile pipeline live in
//! the `agc` library; this binary only adjusts argv for clap compatibility
//! and hands off to `agc::driver::run`.

use std::{env, ffi::OsString};

use agc::driver::{Cli, run};
use clap::Parser;

fn normalize_argv_for_clap(argv: Vec<OsString>) -> Vec<OsString> {
    if argv.len() < 2 {
        return argv;
    }

    let mut out: Vec<OsString> = Vec::new();
    out.push(argv[0].clone());

    let mut i = 1;
    let mut is_run = false;

    // Check if subcommand is "run", "check", "clean", or "build"
    if let Some(cmd) = argv[1].to_str() {
        if cmd == "run" || cmd == "r" {
            is_run = true;
            out.push(OsString::from("--run"));
            i = 2;
        } else if cmd == "check" || cmd == "c" {
            out.push(OsString::from("--check"));
            i = 2;
        } else if cmd == "clean" {
            out.push(OsString::from("--clean"));
            i = 2;
        } else if cmd == "build" || cmd == "b" {
            i = 2;
        }
    }

    let mut seen_input_file = false;
    let mut in_trailing_args = false;

    while i < argv.len() {
        let arg = &argv[i];
        let arg_str = arg.to_str().unwrap_or("");

        if is_run && arg_str == "--" {
            in_trailing_args = true;
            i += 1;
            continue;
        }

        if in_trailing_args {
            out.push(OsString::from("--run-arg"));
            out.push(arg.clone());
            i += 1;
            continue;
        }

        if arg_str == "-###" || arg_str == "--###" {
            out.push(OsString::from("--dry-run"));
        } else if arg_str == "-g0" {
            // clap shorts are single characters; expose clang-style -g0.
            out.push(OsString::from("--g0"));
        } else if arg_str == "-nc" {
            out.push(OsString::from("--no-cache"));
        } else if is_run && !arg_str.starts_with('-') {
            if !seen_input_file {
                seen_input_file = true;
                out.push(arg.clone());
            } else {
                // Trailing positional argument in run mode: forward as run-arg
                out.push(OsString::from("--run-arg"));
                out.push(arg.clone());
            }
        } else {
            out.push(arg.clone());
        }
        i += 1;
    }

    out
}

fn main() {
    let argv = normalize_argv_for_clap(env::args_os().collect());
    let cli = Cli::parse_from(argv);
    run(cli);
}
