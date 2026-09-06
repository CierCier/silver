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

    let command_index = find_command_index(&argv);
    let mut i = 1;
    let mut is_run = false;

    // Check if a command appears after leading options. The command is
    // rewritten into the hidden flag consumed by the existing Cli parser.
    if let Some(command_index) = command_index {
        out.extend(argv[1..command_index].iter().cloned());
        let cmd = argv[command_index].to_str().unwrap_or("");
        if cmd == "init" {
            i = command_index + 1;
            out.push(OsString::from("--init"));
        } else if cmd == "run" || cmd == "r" {
            is_run = true;
            out.push(OsString::from("--run"));
            i = command_index + 1;
        } else if cmd == "check" || cmd == "c" {
            out.push(OsString::from("--check"));
            i = command_index + 1;
        } else if cmd == "clean" {
            out.push(OsString::from("--clean"));
            i = command_index + 1;
        } else if cmd == "build" || cmd == "b" {
            i = command_index + 1;
        }
    }

    let mut seen_input_file = false;
    let mut in_trailing_args = false;
    let mut pending_option_value = false;

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

        if pending_option_value {
            out.push(arg.clone());
            pending_option_value = false;
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
        } else if is_run && run_option_requires_value(arg_str) {
            out.push(arg.clone());
            pending_option_value = true;
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

fn find_command_index(argv: &[OsString]) -> Option<usize> {
    let mut i = 1;
    while i < argv.len() {
        let arg = argv[i].to_str().unwrap_or("");
        if arg == "--" {
            return None;
        }
        if arg.starts_with('-') {
            if command_scan_option_requires_value(arg) {
                i = i.saturating_add(2);
            } else {
                i += 1;
            }
            continue;
        }
        return matches!(
            arg,
            "init" | "build" | "b" | "run" | "r" | "check" | "c" | "clean"
        )
            .then_some(i);
    }
    None
}

fn command_scan_option_requires_value(arg: &str) -> bool {
    matches!(
        arg,
        "-o"
            | "--output"
            | "-I"
            | "--root"
            | "--bin"
            | "--lib"
            | "--name"
            | "-D"
            | "-L"
            | "-l"
            | "-W"
            | "--target"
            | "--sysroot"
            | "--cfg"
            | "--cache-dir"
            | "-j"
            | "--jobs"
            | "--emit"
            | "-O"
            | "--run-arg"
    )
}

fn run_option_requires_value(arg: &str) -> bool {
    matches!(
        arg,
        "-o"
            | "--output"
            | "-I"
            | "--root"
            | "-D"
            | "-L"
            | "-l"
            | "-W"
            | "--target"
            | "--sysroot"
            | "--cfg"
            | "--cache-dir"
            | "-j"
            | "--jobs"
            | "--emit"
            | "--run-arg"
    )
}

fn main() {
    let argv = normalize_argv_for_clap(env::args_os().collect());
    let cli = Cli::parse_from(argv);
    run(cli);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn run_option_values_are_not_forwarded_as_program_arguments() {
        let normalized = normalize_argv_for_clap(vec![
            OsString::from("agc"),
            OsString::from("run"),
            OsString::from("-I"),
            OsString::from("/tmp/modules"),
            OsString::from("-L"),
            OsString::from("/tmp/libs"),
            OsString::from("main.ag"),
            OsString::from("arg"),
        ]);

        assert_eq!(
            normalized,
            vec![
                OsString::from("agc"),
                OsString::from("--run"),
                OsString::from("-I"),
                OsString::from("/tmp/modules"),
                OsString::from("-L"),
                OsString::from("/tmp/libs"),
                OsString::from("main.ag"),
                OsString::from("--run-arg"),
                OsString::from("arg"),
            ]
        );
    }

    #[test]
    fn commands_after_leading_options_are_normalized() {
        let normalized = normalize_argv_for_clap(vec![
            OsString::from("agc"),
            OsString::from("--no-progress"),
            OsString::from("--root"),
            OsString::from("/tmp/project"),
            OsString::from("run"),
            OsString::from("main.ag"),
            OsString::from("arg"),
        ]);

        assert_eq!(
            normalized,
            vec![
                OsString::from("agc"),
                OsString::from("--no-progress"),
                OsString::from("--root"),
                OsString::from("/tmp/project"),
                OsString::from("--run"),
                OsString::from("main.ag"),
                OsString::from("--run-arg"),
                OsString::from("arg"),
            ]
        );
    }

    #[test]
    fn init_command_is_normalized_after_leading_options() {
        let normalized = normalize_argv_for_clap(vec![
            OsString::from("agc"),
            OsString::from("--name"),
            OsString::from("custom"),
            OsString::from("init"),
            OsString::from("project"),
        ]);

        assert_eq!(
            normalized,
            vec![
                OsString::from("agc"),
                OsString::from("--name"),
                OsString::from("custom"),
                OsString::from("--init"),
                OsString::from("project"),
            ]
        );
    }
}
