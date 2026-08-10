//! Silver compiler driver binary: argument normalization and entry point.
//!
//! The CLI definition ([`agc::driver::Cli`]) and the compile pipeline live in
//! the `agc` library; this binary only adjusts argv for clap compatibility
//! and hands off to `agc::driver::run`.

use std::{env, ffi::OsString};

use agc::driver::{Cli, run};
use clap::Parser;

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
    run(cli);
}
