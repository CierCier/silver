use std::path::PathBuf;
use std::process::ExitCode;

use clap::{Args, CommandFactory, Parser, Subcommand};

#[derive(Debug, Parser)]
#[command(name = "agsm", about = "Silver foreign sourcemap module builder")]
struct Cli {
    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(Debug, Subcommand)]
enum Command {
    Build(BuildArgs),
}

#[derive(Debug, Args)]
struct BuildArgs {
    #[arg(default_value = "sourcemap.toml")]
    sourcemap: PathBuf,
    #[arg(short = 'o', long)]
    output: Option<PathBuf>,
    #[arg(long)]
    target: Option<String>,
    #[arg(short = 'I', long = "include-path", value_name = "DIR")]
    include_paths: Vec<PathBuf>,
    #[arg(short = 'L', long = "lib-path", value_name = "DIR")]
    lib_paths: Vec<PathBuf>,
    #[arg(short = 'D', long = "define", value_name = "NAME[=VALUE]")]
    defines: Vec<String>,
}

fn main() -> ExitCode {
    let Some(Command::Build(args)) = Cli::parse().command else {
        println!("{}", Cli::command().render_help());
        return ExitCode::SUCCESS;
    };
    let options = agsm::build::BuildOptions {
        sourcemap: args.sourcemap,
        output: args.output,
        target: args.target,
        include_paths: args.include_paths,
        lib_paths: args.lib_paths,
        defines: args.defines,
    };
    match agsm::build::build(&options) {
        Ok(output) => {
            println!("{}", output.display());
            ExitCode::SUCCESS
        }
        Err(error) => {
            eprintln!("agsm: error: {error}");
            ExitCode::from(2)
        }
    }
}
