use clap::Parser;

#[derive(Parser, Debug)]
#[command(name = "agco")]
#[command(version = env!("SILVER_GIT_DESCRIBE"))]
#[command(about = "The Silver Programming Language Compiler (Rust Refactor)", long_about = None)]
struct Args {
    #[arg(short, long)]
    verbose: bool,

    /// Input file
    input: Option<String>,
}

fn main() {
    let args = Args::parse();

    println!("Silver Compiler (agco) - Refactor");
    println!("Version: {}", env!("SILVER_GIT_DESCRIBE"));

    if let Some(input_path) = args.input {
        let code = std::fs::read_to_string(&input_path).expect("Failed to read input file");

        if args.verbose {
            println!("Compiling: {}", input_path);
            println!("Source Code:\n{}", code);
        }

        match agco::compile(&code, args.verbose) {
            Ok(program) => {
                println!("Parse Validated.");
                if args.verbose {
                    println!("AST:\n{:#?}", program);
                }
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    } else {
        println!("No input file provided.");
    }
}
