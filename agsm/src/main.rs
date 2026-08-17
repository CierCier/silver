use std::env;

fn main() {
    let wants_help = env::args().any(|arg| arg == "--help" || arg == "-h");
    if wants_help || env::args().len() == 1 {
        println!(
            "agsm - Silver foreign sourcemap module builder\n\nUsage:\n  agsm build [OPTIONS] [SOURCEMAP]\n\nOptions:\n  -o, --output <PATH>  Write the generated .agm to PATH\n  -h, --help           Print help"
        );
        return;
    }

    eprintln!("agsm: command implementation is not available yet; use --help");
    std::process::exit(2);
}
