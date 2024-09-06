use std::path::PathBuf;
use std::process::exit;

use clap::Parser;
use loxi::{run_file, run_prompt};

#[derive(Parser, Debug)]
#[clap(name = "loxi", about = "A Lox interpreter written")]
struct Args {
    #[arg(required = false)]
    pub source: Option<String>,
}

fn main() {
    let args = Args::parse();

    match args.source {
        Some(source) => {
            let path = PathBuf::from(source);

            if !path.exists() {
                eprintln!("File not found: {:?}", path);
                exit(1);
            } else if !path.is_file() {
                eprintln!("Not a file: {:?}", path);
                exit(1);
            }

            if let Err(err) = run_file(path) {
                eprintln!("{}", err);
                exit(1);
            }
        }
        None => {
            if let Err(err) = run_prompt() {
                eprintln!("{}", err);
                exit(1);
            }
        }
    }
}
