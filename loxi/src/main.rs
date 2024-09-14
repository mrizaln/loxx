use std::path::PathBuf;
use std::process::exit;

use clap::Parser;
use loxi::{run_file, run_prompt, LoxError};

#[derive(Parser, Debug)]
#[clap(
    name = "loxi",
    about = "A Lox interpreter (tree-walk interpreter) written in Rust"
)]
struct Args {
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

            match run_file(path) {
                Ok(_) => (),
                Err(LoxError::EmptyError(_)) => (),
                Err(err) => {
                    eprintln!("{}", err);
                    exit(1);
                }
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
