use std::path::PathBuf;
use std::process::ExitCode;

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

fn main() -> ExitCode {
    let args = Args::parse();

    match args.source {
        Some(source) => {
            let path = PathBuf::from(source);

            if !path.exists() {
                eprintln!("File not found: {:?}", path);
                return ExitCode::FAILURE;
            } else if !path.is_file() {
                eprintln!("Not a file: {:?}", path);
                return ExitCode::FAILURE;
            }

            if let Err(err) = run_file(path) {
                eprintln!("{err}");
                return match err {
                    LoxError::IoError(_) => ExitCode::FAILURE,
                    LoxError::LexError(_) => ExitCode::from(65),
                    LoxError::ParseError => ExitCode::from(65),
                    LoxError::RuntimeError => ExitCode::from(70),
                    LoxError::EmptyError => ExitCode::SUCCESS,
                };
            }
            ExitCode::SUCCESS
        }
        None => {
            if let Err(err) = run_prompt() {
                eprintln!("{}", err);
                return ExitCode::FAILURE;
            }
            ExitCode::SUCCESS
        }
    }
}
