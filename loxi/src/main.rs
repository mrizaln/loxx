use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use loxi::{run_file, run_prompt, LoxError, RunMode};

#[derive(Parser, Debug)]
#[clap(
    name = "loxi",
    about = "A Lox interpreter (tree-walk interpreter) written in Rust"
)]
struct Args {
    pub source: Option<String>,

    #[arg(long, default_value_t = false, requires = "source", group = "dump")]
    pub dump_lex: bool,

    #[arg(long, default_value_t = false, requires = "source", group = "dump")]
    pub dump_parse: bool,
}

fn main() -> ExitCode {
    coredump::register_panic_handler().unwrap();

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

            let mode = match (args.dump_lex, args.dump_parse) {
                (true, false) => RunMode::DumpLex,
                (false, true) => RunMode::DumpParse,
                _ => RunMode::Normal,
            };

            if let Err(err) = run_file(path, mode) {
                eprintln!("{err}");
                return match err {
                    LoxError::EmptyError => ExitCode::SUCCESS,
                    LoxError::IoError(_) => ExitCode::FAILURE,
                    LoxError::LexError(_) => ExitCode::from(65),
                    LoxError::ParseError => ExitCode::from(65),
                    LoxError::ResolveError => ExitCode::from(65),
                    LoxError::RuntimeError => ExitCode::from(70),
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
