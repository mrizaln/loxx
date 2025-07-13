use std::path::PathBuf;
use std::process::ExitCode;

use clap::Parser;
use loxi::Mode;
use loxii::run_file;

#[derive(Parser, Debug)]
#[clap(
    name = "loxi",
    about = "A Lox interpreter (bytecode interpreter) written in Rust"
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
                (true, false) => Mode::DumpLex,
                (false, true) => Mode::DumpParse,
                _ => Mode::Normal,
            };

            match run_file(path, mode) {
                Err(err) => {
                    eprintln!("{err}");
                    ExitCode::FAILURE
                }
                Ok(_) => ExitCode::SUCCESS,
            }
        }
        None => {
            unimplemented!("soon REPL will be implemented");
            ExitCode::SUCCESS
        }
    }
}
