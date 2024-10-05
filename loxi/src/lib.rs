use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::interp::Interpreter;
use self::lex::{Lexer, ScanResult};
use self::parse::Parser;
use self::resolve::Resolver;
use self::util::Location;

mod interp;
mod lex;
mod parse;
mod resolve;
mod util;

macro_rules! println_red {
    ($fmt:literal, $($arg:tt)*) => {
        let str = format!($fmt, $($arg)*);
        println!("\x1b[1;31m{}\x1b[00m", str)
    };
}

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("--[ LoxError ]-- Could not read file: '{0}'")]
    IoError(#[from] io::Error),

    #[error("--[ LoxError ]-- {0} Lexing errors occurred, aborting.")]
    LexError(usize),

    #[error("--[ LoxError ]-- Parsing error occured, aborting.")]
    ParseError,

    #[error("--[ LoxError ]-- Resolving error occurred, aborting.")]
    ResolveError,

    #[error("--[ LoxError ]-- Runtime error occured, aborting.")]
    RuntimeError,

    #[error("--[ LoxError ]-- Empty file")]
    EmptyError,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum RunMode {
    Normal,
    DumpLex,
    DumpParse,
}

pub fn run(program: &str, mode: RunMode) -> Result<(), LoxError> {
    let mut interpreter = Interpreter::new();
    let interner = interpreter.interner();

    // lexing
    let lexer = Lexer::new(program, interner);
    let ScanResult {
        lines,
        tokens,
        errors,
    } = lexer.scan();

    if !errors.is_empty() {
        errors.iter().for_each(|err| {
            let loc = match err {
                lex::LexError::UnknownToken(loc, _, _) => loc,
                lex::LexError::UnterminatedString(loc) => loc,
                lex::LexError::UnableToParseNumber(loc, _) => loc,
            };
            print_context(&lines, *loc);
            println_red!("{}", err);
        });
        return Err(LoxError::LexError(errors.len()));
    }

    if mode == RunMode::DumpLex {
        for tok in tokens.iter() {
            println!("{}", tok.display(interner));
        }
        return Ok(());
    }

    // only <eof> exist
    if tokens.len() == 1 {
        return Err(LoxError::EmptyError);
    }

    // parsing
    let mut parser = Parser::new();
    let program = parser.parse(tokens).map_err(|err| {
        err.iter().for_each(|e| {
            print_context(&lines, e.loc());
            println_red!("{}", e);
        });
        LoxError::ParseError
    })?;

    if mode == RunMode::DumpParse {
        println!("{}", program.display(interner));
        return Ok(());
    }

    // resolving
    let mut resolver = Resolver::new(interner);
    let resolve_map = resolver.resolve(&program).map_err(|err| {
        print_context(&lines, err.loc());
        println_red!("{}", err);
        LoxError::ResolveError
    })?;

    // interpreting
    interpreter.interpret(program, resolve_map).map_err(|err| {
        print_context(&lines, err.loc());
        println_red!("{}", err);
        LoxError::RuntimeError
    })?;

    Ok(())
}

pub fn run_file(path: PathBuf, mode: RunMode) -> Result<(), LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path.clone())?;
        file.read_to_string(&mut string)?;

        match string.is_empty() {
            true => return Err(LoxError::EmptyError),
            false => {
                // make sure the content of the file ends with newline
                if !string.ends_with('\n') {
                    string.push('\n');
                }
            }
        }

        string
    };

    run(&contents, mode)?;
    Ok(())
}

// FIXME: currently not working like a REPL
pub fn run_prompt() -> io::Result<()> {
    println!("Loxi: a Lox programming language interpreter (currently under construction)");

    let mut line = String::new();
    loop {
        print!(">>> ");
        stdout().flush().expect("Unable to flush stdout");

        if stdin().read_line(&mut line)? == 0 {
            break;
        }

        if let Err(err) = run(&line, RunMode::Normal) {
            println!("{}", err);
        }

        line.clear();
    }

    println!("\nExiting loxi...");
    Ok(())
}

#[rustfmt::skip]
fn print_context(lines: &[&str], loc: Location) {
    let line = match loc.line > lines.len() {
        true => "",
        false => lines[loc.line - 1],
    };
    println!("{:->width$}", "", width = 80);
    println!("{:>4} |", "");
    println!("{:>4} | {}", loc.line, line);
    println!("{:>4} | \x1b[1m{:>width$}\x1b[1;31m^\x1b[00m", "", "", width = loc.column - 1);
}
