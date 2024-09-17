use core::panic;
use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::interp::Interpreter;
use self::lex::{Lexer, ScanResult};
use self::parse::Parser;
use self::util::Location;

mod interp;
mod lex;
mod parse;
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

    #[error("--[ LoxError ]-- {0} Lexing errors occurred, aborting...")]
    LexError(usize),

    #[error("--[ LoxError ]-- Parsing error occured, aborting...")]
    ParseError,

    #[error("--[ LoxError ]-- Runtime error occured, aborting...")]
    RuntimeError,

    #[error("--[ LoxError ]-- Empty file")]
    EmptyError,
}

pub fn run(program: &str) -> Result<(), LoxError> {
    let lexer = Lexer::new(program);
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

    if tokens.len() == 1 {
        return Err(LoxError::EmptyError);
    }

    let parser = Parser::new(&tokens);
    let program = parser.parse().map_err(|err| {
        match err {
            parse::ParseError::SyntaxError { loc, .. } => {
                print_context(&lines, loc);
                println_red!("{}", err);
            }
            parse::ParseError::EndOfFile(loc) => {
                print_context(&lines, loc);
                println_red!("{}", err);
            }
            parse::ParseError::EmptyExpr(loc) => {
                print_context(&lines, loc);
                println_red!("{}", err);
            }
        }
        LoxError::ParseError
    })?;

    let mut interpreter = Interpreter::new();
    let _ = interpreter.interpret(program).map_err(|err| {
        let loc = match err {
            interp::RuntimeError::InvalidBinaryOp(loc, _, _, _) => loc,
            interp::RuntimeError::InvalidUnaryOp(loc, _, _) => loc,
        };
        print_context(&lines, loc);
        println_red!("{}", err);
        LoxError::RuntimeError
    })?;

    Ok(())
}

pub fn run_file(path: PathBuf) -> Result<(), LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path.clone())?;
        file.read_to_string(&mut string)?;

        match string.is_empty() {
            true => return Err(LoxError::EmptyError),
            false => {
                // make sure the content of the file ends with newline
                if string.chars().last().unwrap() != '\n' {
                    string.push('\n');
                }
            }
        }

        string
    };

    run(&contents)?;
    Ok(())
}

pub fn run_prompt() -> io::Result<()> {
    println!("Loxi: a Lox programming language interpreter (currently under construction)");

    let mut line = String::new();
    loop {
        print!(">>> ");
        stdout().flush().expect("Unable to flush stdout");

        match stdin().read_line(&mut line)? {
            0 => break, // EOF reached
            _ => (),
        }

        if let Err(err) = run(&line) {
            println!("{}", err);
        }

        line.clear();
    }

    println!("\nExiting loxi...");
    Ok(())
}

#[rustfmt::skip]
fn print_context(lines: &Vec<&str>, loc: Location) {
    let line = match loc.line > lines.len() {
        true => "",
        false => lines[loc.line - 1],
    };
    println!("{:->width$}", "", width = 80);
    println!("{:>4} |", "");
    println!("{:>4} | {}", loc.line, line);
    println!("{:>4} | \x1b[1m{:>width$}\x1b[1;31m^\x1b[00m", "", "", width = loc.column - 1);
}
