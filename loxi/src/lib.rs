use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::interp::object::{Value, ValueError};
use self::lex::{Lexer, ScanResult};
use self::parse::Parser;

mod interp;
mod lex;
mod parse;
mod util;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Could not read: {0}")]
    IoError(#[from] io::Error),

    #[error("Lexing error: {0}")]
    LexError(#[from] lex::LexError),

    #[error("Empty file: {0}")]
    EmptyError(PathBuf),
}

pub fn run(program: &str) -> Result<(), LoxError> {
    let ScanResult {
        lines,
        tokens,
        errors,
    } = Lexer::new(program).scan();

    // TODO: pretty print the errors :)
    if !errors.is_empty() {
        errors.iter().for_each(|e| println!("{e}"));
        println!("{} errors happened, aborting...", errors.len());
        return Ok(());
    }

    let parser = Parser::new(&tokens);
    let expr = parser.parse();

    match expr {
        Err(err) => {
            match err {
                #[rustfmt::skip]
                parse::ParseError::SyntaxError { loc, .. } => {
                    let line = match loc.line > lines.len() {
                        true => "",
                        false => lines[loc.line - 1]
                    };
                    println!("{:>4} | {}", loc.line, line);
                    println!("{:>4}   {:->width$}\x1b[1;91m^", "", "", width = loc.column - 1);
                    println!("{err}\x1b[00m");
                }
                parse::ParseError::EndOfFile => {
                    println!("Unexpected EndOfFile at line {}", lines.len())
                }
            };
            return Ok(());
        }
        Ok(ref val) => println!("Expr: {val}"),
    };

    let result = expr.unwrap().eval();
    match result {
        Ok(val) => println!("Eval: {val}"),
        Err(err) => eprintln!("Eval err: {err:?}"),
    }

    Ok(())
}

pub fn run_file(path: PathBuf) -> Result<(), LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path.clone())?;
        file.read_to_string(&mut string)?;

        match string.is_empty() {
            true => return Err(LoxError::EmptyError(path)),
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

pub fn run_prompt() -> Result<(), LoxError> {
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
