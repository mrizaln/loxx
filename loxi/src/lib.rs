use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::lex::{Lexer, ScanResult};

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

    println!("\nlines: {}", lines.len());
    lines.iter().for_each(|line| println!("{line:?}"));

    println!("\ntokens: {}", tokens.len());
    tokens.iter().for_each(|tok| println!("{tok}"));

    println!("\nerrors: {}", errors.len());
    errors.iter().for_each(|err| println!("{err}"));

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
