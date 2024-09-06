use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::lex::Lexer;

mod lex;

#[derive(Debug, Error)]
pub enum LoxError {
    #[error("Could not read: {0}")]
    IoError(#[from] io::Error),

    #[error("Lexing error: {0}")]
    LexError(#[from] lex::LexError),
}

pub fn run(program: &str) -> Result<(), LoxError> {
    let tokens = Lexer::new(program).scan_tokens()?;
    tokens.iter().for_each(|tok| {
        println!("{:?}", tok);
    });
    Ok(())
}

pub fn run_file(path: PathBuf) -> Result<(), LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path)?;
        file.read_to_string(&mut string)?;
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
