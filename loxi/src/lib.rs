use std::fs::File;
use std::io::{self, stdin, stdout, Read, Write};
use std::path::PathBuf;
use thiserror::Error;

use self::interp::Interpreter;
use self::lex::{Lexer, ScanResult};
use self::parse::Parser;
use self::resolve::Resolver;
use self::util::Location;

pub mod lex;
pub mod native_fn;
pub mod parse;
pub mod util;

pub use interp::interner;

mod interp;
mod resolve;

#[macro_export]
macro_rules! println_red {
    ($fmt:literal, $($arg:tt)*) => {
        let str = format!($fmt, $($arg)*);
        println!("\x1b[1;31m{}\x1b[00m", str)
    };
}

#[macro_export]
macro_rules! eprintln_red {
    ($fmt:literal, $($arg:tt)*) => {
        let str = format!($fmt, $($arg)*);
        eprintln!("\x1b[1;31m{}\x1b[00m", str)
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
pub enum Mode {
    Normal,
    DumpLex,
    DumpParse,
    Repl,
}

pub fn run(interpreter: &mut Interpreter, program: &str, mode: Mode) -> Result<(), LoxError> {
    let (interner, ast) = interpreter.interner_and_ast_mut();

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
            eprint_context(&lines, *loc);
            eprintln_red!("{}", err);
        });
        return Err(LoxError::LexError(errors.len()));
    }

    if mode == Mode::DumpLex {
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
    let parse_mode = match mode {
        Mode::Repl => parse::Mode::Repl,
        _ => parse::Mode::Script,
    };
    let mut parser = Parser::new(ast, parse_mode);
    let program = parser.parse(tokens).map_err(|err| {
        err.iter().for_each(|e| {
            eprint_context(&lines, e.loc());
            eprintln_red!("{}", e);
        });
        LoxError::ParseError
    })?;

    if mode == Mode::DumpParse {
        print!("{}", program.display(interner, ast));
        return Ok(());
    }

    // resolving
    let mut resolver = Resolver::new(interner, ast);
    let (resolve_map, captures_list) = resolver.resolve(&program).map_err(|err| {
        err.iter().for_each(|e| {
            eprint_context(&lines, e.loc());
            eprintln_red!("{}", e);
        });
        LoxError::ResolveError
    })?;

    for (id, captures) in captures_list.list {
        ast.func_add_captures(&id, captures);
    }

    // for (id, captures) in captures_list.list.iter() {
    //     let StmtFunctionL { func, loc } = ast.get_func(id);
    //     let name = interner.resolve(func.name);
    //     println!("> capture list for function: {:?} at {}", name, loc);
    //     eprint_context(&lines, *loc);
    //     for (i, (name, loc)) in captures.iter().enumerate() {
    //         let name = interner.resolve(*name);
    //         println!("\n  - [{}] capture: {:?} at {}", i, name, loc);
    //         eprint_context(&lines, *loc);
    //     }
    //     println!("\n\n");
    // }

    // interpreting
    interpreter.interpret(program, resolve_map).map_err(|err| {
        eprint_context(&lines, err.loc());
        eprintln_red!("{}", err);
        LoxError::RuntimeError
    })?;

    Ok(())
}

pub fn run_file(path: PathBuf, mode: Mode) -> Result<(), LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path)?;
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

    let mut interpreter = Interpreter::new();
    run(&mut interpreter, &contents, mode)?;
    Ok(())
}

// FIXME: currently not working like a REPL
pub fn run_prompt() -> io::Result<()> {
    println!("Loxi: a Lox programming language interpreter (currently under construction)");

    let mut interpreter = Interpreter::new();
    let mut line = String::new();

    loop {
        print!(">>> ");
        stdout().flush().expect("Unable to flush stdout");

        if stdin().read_line(&mut line)? == 0 {
            break;
        }

        if let Err(err) = run(&mut interpreter, &line, Mode::Repl) {
            println!("{}", err);
        }

        line.clear();
    }

    println!("\nExiting loxi...");
    Ok(())
}

#[rustfmt::skip]
pub fn eprint_context(lines: &[&str], loc: Location) {
    let line = match loc.line > lines.len() {
        true => "",
        false => lines[loc.line - 1],
    };

    eprintln!("{:->width$}", "", width = 80);
    eprintln!("{:>4} |", "");
    eprintln!("{:>4} | {}", loc.line, line);
    eprintln!("{:>4} | \x1b[1m{:>width$}\x1b[1;31m^\x1b[00m", "", "", width = loc.column - 1);
}
