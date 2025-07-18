use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

use loxi::resolve::Resolver;

use self::compiler::Compiler;
use self::vm::Vm;

mod bytecode;
mod compiler;
mod memory;
mod metadata;
mod util;
mod value;
mod vm;

pub fn run(program: &str, mode: loxi::Mode) -> Result<(), loxi::LoxError> {
    let mut interner = loxi::interner::Interner::new();
    let mut ast = loxi::parse::ast::Ast::new();

    // lexing
    let lexer = loxi::lex::Lexer::new(program, &mut interner);
    let loxi::lex::ScanResult {
        lines,
        tokens,
        errors,
    } = lexer.scan();

    if !errors.is_empty() {
        errors.iter().for_each(|err| {
            let loc = match err {
                loxi::lex::LexError::UnknownToken(loc, _, _) => loc,
                loxi::lex::LexError::UnterminatedString(loc) => loc,
                loxi::lex::LexError::UnableToParseNumber(loc, _) => loc,
            };
            loxi::eprint_context(&lines, *loc);
            loxi::println_red!("{}", err);
        });
        return Err(loxi::LoxError::LexError(errors.len()));
    }

    if mode == loxi::Mode::DumpLex {
        for tok in tokens.iter() {
            println!("{}", tok.display(&interner));
        }
        return Ok(());
    }

    // only <eof> exist
    if tokens.len() == 1 {
        return Err(loxi::LoxError::EmptyError);
    }

    // parsing
    let mut parser = loxi::parse::Parser::new(&mut ast, loxi::parse::Mode::Script);
    let program = parser.parse(tokens).map_err(|err| {
        err.iter().for_each(|e| {
            loxi::eprint_context(&lines, e.loc());
            loxi::println_red!("{}", e);
        });
        loxi::LoxError::ParseError
    })?;

    if mode == loxi::Mode::DumpParse {
        println!("{}", program.display(&interner, &ast));
        return Ok(());
    }

    // resolving
    let mut resolver = Resolver::new(&mut interner, &ast);
    let resolve_map = resolver.resolve(&program).map_err(|err| {
        err.iter().for_each(|e| {
            loxi::eprint_context(&lines, e.loc());
            loxi::eprintln_red!("{}", e);
        });
        loxi::LoxError::ResolveError
    })?;

    // compile
    let mut compiler = Compiler::new(resolve_map, &ast, &interner);
    let Ok(bytecode) = compiler.compile(&program) else {
        loxi::eprintln_red!("{}", "failed to compile");
        return Ok(());
    };

    // dump (for debugging purpose only)
    let display = bytecode.display();
    eprintln!("bytecode:\n{display}");

    // interpret
    let mut vm = Vm::new();
    vm.interpret(&bytecode).map_err(|err| {
        loxi::eprintln_red!("{}", err);
        loxi::LoxError::RuntimeError
    })
}

pub fn run_file(path: PathBuf, mode: loxi::Mode) -> Result<(), loxi::LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path)?;
        file.read_to_string(&mut string)?;

        match string.is_empty() {
            true => return Err(loxi::LoxError::EmptyError),
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
