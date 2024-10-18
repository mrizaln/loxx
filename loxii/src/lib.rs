use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

mod util;

pub fn run(program: &str, mode: loxi::RunMode) -> Result<(), loxi::LoxError> {
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
            loxi::print_context(&lines, *loc);
            loxi::println_red!("{}", err);
        });
        return Err(loxi::LoxError::LexError(errors.len()));
    }

    if mode == loxi::RunMode::DumpLex {
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
    let mut parser = loxi::parse::Parser::new(&mut ast);
    let program = parser.parse(tokens).map_err(|err| {
        err.iter().for_each(|e| {
            loxi::print_context(&lines, e.loc());
            loxi::println_red!("{}", e);
        });
        loxi::LoxError::ParseError
    })?;

    if mode == loxi::RunMode::DumpParse {
        println!("{}", program.display(&interner, &ast));
        return Ok(());
    }

    // compile

    // interpret

    todo!("The rest of the interpreter is not implemented yet");
}

pub fn run_file(path: PathBuf, mode: loxi::RunMode) -> Result<(), loxi::LoxError> {
    let contents = {
        let mut string = String::new();
        let mut file = File::open(path.clone())?;
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
