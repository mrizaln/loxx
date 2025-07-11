use crate::interp::{function::FunctionError, value::Value, Interpreter};
use crate::util::Loc;

/// Return the current time in seconds since the Unix epoch.
pub fn clock(_: &Interpreter, _args: &[Value], _: Loc) -> Result<Value, FunctionError> {
    assert_eq!(_args.len(), 0);

    let now = std::time::SystemTime::now();
    let seconds = now
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs_f64();
    Ok(Value::number(seconds))
}

/// Collection of native functions proposed by Ben Hoyt for his attempt at implementing Lox
/// interp in Lox itself.
///
/// You can read more about it [here](https://benhoyt.com/writings/loxlox/).
#[cfg(feature = "loxlox")]
pub mod loxlox {
    use std::io::Read;

    use super::macros::arg_match;
    use super::*;

    /// Read a single character from standard input, encoded as `Value::Number`.
    pub fn getc(_: &Interpreter, _args: &[Value], _: Loc) -> Result<Value, FunctionError> {
        assert_eq!(_args.len(), 0);

        // FIXME: properly handle utf8!
        let mut buf = [0u8; 1];
        let stdin = std::io::stdin();
        match stdin.lock().read_exact(&mut buf) {
            Ok(_) => Ok(Value::number(buf[0] as f64)),
            Err(_) => Ok(Value::number(-1.0)),
        }
    }

    /// Convert ASCII code point to a string.
    pub fn chr(_: &Interpreter, args: &[Value], loc: Loc) -> Result<Value, FunctionError> {
        assert_eq!(args.len(), 1);

        // FIXME: properly handle utf8!
        let ch = arg_match!(&args[0] => Number; ["<number>", loc]);
        let buf = [*ch as u8];
        let str = std::str::from_utf8(&buf).ok();

        // NOTE: for now, if chr encounters a part of utf char, it will return nil instead
        match str {
            Some(str) => Ok(Value::string(str.to_owned())),
            None => Ok(Value::nil()),
        }
    }

    /// Exit the program with a given exit code.
    pub fn exit(_: &Interpreter, args: &[Value], loc: Loc) -> Result<Value, FunctionError> {
        assert_eq!(args.len(), 1);

        let code = arg_match!(&args[0] => Number; ["<number>", loc]);
        let code = *code as i32;

        // hmmm, is it okay to exit the program immediately like this?
        std::process::exit(code);
    }

    /// Print a string to stderr.
    pub fn print_error(
        interp: &Interpreter,
        args: &[Value],
        _: Loc,
    ) -> Result<Value, FunctionError> {
        assert_eq!(args.len(), 1);

        let (interner, ast) = interp.interner_and_ast();
        let arg = args[0].display(interner, ast);
        eprintln!("{arg}");

        Ok(Value::nil())
    }
}

mod macros {
    macro_rules! arg_match {
        ($arg:expr => $var:ident; [$name:literal, $loc:ident]) => {
            match $arg {
                Value::$var(val) => val,
                arg => Err(FunctionError::NativeArgumentError {
                    loc: $loc,
                    expect: $name,
                    got: arg.name(),
                })?,
            }
        };
    }

    pub(crate) use arg_match;
}
