use thiserror::Error;

use crate::util::Location;

use super::expr::Expr;

#[derive(Debug, Error)]
pub enum SyntaxError {
    #[error("{loc} SyntaxError: Expect '{expect}', got '{real}'")]
    Expect {
        expect: &'static str,
        real: &'static str,
        loc: Location,
    },

    #[error("{start} SyntaxError: Missing closing delimiter '{delim}'")]
    MissingDelim {
        delim: &'static str,
        start: Location,
    },

    #[error("{loc} SyntaxError: Number of arguments exceed language limit ({num} exceed {limit})")]
    TooManyArguments {
        num: usize,
        limit: usize,
        loc: Location,
    },
}

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(SyntaxError),
    EndOfFile(Location),
}

pub trait ParseResultExt<T> {
    fn map_syntax_err(self, expect: &'static str) -> Result<T, SyntaxError>;
}

impl SyntaxError {
    pub fn expect(expect: &'static str, real: &'static str, loc: Location) -> SyntaxError {
        SyntaxError::Expect { expect, real, loc }
    }

    pub fn missing_delim(delim: &'static str, start: Location) -> SyntaxError {
        SyntaxError::MissingDelim { delim, start }
    }

    pub fn too_many_args(num: usize, loc: Location) -> SyntaxError {
        SyntaxError::TooManyArguments {
            num,
            limit: Expr::MAX_FUNC_ARGS,
            loc,
        }
    }

    pub fn loc(&self) -> Location {
        match self {
            SyntaxError::Expect { loc, .. } => *loc,
            SyntaxError::MissingDelim { start, .. } => *start,
            SyntaxError::TooManyArguments { loc, .. } => *loc,
        }
    }

    pub fn as_parse_err(self) -> ParseError {
        ParseError::SyntaxError(self)
    }
}

impl ParseError {
    pub fn too_many_args(num: usize, loc: Location) -> ParseError {
        ParseError::SyntaxError(SyntaxError::TooManyArguments {
            num,
            limit: Expr::MAX_FUNC_ARGS,
            loc,
        })
    }

    /// Convert the variant to ParseError::SyntaxError(SyntaxError::Expect)
    pub fn as_syntax_err(self, expect: &'static str) -> SyntaxError {
        match self {
            ParseError::EndOfFile(loc) => SyntaxError::expect(expect, "<eof>", loc),
            ParseError::SyntaxError(err) => err,
        }
    }

    /// convert the variant to ParseError::SyntaxError(SyntaxError::MissingDelim)
    pub fn missing_delim(self, delim: &'static str, start: Location) -> Self {
        match self {
            ParseError::EndOfFile(_) => {
                ParseError::SyntaxError(SyntaxError::MissingDelim { start, delim })
            }
            _ => self,
        }
    }
}

impl From<SyntaxError> for ParseError {
    fn from(err: SyntaxError) -> Self {
        ParseError::SyntaxError(err)
    }
}

impl<T> ParseResultExt<T> for Result<T, ParseError> {
    fn map_syntax_err(self, expect: &'static str) -> Result<T, SyntaxError> {
        self.map_err(|err| match err {
            ParseError::SyntaxError(err) => err,
            ParseError::EndOfFile(loc) => SyntaxError::expect(expect, "<eof>", loc),
        })
    }
}
