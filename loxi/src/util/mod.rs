use std::fmt::{Display, Formatter};

pub trait Token {}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct TokLoc<T: Token> {
    pub tok: T,
    pub loc: Location,
}

// TODO: add other information like filename and column
#[derive(Copy, Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
pub struct Location {
    pub line: usize,
    pub column: usize,
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}:{}]", self.line, self.column)
    }
}

/// ideally this function only require a single generic parameter, but the compiler is not able to
/// do const arithmetic yet, so i can only do assert here.
/// https://stackoverflow.com/a/77383522 [accessed: 2024/09/07]
pub fn to_str<'a, const M: usize, const N: usize>(
    buf: &'a mut [u8; M],
    chars: &[char; N],
) -> &'a str {
    assert!(M >= N * 4);

    let mut pos = 0;
    for ch in chars {
        pos += ch.encode_utf8(&mut buf[pos..]).len();
    }

    std::str::from_utf8(&buf[..pos]).expect("All chars is valid")
}

/// for debugging purposes
/// https://stackoverflow.com/a/78843608/16506263
#[cfg(target_arch = "x86_64")]
#[allow(unused)]
pub unsafe fn sigtrap() {
    std::arch::asm!("int3")
}
