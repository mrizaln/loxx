/// ideally I need a single generic parameter, but the compiler is not able to do const arithmetic.
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
