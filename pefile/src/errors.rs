///! fff-lang
///! 
///! pefile/errors, simple error type

#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Debug)]
pub enum Error {
    ByteArrayTooShort,
    InvalidDOSHeaderMagic,
}