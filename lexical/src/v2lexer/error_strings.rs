#![allow(non_upper_case_globals)]
#![allow(dead_code)]

///! fff-lang
///!
///! some error strings

pub const UnexpectedNonASCIIChar: &'static str = "Unexpected non ASCII char";
pub const InvalidNumericLiteral: &'static str = "Invalid numeric literal";
pub const NumLitShouldNotStartWith0: &'static str = "numeric literal should not start with 0";
pub const CStyleOctNumLitHelp: &'static str = "If you mean C style octal numeric literal, use `0o777` syntax";
pub const IntegralPrefixIsLowerCase: &'static str = "Integral prefix `0b`, `0o`, `0d`, `0x` is lower case";
pub const UnexpectedEOFInMaybeUnsignedIntPostfix: &'static str = "unexpected EOF in maybe unsigned integral postfix";
pub const UnexpectedEOFInMaybeSignedIntPostfix: &'static str = "unexpected EOF in maybe signed integral postfix";
pub const UnexpectedEOFInMaybeFloatingPostfix: &'static str = "unexpected EOF in maybe floating point postfix";
pub const UnexpectedValueAfterMaybeUnsignedIntPostfix: &'static str = "unexpected value after maybe unsigned integral postfix";
pub const UnexpectedValueAfterMaybeSignedIntPostfix: &'static str = "unexpected value after maybe signed integral postfix";
pub const UnexpectedValueAfterMaybeFloatingPostfix: &'static str = "unexpected value after maybe floating point postfix";
pub const UnexpectedNotEOF: &'static str = "unexpected not EOF";
pub const FloatingPointUnderflow: &'static str = "floating point underflow";
pub const FloatingPointOverflow: &'static str = "floating point overflow";
pub const IntegralOverflow: &'static str = "integral overflow";
pub const IntegralUnderflow: &'static str = "integral underflow";
pub const IntegralOverflowHelpMaxValue: [&'static str; 8] = [
    "Max value of i8 is 127",
    "Max value of u8 is 255",
    "Max value of i16 is 32767",
    "Max value of u16 is 65535",
    "Max value of i32 is 2147483647",
    "Max value of u32 is 4294967293",
    "Max value of i32 is 9223372036854775807",
    "Max value of u64 is 18446744073709551615",
];
pub const IntegralUnderflowHelpMinValue: [&'static str; 4] = [
    "Max value of i8 is -128",
    "Max value of i16 is -32768",
    "Max value of i32 is -2147483648",
    "Max value of i32 is -9223372036854775808",
];
pub const InvalidCharInIntLiteral: &'static str = "invalid char in integral literal";
pub const InvalidCharInFloatLiteral: &'static str = "invalid char in floating point literal";