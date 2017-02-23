// error strings

#![allow(non_upper_case_globals)]

macro_rules! define_string {
    ($name: ident, $value: expr) => (pub const $name: &'static str = $value;)
}

define_string!{ UnexpectedEOF, "Unexpected EOF" }
define_string!{ BlockCommentStartHere, "Block comment starts here" }
define_string!{ EOFHere, "EOF here" }
define_string!{ UnexpectedCharLiteralEnd, "Unexpected char literal end" }
define_string!{ CharLiteralStartHere, "Char literal start here" }
define_string!{ CharLiteralEndHere, "Char literal end here" }
define_string!{ UnicodeCharEscapeHelpSyntax, "Unicode char escape is like \\uxxxx or \\Uxxxxxxxx" }
define_string!{ UnknownCharEscape, "Unknown char escape" }
define_string!{ UnknownCharEscapeHere, "Unknown char escape here" }
define_string!{ CharLiteralTooLong, "Char literal too long" }
define_string!{ StringLiteralSyntaxHelp, "String literal should be surround with double quotation" }
define_string!{ EmptyCharLiteral, "Empty char literal" }
define_string!{ CharLiteralSyntaxHelp1, "Char literal should contain exactly one code point" }
define_string!{ SingleQuoteOrBackSlashCharLiteralMaybeHelp, "did you mean `'\\''` or `'\\\\'`?" }
define_string!{ InvalidUnicodeCharEscape, "Invalid unicode char escape" }
define_string!{ UnicodeCharEscapeCodePointValueIs, "The code point value is 0x" }
define_string!{ UnicodeCharEscapeStartHere, "Unicode escape start here" }
define_string!{ UnicodeCharEscapeInvalidChar, "Invalid hex char here" }
define_string!{ UnicodeCharEscapeHelpValue, "Currently (at this program build time) max unicode code point is 0x10FFFF and much area is not used" }