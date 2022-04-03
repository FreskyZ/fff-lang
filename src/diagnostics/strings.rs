#![allow(non_upper_case_globals)]
#![allow(dead_code)]
///! all kinds of error strings

pub const Empty: &str = "";

pub const FailedToReadFile: &str = "failed to read file";
pub const FailedToReadAllCandidates: &str = "failed to read all candidates for module request";
pub const OriginatedHere: &str = "originated from here";
pub const Candidates: &str = "candidates:";

pub const UnexpectedEOF: &str = "Unexpected EOF";
pub const UnexpectedEOL: &str = "Unexpected end of line";
pub const BlockCommentStartHere: &str = "Block comment starts here";
pub const EOFHere: &str = "EOF here";
pub const EOLHere: &str = "EOL here";
pub const UnknownCharactor: &str = "Unknown charactor";
pub const UnexpectedCharLiteralEnd: &str = "Unexpected char literal end";
pub const CharLiteralStartHere: &str = "Char literal start here";
pub const CharLiteralEndHere: &str = "Char literal end here";
pub const UnicodeCharEscapeHelpSyntax: &str = "Unicode char escape is like \\uxxxx or \\Uxxxxxxxx";
pub const UnknownCharEscape: &str = "Unknown char escape";
pub const UnknownCharEscapeHere: &str = "Unknown char escape here";
pub const CharLiteralTooLong: &str = "Char literal too long";
pub const StringLiteralSyntaxHelp: &str = "String literal should be surround with double quotation";
pub const EmptyCharLiteral: &str = "Empty char literal";
pub const CharLiteralSyntaxHelp1: &str = "Char literal should contain exactly one code point";
pub const InvalidUnicodeCharEscape: &str = "Invalid unicode char escape";
pub const UnicodeCharEscapeCodePointValueIs: &str = "The code point value is 0x";
pub const UnicodeCharEscapeStartHere: &str = "Unicode escape start here";
pub const UnicodeCharEscapeInvalidChar: &str = "Invalid hex char here";
pub const UnicodeCharEscapeHelpValue: &str = "Not a valid unicode code point";
pub const StringLiteralStartHere: &str = "String literal start here";
pub const UnexpectedStringLiteralEnd: &str = "Unexpected string literal end";
pub const StringLiteralEndHere: &str = "String literal end here";
pub const LastEscapedQuoteHere: &str = "Last escaped quote here";
pub const CharLiteralHere: &str = "Char literal here";
pub const UnicodeCharEscapeHere: &str = "Unicode char escape here";

// keep these 'static to indicate they are written at that age
pub const UnexpectedNonASCIIChar: &'static str = "Unexpected non ASCII char";
pub const InvalidNumericLiteral: &'static str = "Invalid numeric literal";
pub const NumLitShouldNotStartWith0: &'static str = "numeric literal should not start with 0";
pub const CStyleOctNumLitHelp: &'static str = "If you mean C style octal numeric literal, use `0o777` syntax";
pub const IntegralPrefixIsLowerCase: &'static str = "Integral prefix `0b`, `0o`, `0d`, `0x` is lower case";
pub const UnexpectedEOFInMaybeUnsignedIntPostfix: &'static str = "unexpected EOF in maybe unsigned integral postfix";
pub const UnexpectedEOFInMaybeSignedIntPostfix: &'static str = "unexpected EOF in maybe signed integral postfix";
pub const UnexpectedEOFInMaybeFloatingPostfix: &'static str = "unexpected EOF in maybe floating point postfix";
pub const UnexpectedEOFInExponent: &'static str = "unexpected EOF in floating point Exponent";
pub const UnexpectedValueAfterMaybeUnsignedIntPostfix: &'static str = "unexpected value after maybe unsigned integral postfix";
pub const UnexpectedValueAfterMaybeSignedIntPostfix: &'static str = "unexpected value after maybe signed integral postfix";
pub const UnexpectedValueAfterMaybeFloatingPostfix: &'static str = "unexpected value after maybe floating point postfix";
pub const UnexpectedNotEOF: &'static str = "unexpected not EOF";
pub const FloatPointUnderflow: &'static str = "floating point underflow";
pub const FloatPointOverflow: &'static str = "floating point overflow";
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
pub const InvalidCharInFloatLiteral: &'static str = "invalid char in floating point literal";
pub const InvalidCharInIntLiteral: &'static str = "invalid char in integral literal";
pub const InvalidChar: &'static str = "invalid char";
pub const IntLiteralAllowedChars: [&'static str; 4] = [
    "It is a binary literal and only allows 0 and 1",
    "It is an octal literal and only allows 0-7",
    "It is a decimal literal and only allows 0-9",
    "It is a hexadecimal liteteral and only allows 0-9a-fA-F"
];
pub const InternalErrorAt: &'static str = "internal error at ";
pub const ExponentInIntLiteral: &'static str = "Exponent not allowed in integral literal";
pub const EmptyIntLiteral: &'static str = "empty integral literal";
pub const EmptyLiteral: &'static str = "empty literal";
pub const AndFloatPostfixInIntLiteral: &'static str = "And floating point literal not allowed in integral literal";
pub const DotInIntLiteral: &'static str = "decimal dot not allowed in integral literal";
pub const FloatExponentFloat: &'static str = "floating point exponentail should be integer";
pub const FloatPointOverflowHelpMaxValue: [&'static str; 4] = [
    "It is a single precision floating point literal and positive max value is about 3.40282347E+38",
    "It is a single precision floating point literal and negative max value is about -3.40282347E+38",
    "It is a double precision floating point literal and positive max value is about 1.7976931348623157E+308", // 1.79E308?
    "It is a double precision floating point literal and negative max value is about -1.7976931348623157E+308", // -1.79E308
];
pub const FloatPointUnderflowHelpMinValue: [&'static str; 4] = [
    "It is a single precision floating point literal and positive min value is about 1.17549435E-38",
    "It is a single precision floating point literal and negative min value is about -1.17549435E-38",
    "It is a double precision floating point literal and positive min value is about 2.2250738585072014E-308", // 1.79E-308?
    "It is a double precision floating point literal and negative min value is about -2.2250738585072014E-308", // -1.79E-308?
];
pub const NegativeOperatorOnUnsignedInt: &'static str = "Negative operator should not apply to unsigned integral literal";
pub const UnderscoreDouble: &'static str = "continuous underscore not allowed";
pub const UnderscoreArroundDot: &'static str = "underscore should not before or after decimal dot";
pub const UnderscoreAtHead: &'static str = "underscore should not be before first digit of numeric literal";
pub const UnderscoreAtExponentHead: &'static str = "underscore should not be before first digit of floating point literal exponent";
pub const UnderscoreAtEnd: &'static str = "underscore should not be end of numeric literal";
pub const UnderscoreInMaybeSignedIntPostfix: &'static str = "underscore should not be within maybe signed integral postfix";
pub const UnderscoreInMaybeUnsignedIntPostfix: &'static str = "underscore should not be within maybe unsigned integral postfix";
pub const UnderscoreInMaybeFloatPointPostfix: &'static str = "underscore should not be within maybe floating point integral postfix";
pub const DotAtHead: &'static str = "decimal dot should not be before first digit of numeric literal";
pub const DotAtEnd: &'static str = "decimal dot should not be after last digit of numeric literal";
pub const DotDouble: &'static str = "multiple decimal dot";
pub const MaybeIntPostfixInFloatPoint: &'static str = "maybe integral postfix not allowed in floating point literal";
pub const UseReservedKeyword: &str = "Use of reserved keyword";

pub const UnexpectedSingleComma: &str = "unexpected single comma";
pub const ArrayDefHere: &str = "array definition here";
pub const TupleDefHere: &str = "tuple definition here";
pub const FnCallHere: &str = "function call here";
pub const EmptyIndexCall: &str = "empty indexer call";
pub const IndexCallHere: &str = "indexer call here";
pub const InvalidArrayType: &str = "invalid array type";
pub const ArrayTypeSyntaxHelp: &str = "array type syntax is [type; size]";
pub const SingleItemTupleType: &str = "single item tuple type should ends with comma";
pub const TupleTypeExpectCommaMeetRightParen: &str = "expected comma, meet right paren";
pub const FunctionReturnTypeShouldUseArrow: &str = "function return type should use arrow `->`";
pub const FunctionReturnTypeExpectArrowMeetColon: &str = "expected arrow `->`, meet colon";
pub const InvalidTupleIndex: &str = "invalid tuple index";
pub const TupleIndexSyntaxHelp: &str = "tuple index can only use simple numeric value";
pub const InvalidMemberAccess: &str = "invalid member access";
pub const GenericMemberAccessSyntaxHelp: &str = "generic parameter should be inside angle bracket";
pub const InvalidNameSegment: &str = "invalid name segment";
pub const NameSegmentExpect: &str = "expect identifier, meet less-than `<`";
pub const MaybeGeneric: &str = "relational operators cannot be chained, did you mean generic parameters?";
pub const MaybeGenericHelp: &str = "use `::<...>` instead of `<...>` in expressions";
