use crate::source::{SourceContext, VirtualFileSystem, IsId, make_source};
use crate::diagnostics::MessageCollection;
use super::*;

fn testcase(mut scx: SourceContext<VirtualFileSystem>, spans: &[Span], symbols: &[&'static str], expect_tokens: Vec<(Token, Span)>, expect_messages: MessageCollection, line: u32) {
    let mut actual_messages = MessageCollection::new();
    let chars = scx.entry("1");
    let mut parser = Parser::<VirtualFileSystem>::new(chars, &mut actual_messages);
    for span in spans {
        parser.chars.intern_span(*span);
    }
    for symbol in symbols {
        parser.chars.intern(*symbol);
    }
    for expect_token in expect_tokens {
        assert_eq!(parser.next(), expect_token, "line {line}");
    }
    let next = parser.next();
    if next.0 != Token::EOF { panic!("next is not EOF but {:?} line {}", next, line); }

    assert_eq!(actual_messages, expect_messages, "line {line}");
}

macro_rules! case {
    ($src:literal expect [$($expect_token:expr),+$(,)?]) =>
        (testcase(make_source!($src), &[], &[], vec![$($expect_token),+], MessageCollection::new(), line!()));
    ($src:literal expect [$($expect_token:expr),+$(,)?], $expect_messages: expr) =>
        (testcase(make_source!($src), &[], &[], vec![$($expect_token),+], $expect_messages, line!()));
    ($src:literal, [$($span:expr),*] expect [$($expect_token:expr),+$(,)?]) =>
        (testcase(make_source!($src), &[$($span),*], &[], vec![$($expect_token),+], MessageCollection::new(), line!()));
    ($src:literal, [$($span:expr),*] expect [$($expect_token:expr),+$(,)?], $expect_messages: expr) => 
        (testcase(make_source!($src), &[$($span),*], &[], vec![$($expect_token),+], $expect_messages, line!()));
    ($src:literal, [$($span:expr),*], [$($string:literal),*] expect [$($expect_token:expr),+$(,)?]) =>
        (testcase(make_source!($src), &[$($span),*], &[$($string),*], vec![$($expect_token),+], MessageCollection::new(), line!()));
    ($src:literal, [$($span:expr),*], [$($string:literal),*] expect [$($expect_token:expr),+$(,)?], $expect_messages: expr) => 
        (v1234_testcase(make_source!($src), &[$($span),*], &[$($string),*], vec![$($expect_token),+], $expect_messages, line!()));
}

// make token
macro_rules! t {
    ($v:literal: bool, $start:expr, $end:expr) => ((Token::Bool($v), Span::new($start, $end)));
    (0: char, $start:expr, $end:expr) => ((Token::Char('\0'), Span::new($start, $end))); // because '\0' is hard to type
    ($v:literal: char, $start:expr, $end:expr) => ((Token::Char($v), Span::new($start, $end)));
    ($v:literal: str, $start:expr, $end:expr) => ((Token::Str(IsId::new($v), StringLiteralType::Normal), Span::new($start, $end)));
    ($v:literal: rstr, $start:expr, $end:expr) => ((Token::Str(IsId::new($v), StringLiteralType::Raw), Span::new($start, $end)));
    ($v:literal: u8, $start:expr, $end:expr) => ((Token::Num(Numeric::U8($v)), Span::new($start, $end)));
    ($v:literal: i32, $start:expr, $end:expr) => ((Token::Num(Numeric::I32($v)), Span::new($start, $end)));
    ($v:literal: u32, $start:expr, $end:expr) => ((Token::Num(Numeric::U32($v)), Span::new($start, $end)));
    ($v:literal: u64, $start:expr, $end:expr) => ((Token::Num(Numeric::U64($v)), Span::new($start, $end)));
    ($v:literal: r32, $start:expr, $end:expr) => ((Token::Num(Numeric::R32($v)), Span::new($start, $end)));
    ($v:literal: r64, $start:expr, $end:expr) => ((Token::Num(Numeric::R64($v)), Span::new($start, $end)));
    ($v:literal: ident, $start:expr, $end:expr) => ((Token::Ident(IsId::new($v)), Span::new($start, $end)));
    (Keyword::$v:ident, $start:expr, $end:expr) => ((Token::Keyword(Keyword::$v), Span::new($start, $end)));
    ($v:literal: label, $start:expr, $end:expr) => ((Token::Label(IsId::new($v)), Span::new($start, $end)));
    (Separator::$v:ident, $start:expr, $end:expr) => ((Token::Sep(Separator::$v), Span::new($start, $end)));
}

#[test]
fn line_comment() {
    // ends with lf
    case!{ "ABC//DEF\n" expect [t!(2: ident, 0, 2)] }
    // and not ends with lf
    case!{ "ABC//DEF" expect [t!(2: ident, 0, 2)] }
}

#[test]
fn block_comment() {

    case!{ "A/*D\nEF*/GH" expect [t!(2: ident, 0, 0), t!(3: ident, 9, 10)] }

    // EOF in block comment is error 
    case!{ "A/*BC" expect [t!(2: ident, 0, 0)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 1), strings::BlockCommentStartHere),
            (Span::new(5, 5), strings::EOFHere),
        ])
    ]}
}

#[test]
fn string_literal() {

    case!{ r#""Hello, world!""# expect [t!(2: str, 0, 14)] }

    case!{ r#""He"# expect [t!(1: str, 0, 3)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(3, 3), strings::EOFHere)
        ])
    ]}

    case!{ r#""He\"l\"lo"# expect [t!(1: str, 0, 10)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(10, 10), strings::EOFHere),
            (Span::new(6, 6), strings::LastEscapedQuoteHere),
        ])
    ]}

    case!{ r#""H\t\n\0\'\"llo""#, [], ["H\t\n\0'\"llo"] expect [t!(2: str, 0, 15)] }

    case!{ r#""h\c\d\e\n\g""# expect [t!(1: str, 0, 12)], make_messages![
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
            (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
            (Span::new(2, 2), strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'd'), vec![
            (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
            (Span::new(4, 4), strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'e'), vec![
            (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
            (Span::new(6, 6), strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'g'), vec![
            (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
            (Span::new(10, 10), strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]}

    case!{ r#""H\uABCDel""#, [], ["H\u{ABCD}el"] expect [t!(2: str, 0, 10)] } 
    
    //        01234567890123456
    case!{ r#""H\uABCHel\uABgC""# expect [t!(1: str, 0, 16)], make_messages![ // TODO: according to new char lit parser's policy, this should be str_t!(0, 15)
        Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
            (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
            (Span::new(7, 7), strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax,
        ]),
        Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
            (Span::new(10, 10), strings::UnicodeCharEscapeStartHere),
            (Span::new(14, 14), strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]} 
    
    //        012345678901
    case!{ r#""H\U0011ABCD""# expect [t!(1: str, 0, 12)], make_messages![ // TODO: same as before, this should be str_t!(0, 11)
        Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (Span::new(2, 11), strings::UnicodeCharEscapeHere.to_owned()),
        ], vec![
            format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}

    case!{ r#""H\u""# expect [t!(1: str, 0, 4)], make_messages![
        Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
            (Span::new(4, 4), strings::StringLiteralEndHere),
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]}

    case!{ r#""h\U123"# expect [t!(1: str, 0, 7)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(7, 7), strings::EOFHere)
        ])
    ]}

    case!{ r#""he\"# expect [t!(1: str, 0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}
}

#[test]
fn raw_string_literal() {

    case!{ r#"r"hell\u\no""#, [], [r"hell\u\no"] expect [t!(2: rstr, 0, 11)] }

    case!{ r#"R"he"# expect [t!(1: rstr, 0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}
}

#[test]
fn char_literal() {

    case!{ "'A'" expect [t!('A': char, 0, 2)] }
    case!{ r"'\t'" expect [t!('\t': char, 0, 3)] }
    case!{ r"'\uABCD'" expect [t!('\u{ABCD}': char, 0, 7)] }

    case!{ "''" expect [t!(0: char, 0, 1)], make_messages![
        Message::with_help_by_str(strings::EmptyCharLiteral, vec![
            (Span::new(0, 1), strings::CharLiteralHere), 
        ], vec![
            strings::CharLiteralSyntaxHelp1
        ])
    ]}

    case!{ "'ABC'" expect [t!(0: char, 0, 4)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 4), strings::CharLiteralHere),
        ])
    ]}

    case!{ r"'\c'" expect [t!(0: char, 0, 3)], make_messages![
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
            (Span::new(0, 0), strings::CharLiteralStartHere.to_owned()), 
            (Span::new(1, 1), strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]} 
    //       012345
    case!{ r"'\uBG'" expect [t!(0: char, 0, 5)], make_messages![
        Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
            (Span::new(1, 1), strings::UnicodeCharEscapeStartHere), 
            (Span::new(4, 4), strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax,
        ]),
        Message::with_help_by_str(strings::UnexpectedCharLiteralEnd, vec![
            (Span::new(0, 5), strings::CharLiteralHere),
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax
        ])
    ]}

    case!{ r"'\U0011ABCD'" expect [t!(0: char, 0, 11)], make_messages![
        Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (Span::new(1, 10), strings::UnicodeCharEscapeHere.to_owned()), 
        ], vec![
            format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}

    case!{ r"'\na'" expect [t!(0: char, 0, 4)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 4), strings::CharLiteralHere),   
        ])
    ]}

    case!{ r"'\uABCDA'" expect [t!(0: char, 0, 8)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 8), strings::CharLiteralHere),      
        ])
    ]}

    case!{ "'" expect [t!(0: char, 0, 0)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::CharLiteralHere),    
            (Span::new(1, 1), strings::EOFHere)
        ])
    ]}

    case!{ r"'\" expect [t!(0: char, 0, 1)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 1), strings::CharLiteralHere),      
            (Span::new(2, 2), strings::EOFHere)
        ])
    ]}

    case!{ r"'\u" expect [t!(0: char, 0, 2)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 2), strings::CharLiteralHere),
            (Span::new(3, 3), strings::EOFHere)
        ])
    ]}

    case! { r"'A" expect [t!(0: char, 0, 1)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 1), strings::CharLiteralHere),
            (Span::new(2, 2), strings::EOFHere)
        ])
    ]}

    case! { "'ABC" expect [t!(0: char, 0, 3)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 3), strings::CharLiteralHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}

    case!{ r"'\'AB" expect [t!(0: char, 0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 4), strings::CharLiteralHere),
            (Span::new(5, 5), strings::EOFHere),
        ])
    ]}
}

// simple ident/label test is too simple, numeric test in numeric, so put original v2 tests still in one function
#[test]
fn v2() {

    // keyword, identifier, bool lit, num lit, separator
    //      0         1          2         3   
    //      01234567890123 456789012345678901234 5678
    case!{ "var a = true;\nvar b = 789_123.456;\ndefg", [Span::new(4, 4), Span::new(18, 18), Span::new(35, 38)] expect [      
        t!(Keyword::Var, 0, 2),
        t!(2: ident, 4, 4),
        t!(Separator::Eq, 6, 6),
        t!(true: bool, 8, 11),
        t!(Separator::SemiColon, 12, 12),
        t!(Keyword::Var, 14, 16),
        t!(3: ident, 18, 18),
        t!(Separator::Eq, 20, 20),
        t!(789123.4560000001: r64, 22, 32),
        t!(Separator::SemiColon, 33, 33),
        t!(4: ident, 35, 38),
    ]}

    //           0       1      2       3
    //           0 3 67890123 690123 4 9012
    case!{ "一个chinese变量, a_中文_var", [Span::new(0, 16), Span::new(21, 32)] expect [  // chinese ident
        t!(2: ident, 0, 16),
        t!(Separator::Comma, 19, 19),
        t!(3: ident, 21, 32),
    ]}

    // different postfix\types of num lit, different types of sep
    //      0         1         2         3         4         5         6         7
    //      0123456789012345678901234567890123456789012345678901234567890123456789012345
    case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123r32 += 123.0 / 123u8 && 1024u8]", [] expect [  
        t!(Separator::LeftBracket, 0, 0),
        t!(1: i32, 1, 1),
        t!(Separator::Comma, 2, 2),
        t!(123: i32, 4, 6),
        t!(Keyword::Underscore, 8, 8),
        t!(1: u64, 10, 13),
        t!(Separator::LeftParen, 14, 14),
        t!(123.456: r64, 16, 22),
        t!(Separator::Comma, 23, 23),
        t!(Separator::RightParen, 24, 24),
        t!(Separator::Sub, 26, 26),
        t!(123456: i32, 27, 33),
        t!(Separator::LeftBrace, 34, 34),
        t!(123: u32, 35, 40),
        t!(Separator::RightBrace, 41, 41),
        t!(123.0: r32, 42, 47),
        t!(Separator::AddEq, 49, 50),
        t!(123.0: r64, 52, 56),
        t!(Separator::Div, 58, 58),
        t!(123: u8, 60, 64),
        t!(Separator::AndAnd, 66, 67),
        t!(0: i32, 69, 74),
        t!(Separator::RightBracket, 75, 75),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::IntegralOverflow),
            vec![(Span::new(69, 74), String::new())],
            vec![strings::IntegralOverflowHelpMaxValue[1].to_owned()]
        ),
    ]}

    // differnt prefix\base of num lit
    //           0         1         2         3         4         5         6         7         8
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", [] expect [
        t!(Separator::LeftBracket, 0, 0),
        t!(123: i32, 1, 3),
        t!(Separator::Mul, 5, 5),
        t!(0x123: i32, 7, 11),
        t!(Separator::Sub, 13, 13),
        t!(0xAFF: i32, 15, 19),
        t!(Separator::And, 21, 21),
        t!(0o777: i32, 23, 27),
        t!(Separator::OrOr, 29, 30),
        t!(0: i32, 32, 36),
        t!(Separator::NotEq, 38, 39),
        t!(0b101010: i32, 41, 48),
        t!(Separator::EqEq, 50, 51),
        t!(0: i32, 53, 60),
        t!(Separator::Arrow, 62, 63),
        t!(123: i32, 65, 69),
        t!(Separator::DotDot, 70, 71),
        t!(0: i32, 73, 77),
        t!(Separator::RightBracket, 78, 78),
        t!(Separator::DotDot, 80, 81),
        t!(Separator::SubEq, 83, 84),
    ], make_messages![
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(32, 36), String::new())],
            vec![strings::IntLiteralAllowedChars[1].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(53, 60), String::new())],
            vec![strings::IntLiteralAllowedChars[0].to_owned()]
        ),
        Message::with_help(
            format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
            vec![(Span::new(73, 77), String::new())],
            vec![strings::IntLiteralAllowedChars[2].to_owned()]
        ),
    ]}

    //           0       1        2
    //           012345 8901234578 123
    case!{ "[1, 2，3.5, 4。5】<<=", [] expect [  // not ascii char hint and recover
        t!(Separator::LeftBracket, 0, 0),
        t!(1: i32, 1, 1),
        t!(Separator::Comma, 2, 2),
        t!(2: i32, 4, 4),
        t!(Separator::Comma, 5, 5),
        t!(3.5: r64, 8, 10),
        t!(Separator::Comma, 11, 11),
        t!(4.5: r64, 13, 17),
        t!(Separator::RightBracket, 18, 18),
        t!(Separator::LtLtEq, 21, 23),
    ], make_messages![
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(5, 5), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ',', "COMMA", '，', "FULLWIDTH COMMA"),
        ]),
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(14, 14), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "FULL STOP", '。', "IDEOGRAPHIC FULL STOP"),
        ]),
        Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
            (Span::new(18, 18), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ']', "RIGHT SQUARE BRACKET", '】', "RIGHT BLACK LENTICULAR BRACKET"),
        ]),
    ]}

    //           012345678
    case!{ "1..2.0r32", [] expect [  // range operator special case
        t!(1: i32, 0, 0),
        t!(Separator::DotDot, 1, 2),
        t!(2.0: r32, 3, 8),
    ]}

    //           01234
    case!{ "2...3", [] expect [  // range operator special case 2
        t!(2: i32, 0, 0),
        t!(Separator::DotDot, 1, 2),
        t!(Separator::Dot, 3, 3),
        t!(3: i32, 4, 4),
    ]}

    //           0         1         2         
    //           012345678901234567890123456789
    case!{ "1.is_odd(), 123r64.to_string()", [Span::new(2, 7), Span::new(19, 27)] expect [  //  another special case
        t!(1: i32, 0, 0),
        t!(Separator::Dot, 1, 1),
        t!(2: ident, 2, 7),
        t!(Separator::LeftParen, 8, 8),
        t!(Separator::RightParen, 9, 9),
        t!(Separator::Comma, 10, 10),
        t!(123.0: r64, 12, 17),
        t!(Separator::Dot, 18, 18),
        t!(3: ident, 19, 27),
        t!(Separator::LeftParen, 28, 28),
        t!(Separator::RightParen, 29, 29),
    ]}

    //           0           1          2
    //           01 234567 890 1234567890123456
    case!{ "r\"hello\" '\\u1234' 12/**/34 ", [], ["hello"] expect [    // dispatch v1
        t!(2: rstr, 0, 7),
        t!('\u{1234}': char, 9, 16),
        t!(12: i32, 18, 19),
        t!(34: i32, 24, 25),
    ]}

    // bug from syntax::expr::postfix_expr
    //           0         1         2         3         4         5
    //           012345678901234567890123456789012345678901234567890123
    case!{ "1.a[[3](4, [5, 6], )](7, 8)() and[i32].bcd[10, 11, 12]", [Span::new(2, 2), Span::new(39, 41)] expect [
        t!(1: i32, 0, 0),
        t!(Separator::Dot, 1, 1),
        t!(2: ident, 2, 2),
        t!(Separator::LeftBracket, 3, 3),
        t!(Separator::LeftBracket, 4, 4),
        t!(3: i32, 5, 5),
        t!(Separator::RightBracket, 6, 6),
        t!(Separator::LeftParen, 7, 7),
        t!(4: i32, 8, 8),
        t!(Separator::Comma, 9, 9),
        t!(Separator::LeftBracket, 11, 11),
        t!(5: i32, 12, 12),
        t!(Separator::Comma, 13, 13),
        t!(6: i32, 15, 15),
        t!(Separator::RightBracket, 16, 16),
        t!(Separator::Comma, 17, 17),
        t!(Separator::RightParen, 19, 19),
        t!(Separator::RightBracket, 20, 20),
        t!(Separator::LeftParen, 21, 21),
        t!(7: i32, 22, 22),
        t!(Separator::Comma, 23, 23),
        t!(8: i32, 25, 25),
        t!(Separator::RightParen, 26, 26),
        t!(Separator::LeftParen, 27, 27),
        t!(Separator::RightParen, 28, 28),
        t!(Keyword::And, 30, 32),
        t!(Separator::LeftBracket, 33, 33),
        t!(Keyword::I32, 34, 36),
        t!(Separator::RightBracket, 37, 37),
        t!(Separator::Dot, 38, 38),
        t!(3: ident, 39, 41),
        t!(Separator::LeftBracket, 42, 42),
        t!(10: i32, 43, 44),
        t!(Separator::Comma, 45, 45),
        t!(11: i32, 47, 48),
        t!(Separator::Comma, 49, 49),
        t!(12: i32, 51, 52),
        t!(Separator::RightBracket, 53, 53),
    ], make_messages![
        Message::new(format!("{}: {:?}", strings::UseReservedKeyword, Keyword::And), vec![(Span::new(30, 32), String::new())]),
    ]}

    case!{ "@", [] expect [
        t!(1: label, 0, 0),
    ]}

    //      0         1     
    //      0123456789012345678
    case!{ "abc @abc @ @@ 1 @a", [Span::new(0, 2), Span::new(12, 12), Span::new(17, 17)], [""] expect [
        t!(2: ident, 0, 2),  // yeah
        t!(2: label, 4, 7),  // yeah
        t!(1: label, 9, 9),
        t!(3: label, 11, 12),
        t!(1: i32, 14, 14),
        t!(4: label, 16, 17),
    ]}

    case!{ "a:", [Span::new(0, 0)] expect [
        t!(2: ident, 0, 0),
        t!(Separator::Colon, 1, 1),
    ]}
    case!{ "@: {}", [] expect [
        t!(1: label, 0, 0),
        t!(Separator::Colon, 1, 1),
        t!(Separator::LeftBrace, 3, 3),
        t!(Separator::RightBrace, 4, 4),
    ]}
}

#[test]
fn v4() { // in memory for old v4lexer
    case! { "123 abc 'd', [1]" expect [
        t!(123: i32, 0, 2),
        t!(2: ident, 4, 6),
        t!('d': char, 8, 10),
        t!(Separator::Comma, 11, 11),
        t!(Separator::LeftBracket, 13, 13),
        t!(1: i32, 14, 14),
        t!(Separator::RightBracket, 15, 15),
    ]}
}
