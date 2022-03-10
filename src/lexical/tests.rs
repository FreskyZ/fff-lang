use crate::source::{SourceContext, VirtualFileSystem, IsId, make_source};
use crate::diagnostics::MessageCollection;
use super::*;

// this is how you name a function when v1, v2, v3, v4 all disappears
fn v1234_testcase(mut scx: SourceContext<VirtualFileSystem>, spans: &[Span], symbols: &[&'static str], expect_tokens: Vec<(Token, Span)>, expect_messages: MessageCollection) {
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
        assert_eq!(parser.next(), expect_token);
    }
    let next = parser.next();
    if next.0 != Token::EOF { panic!("next is not EOF but {:?}", next); }

    assert_eq!(actual_messages, expect_messages);
}

macro_rules! case {
    ($src:literal expect [$($expect_token:expr),+$(,)?]) =>
        (v1234_testcase(make_source!($src), &[], &[], vec![$($expect_token),+], MessageCollection::new()));
    ($src:literal, [$($span:expr),*] expect [$($expect_token:expr),+$(,)?]) =>
        (v1234_testcase(make_source!($src), &[$($span),*], &[], vec![$($expect_token),+], MessageCollection::new()));
    ($src:literal, [$($span:expr),*] expect [$($expect_token:expr),+$(,)?], $expect_messages: expr) => 
        (v1234_testcase(make_source!($src), &[$($span),*], &[], vec![$($expect_token),+], $expect_messages));
    ($src:literal, [$($span:expr),*], [$($string:literal),*] expect [$($expect_token:expr),+$(,)?]) =>
        (v1234_testcase(make_source!($src), &[$($span),*], &[$($string),*], vec![$($expect_token),+], MessageCollection::new()));
    ($src:literal, [$($span:expr),*], [$($string:literal),*] expect [$($expect_token:expr),+$(,)?], $expect_messages: expr) => 
        (v1234_testcase(make_source!($src), &[$($span),*], &[$($string),*], vec![$($expect_token),+], $expect_messages));
}

// make token
macro_rules! t {
    ($v:literal: bool, $start:expr, $end:expr) => ((Token::Bool($v), Span::new($start, $end)));
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
    ($v:literal: keyword, $start:expr, $end:expr) => ((Token::Keyword($v), Span::new($start, $end)));
    ($v:literal: label, $start:expr, $end:expr) => ((Token::Label(IsId::new($v)), Span::new($start, $end)));
    ($v:literal: sep, $start:expr, $end:expr) => ((Token::Sep($sep), Span::new($start, $end)));
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
    
}

// #[test]
// fn v1_v2_v3_v4_base() { 


//     // Block comment is ' ', but has correct position
//     test_case!{ "A/*D\nEF*/GH", vec![n!('A', 0), n!(' ', 1, 8), n!('G', 9), n!('H', 10)] }
//     // EOF in block comment is error 
//     test_case!{ with message, "A/*BC", vec![n!('A', 0)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(1, 1), strings::BlockCommentStartHere),
//             (Span::new(5, 5), strings::EOFHere),
//         ])
//     ]}

//     // String literal test cases
//     test_case!{ with symbol, r#""Hello, world!""#, 
//         "Hello, world!", vec![str_lit!(IsId::new(2), 0, 14)] 
//     }
//     test_case!{ with message, r#""He"#, vec![str_lit!(0, 3)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(3, 3), strings::EOFHere)
//         ])
//     ]}
//     test_case!{ with message, r#""He\"l\"lo"#, vec![str_lit!(0, 10)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(10, 10), strings::EOFHere),
//             (Span::new(6, 6), strings::LastEscapedQuoteHere),
//         ])
//     ]}
//     test_case!{ with symbol, r#""H\t\n\0\'\"llo""#,
//         "H\t\n\0'\"llo", vec![str_lit!(IsId::new(2), 0, 15)]
//     }
//     test_case!{ with message, r#""h\c\d\e\n\g""#, vec![str_lit!(0, 12)], make_messages![
//         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
//             (Span::new(2, 2), strings::UnknownCharEscapeHere.to_owned()),
//         ]),
//         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'd'), vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
//             (Span::new(4, 4), strings::UnknownCharEscapeHere.to_owned()),
//         ]),
//         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'e'), vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
//             (Span::new(6, 6), strings::UnknownCharEscapeHere.to_owned()),
//         ]),
//         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'g'), vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere.to_owned()),
//             (Span::new(10, 10), strings::UnknownCharEscapeHere.to_owned()),
//         ])
//     ]}
//     test_case!{ with symbol, r#""H\uABCDel""#, 
//         "H\u{ABCD}el", vec![str_lit!(IsId::new(2), 0, 10)]
//     } //                          0123456789012345
//     test_case!{ with message, r#""H\uABCHel\uABgC""#, vec![str_lit!(0, 16)], make_messages![ // TODO: according to new char lit parser's policy, this should be str_lit!(0, 15)
//         Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
//             (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
//             (Span::new(7, 7), strings::UnicodeCharEscapeInvalidChar)
//         ], vec![
//             strings::UnicodeCharEscapeHelpSyntax,
//         ]),
//         Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
//             (Span::new(10, 10), strings::UnicodeCharEscapeStartHere),
//             (Span::new(14, 14), strings::UnicodeCharEscapeInvalidChar)
//         ], vec![
//             strings::UnicodeCharEscapeHelpSyntax,
//         ])
//     ]} //                         012345678901
//     test_case!{ with message, r#""H\U0011ABCD""#, vec![str_lit!(0, 12)], make_messages![ // TODO: same as before, this should be str_lit!(0, 11)
//         Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
//             (Span::new(2, 11), strings::UnicodeCharEscapeHere.to_owned()),
//         ], vec![
//             format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
//             strings::UnicodeCharEscapeHelpValue.to_owned(),
//         ])
//     ]}
//     test_case!{ with message, r#""H\u""#, vec![str_lit!(0, 4)], make_messages![
//         Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
//             (Span::new(4, 4), strings::StringLiteralEndHere),
//         ], vec![
//             strings::UnicodeCharEscapeHelpSyntax,
//         ])
//     ]}
//     test_case!{ with message, r#""h\U123"#, vec![str_lit!(0, 7)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(7, 7), strings::EOFHere)
//         ])
//     ]}
//     test_case!{ with message, r#""he\"#, vec![str_lit!(0, 4)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(4, 4), strings::EOFHere)
//         ])
//     ]}

//     // Raw string literal test cases
//     test_case!{ with symbol, r#"r"hell\u\no""#, 
//         r"hell\u\no", vec![rstr_lit!(IsId::new(2), 0, 11)]
//     }
//     test_case!{ with message, r#"R"he"#, vec![rstr_lit!(0, 4)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::StringLiteralStartHere),
//             (Span::new(4, 4), strings::EOFHere)
//         ])
//     ]}

//     // Char literal test cases
//     test_case!{ "'A'", vec![c!('A', 0, 2)] }
//     test_case!{ r"'\t'", vec![c!('\t', 0, 3)] }
//     test_case!{ r"'\uABCD'", vec![c!('\u{ABCD}', 0, 7)] }
//     test_case!{ with message, "''", vec![c!(0, 1)], make_messages![
//         Message::with_help_by_str(strings::EmptyCharLiteral, vec![
//             (Span::new(0, 1), strings::CharLiteralHere), 
//         ], vec![
//             strings::CharLiteralSyntaxHelp1
//         ])
//     ]}
//     test_case!{ with message, "'ABC'", vec![c!(0, 4)], make_messages![
//         Message::new_by_str(strings::CharLiteralTooLong, vec![
//             (Span::new(0, 4), strings::CharLiteralHere),
//         ])
//     ]}
//     test_case!{ with message, r"'\c'", vec![c!(0, 3)], make_messages![
//         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
//             (Span::new(0, 0), strings::CharLiteralStartHere.to_owned()), 
//             (Span::new(1, 1), strings::UnknownCharEscapeHere.to_owned()),
//         ])
//     ]} //         012345
//     test_case!{ with message, r"'\uBG'", vec![c!(0, 5)], make_messages![
//         Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
//             (Span::new(1, 1), strings::UnicodeCharEscapeStartHere), 
//             (Span::new(4, 4), strings::UnicodeCharEscapeInvalidChar)
//         ], vec![
//             strings::UnicodeCharEscapeHelpSyntax,
//         ]),
//         Message::with_help_by_str(strings::UnexpectedCharLiteralEnd, vec![
//             (Span::new(0, 5), strings::CharLiteralHere),
//         ], vec![
//             strings::UnicodeCharEscapeHelpSyntax
//         ])
//     ]}
//     test_case!{ with message, r"'\U0011ABCD'", vec![c!(0, 11)], make_messages![
//         Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
//             (Span::new(1, 10), strings::UnicodeCharEscapeHere.to_owned()), 
//         ], vec![
//             format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
//             strings::UnicodeCharEscapeHelpValue.to_owned(),
//         ])
//     ]}
//     test_case!{ with message, r"'\na'", vec![c!(0, 4)], make_messages![
//         Message::new_by_str(strings::CharLiteralTooLong, vec![
//             (Span::new(0, 4), strings::CharLiteralHere),   
//         ])
//     ]}
//     test_case!{ with message, r"'\uABCDA'", vec![c!(0, 8)], make_messages![
//         Message::new_by_str(strings::CharLiteralTooLong, vec![
//             (Span::new(0, 8), strings::CharLiteralHere),      
//         ])
//     ]}
//     test_case!{ with message, "'", vec![c!(0, 0)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 0), strings::CharLiteralHere),    
//             (Span::new(1, 1), strings::EOFHere)
//         ])
//     ]}
//     test_case!{ with message, r"'\", vec![c!(0, 1)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 1), strings::CharLiteralHere),      
//             (Span::new(2, 2), strings::EOFHere)
//         ])
//     ]}
//     test_case!{ with message, r"'\u", vec![c!(0, 2)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 2), strings::CharLiteralHere),
//             (Span::new(3, 3), strings::EOFHere)
//         ])
//     ]}
//     test_case! { with message, r"'A", vec![c!(0, 1)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 1), strings::CharLiteralHere),
//             (Span::new(2, 2), strings::EOFHere)
//         ])
//     ]}
//     test_case! { with message, "'ABC", vec![c!(0, 3)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 3), strings::CharLiteralHere),
//             (Span::new(4, 4), strings::EOFHere)
//         ])
//     ]}
//     test_case!{ with message, r"'\'AB", vec![c!(0, 4)], make_messages![
//         Message::new_by_str(strings::UnexpectedEOF, vec![
//             (Span::new(0, 4), strings::CharLiteralHere),
//             (Span::new(5, 5), strings::EOFHere),
//         ])
//     ]}

//     // keyword, identifier, bool lit, num lit, separator
//     // byte      0         1          2         3   
//     // byte      01234567890123 456789012345678901234 5678
//     test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", [Span::new(4, 4), Span::new(18, 18), Span::new(35, 38)] expect vec![      
//         kw!(Keyword::Var, 0, 2),
//         ident!(IsId::new(2), 4, 4),
//         sep!(Separator::Eq, 6, 6),
//         lit!(true: bool, 8, 11),
//         sep!(Separator::SemiColon, 12, 12),
//         kw!(Keyword::Var, 14, 16),
//         ident!(IsId::new(3), 18, 18),
//         sep!(Separator::Eq, 20, 20),
//         lit!(789123.4560000001: r64, 22, 32),
//         sep!(Separator::SemiColon, 33, 33),
//         ident!(IsId::new(4), 35, 38),
//     ]}

//     //           0       1      2       3
//     //           0 3 67890123 690123 4 9012
//     test_case!{ "一个chinese变量, a_中文_var", [Span::new(0, 16), Span::new(21, 32)] expect vec![  // chinese ident
//         ident!(IsId::new(2), 0, 16),
//         sep!(Separator::Comma, 19, 19),
//         ident!(IsId::new(3), 21, 32),
//     ]}

//     // different postfix\types of num lit, different types of sep
//     //           0         1         2         3         4         5         6         7
//     //           0123456789012345678901234567890123456789012345678901234567890123456789012345
//     test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123r32 += 123.0 / 123u8 && 1024u8]", [] expect vec![  
//         sep!(Separator::LeftBracket, 0, 0),
//         lit!(1: i32, 1, 1),
//         sep!(Separator::Comma, 2, 2),
//         lit!(123: i32, 4, 6),
//         kw!(Keyword::Underscore, 8, 8),
//         lit!(1: u64, 10, 13),
//         sep!(Separator::LeftParen, 14, 14),
//         lit!(123.456: r64, 16, 22),
//         sep!(Separator::Comma, 23, 23),
//         sep!(Separator::RightParen, 24, 24),
//         sep!(Separator::Sub, 26, 26),
//         lit!(123456: i32, 27, 33),
//         sep!(Separator::LeftBrace, 34, 34),
//         lit!(123: u32, 35, 40),
//         sep!(Separator::RightBrace, 41, 41),
//         lit!(123.0: r32, 42, 47),
//         sep!(Separator::AddEq, 49, 50),
//         lit!(123.0: r64, 52, 56),
//         sep!(Separator::Div, 58, 58),
//         lit!(123: u8, 60, 64),
//         sep!(Separator::AndAnd, 66, 67),
//         lit!(0: i32, 69, 74),
//         sep!(Separator::RightBracket, 75, 75),
//     ], make_messages![
//         Message::with_help(
//             format!("{}, {}", strings::InvalidNumericLiteral, strings::IntegralOverflow),
//             vec![(Span::new(69, 74), String::new())],
//             vec![strings::IntegralOverflowHelpMaxValue[1].to_owned()]
//         ),
//     ]}

//     // differnt prefix\base of num lit
//     //           0         1         2         3         4         5         6         7         8
//     //           0123456789012345678901234567890123456789012345678901234567890123456789012345678901234
//     test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", [] expect vec![
//         sep!(Separator::LeftBracket, 0, 0),
//         lit!(123: i32, 1, 3),
//         sep!(Separator::Mul, 5, 5),
//         lit!(0x123: i32, 7, 11),
//         sep!(Separator::Sub, 13, 13),
//         lit!(0xAFF: i32, 15, 19),
//         sep!(Separator::And, 21, 21),
//         lit!(0o777: i32, 23, 27),
//         sep!(Separator::OrOr, 29, 30),
//         lit!(0: i32, 32, 36),
//         sep!(Separator::NotEq, 38, 39),
//         lit!(0b101010: i32, 41, 48),
//         sep!(Separator::EqEq, 50, 51),
//         lit!(0: i32, 53, 60),
//         sep!(Separator::Arrow, 62, 63),
//         lit!(123: i32, 65, 69),
//         sep!(Separator::DotDot, 70, 71),
//         lit!(0: i32, 73, 77),
//         sep!(Separator::RightBracket, 78, 78),
//         sep!(Separator::DotDot, 80, 81),
//         sep!(Separator::SubEq, 83, 84),
//     ], make_messages![
//         Message::with_help(
//             format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
//             vec![(Span::new(32, 36), String::new())],
//             vec![strings::IntLiteralAllowedChars[1].to_owned()]
//         ),
//         Message::with_help(
//             format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
//             vec![(Span::new(53, 60), String::new())],
//             vec![strings::IntLiteralAllowedChars[0].to_owned()]
//         ),
//         Message::with_help(
//             format!("{}, {}", strings::InvalidNumericLiteral, strings::InvalidCharInIntLiteral),
//             vec![(Span::new(73, 77), String::new())],
//             vec![strings::IntLiteralAllowedChars[2].to_owned()]
//         ),
//     ]}

//     //           0       1        2
//     //           012345 8901234578 123
//     test_case!{ "[1, 2，3.5, 4。5】<<=", [] expect vec![  // not ascii char hint and recover
//         sep!(Separator::LeftBracket, 0, 0),
//         lit!(1: i32, 1, 1),
//         sep!(Separator::Comma, 2, 2),
//         lit!(2: i32, 4, 4),
//         sep!(Separator::Comma, 5, 5),
//         lit!(3.5: r64, 8, 10),
//         sep!(Separator::Comma, 11, 11),
//         lit!(4.5: r64, 13, 17),
//         sep!(Separator::RightBracket, 18, 18),
//         sep!(Separator::LtLtEq, 21, 23),
//     ], make_messages![
//         Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
//             (Span::new(5, 5), ""), 
//         ], vec![
//             &format!("Did you mean `{}`({}) by `{}`({})?", ',', "COMMA", '，', "FULLWIDTH COMMA"),
//         ]),
//         Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
//             (Span::new(14, 14), ""), 
//         ], vec![
//             &format!("Did you mean `{}`({}) by `{}`({})?", '.', "FULL STOP", '。', "IDEOGRAPHIC FULL STOP"),
//         ]),
//         Message::with_help_by_str(strings::UnexpectedNonASCIIChar, vec![
//             (Span::new(18, 18), ""), 
//         ], vec![
//             &format!("Did you mean `{}`({}) by `{}`({})?", ']', "RIGHT SQUARE BRACKET", '】', "RIGHT BLACK LENTICULAR BRACKET"),
//         ]),
//     ]}

//     //           012345678
//     test_case!{ "1..2.0r32", [] expect vec![  // range operator special case
//         lit!(1: i32, 0, 0),
//         sep!(Separator::DotDot, 1, 2),
//         lit!(2.0: r32, 3, 8),
//     ]}

//     //           01234
//     test_case!{ "2...3", [] expect vec![  // range operator special case 2
//         lit!(2: i32, 0, 0),
//         sep!(Separator::DotDot, 1, 2),
//         sep!(Separator::Dot, 3, 3),
//         lit!(3: i32, 4, 4),
//     ]}

//     //           0         1         2         
//     //           012345678901234567890123456789
//     test_case!{ "1.is_odd(), 123r64.to_string()", [Span::new(2, 7), Span::new(19, 27)] expect vec![  //  another special case
//         lit!(1: i32, 0, 0),
//         sep!(Separator::Dot, 1, 1),
//         ident!(IsId::new(2), 2, 7),
//         sep!(Separator::LeftParen, 8, 8),
//         sep!(Separator::RightParen, 9, 9),
//         sep!(Separator::Comma, 10, 10),
//         lit!(123.0: r64, 12, 17),
//         sep!(Separator::Dot, 18, 18),
//         ident!(IsId::new(3), 19, 27),
//         sep!(Separator::LeftParen, 28, 28),
//         sep!(Separator::RightParen, 29, 29),
//     ]}

//     //           0           1          2
//     //           01 234567 890 1234567890123456
//     test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", [], ["hello"] expect vec![    // dispatch v1
//         lit!(2: rstr, 0, 7),
//         lit!('\u{1234}': char, 9, 16),
//         lit!(12: i32, 18, 19),
//         lit!(34: i32, 24, 25),
//     ]}

//     // bug from syntax::expr::postfix_expr
//     //           0         1         2         3         4         5
//     //           012345678901234567890123456789012345678901234567890123
//     test_case!{ "1.a[[3](4, [5, 6], )](7, 8)() and[i32].bcd[10, 11, 12]", [Span::new(2, 2), Span::new(39, 41)] expect vec![
//         lit!(1: i32, 0, 0),
//         sep!(Separator::Dot, 1, 1),
//         ident!(IsId::new(2), 2, 2),
//         sep!(Separator::LeftBracket, 3, 3),
//         sep!(Separator::LeftBracket, 4, 4),
//         lit!(3: i32, 5, 5),
//         sep!(Separator::RightBracket, 6, 6),
//         sep!(Separator::LeftParen, 7, 7),
//         lit!(4: i32, 8, 8),
//         sep!(Separator::Comma, 9, 9),
//         sep!(Separator::LeftBracket, 11, 11),
//         lit!(5: i32, 12, 12),
//         sep!(Separator::Comma, 13, 13),
//         lit!(6: i32, 15, 15),
//         sep!(Separator::RightBracket, 16, 16),
//         sep!(Separator::Comma, 17, 17),
//         sep!(Separator::RightParen, 19, 19),
//         sep!(Separator::RightBracket, 20, 20),
//         sep!(Separator::LeftParen, 21, 21),
//         lit!(7: i32, 22, 22),
//         sep!(Separator::Comma, 23, 23),
//         lit!(8: i32, 25, 25),
//         sep!(Separator::RightParen, 26, 26),
//         sep!(Separator::LeftParen, 27, 27),
//         sep!(Separator::RightParen, 28, 28),
//         kw!(Keyword::And, 30, 32),
//         sep!(Separator::LeftBracket, 33, 33),
//         kw!(Keyword::I32, 34, 36),
//         sep!(Separator::RightBracket, 37, 37),
//         sep!(Separator::Dot, 38, 38),
//         ident!(IsId::new(3), 39, 41),
//         sep!(Separator::LeftBracket, 42, 42),
//         lit!(10: i32, 43, 44),
//         sep!(Separator::Comma, 45, 45),
//         lit!(11: i32, 47, 48),
//         sep!(Separator::Comma, 49, 49),
//         lit!(12: i32, 51, 52),
//         sep!(Separator::RightBracket, 53, 53),
//     ], make_messages![
//         Message::new(format!("{}: {:?}", strings::UseReservedKeyword, Keyword::And), vec![(Span::new(30, 32), String::new())]),
//     ]}

//     //           0         1     
//     //           0123456789012345678
//     test_case!{ "abc @abc @ @@ 1 @a", [Span::new(0, 2), Span::new(12, 12), Span::new(17, 17)], [""] expect vec![
//         ident!(IsId::new(2), 0, 2),  // yeah
//         label!(IsId::new(2), 4, 7),  // yeah
//         label!(IsId::new(1), 9, 9),
//         label!(IsId::new(3), 11, 12),
//         lit!(1: i32, 14, 14),
//         label!(IsId::new(4), 16, 17),
//     ]}

//     test_case!{ "@", [] expect vec![
//         label!(IsId::new(1), 0, 0),
//     ]}

//     test_case!{ "a:", [Span::new(0, 0)] expect vec![
//         ident!(IsId::new(2), 0, 0),
//         sep!(Separator::Colon, 1, 1),
//     ]}
//     test_case!{ "@: {}", [] expect vec![
//         label!(IsId::new(1), 0, 0),
//         sep!(Separator::Colon, 1, 1),
//         sep!(Separator::LeftBrace, 3, 3),
//         sep!(Separator::RightBrace, 4, 4),
//     ]}
// }
