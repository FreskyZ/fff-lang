use crate::source::{IsId, make_source};
use super::*;

#[test]
fn v2_base() {
    use crate::source::{SourceContext, VirtualFileSystem, IsId, make_source};
        
    fn test_case_full(mut scx: SourceContext<VirtualFileSystem>, spans: &[Span], symbols: &[&'static str], expect_tokens: Vec<(Token, Span)>, expect_messages: MessageCollection) {
        let mut actual_messages = MessageCollection::new();
        let mut sess = ParseSession::new(&mut actual_messages);
        let mut chars = scx.entry("1");
        for span in spans {
            chars.intern_span(*span);
        }
        for symbol in symbols {
            chars.intern(*symbol);
        }

        let mut v2lexer = V2Lexer::new(chars);
        for expect_token in expect_tokens {
            assert_eq!(v2lexer.next(&mut sess), expect_token);
        }
        let next = v2lexer.next(&mut sess);
        if next.0 != Token::EOF { panic!("next is not EOF but {:?}", next); }

        assert_eq!(sess.messages, &expect_messages);
    }

    macro_rules! test_case {
        ($src:literal, [$($span:expr),*] expect $expect_tokens: expr) =>
            (test_case_full(make_source!($src), &[$($span),*], &[], $expect_tokens, MessageCollection::new()));
        ($src:literal, [$($span:expr),*] expect $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full(make_source!($src), &[$($span),*], &[], $expect_tokens, $expect_messages));
        ($src:literal, [$($span:expr),*], [$($string:literal),*] expect $expect_tokens: expr) =>
            (test_case_full(make_source!($src), &[$($span),*], &[$($string),*], $expect_tokens, MessageCollection::new()));
        ($src:literal, [$($span:expr),*], [$($string:literal),*] expect $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full(make_source!($src), &[$($span),*], &[$($string),*], $expect_tokens, $expect_messages));
    }

    macro_rules! lit {
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
    }
    macro_rules! label {
        ($val: expr, $start_id: expr, $end_id: expr) => ((Token::Label($val), Span::new($start_id, $end_id)))
    }
    macro_rules! kw {
        ($val: expr, $start_id: expr, $end_id: expr) => ((Token::Keyword($val), Span::new($start_id, $end_id)))
    }
    macro_rules! ident {
        ($name: expr, $start_id: expr, $end_id: expr) => ((Token::Ident($name), Span::new($start_id, $end_id)))
    }
    macro_rules! sep {
        ($sep: expr, $start_id: expr, $end_id: expr) => ((Token::Sep($sep), Span::new($start_id, $end_id)))
    }


    // keyword, identifier, bool lit, num lit, separator
    // byte      0         1          2         3   
    // byte      01234567890123 456789012345678901234 5678
    test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", [Span::new(4, 4), Span::new(18, 18), Span::new(35, 38)] expect vec![      
        kw!(Keyword::Var, 0, 2),
        ident!(IsId::new(2), 4, 4),
        sep!(Separator::Eq, 6, 6),
        lit!(true: bool, 8, 11),
        sep!(Separator::SemiColon, 12, 12),
        kw!(Keyword::Var, 14, 16),
        ident!(IsId::new(3), 18, 18),
        sep!(Separator::Eq, 20, 20),
        lit!(789123.4560000001: r64, 22, 32),
        sep!(Separator::SemiColon, 33, 33),
        ident!(IsId::new(4), 35, 38),
    ]}

    //           0       1      2       3
    //           0 3 67890123 690123 4 9012
    test_case!{ "一个chinese变量, a_中文_var", [Span::new(0, 16), Span::new(21, 32)] expect vec![  // chinese ident
        ident!(IsId::new(2), 0, 16),
        sep!(Separator::Comma, 19, 19),
        ident!(IsId::new(3), 21, 32),
    ]}

    // different postfix\types of num lit, different types of sep
    //           0         1         2         3         4         5         6         7
    //           0123456789012345678901234567890123456789012345678901234567890123456789012345
    test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123r32 += 123.0 / 123u8 && 1024u8]", [] expect vec![  
        sep!(Separator::LeftBracket, 0, 0),
        lit!(1: i32, 1, 1),
        sep!(Separator::Comma, 2, 2),
        lit!(123: i32, 4, 6),
        kw!(Keyword::Underscore, 8, 8),
        lit!(1: u64, 10, 13),
        sep!(Separator::LeftParen, 14, 14),
        lit!(123.456: r64, 16, 22),
        sep!(Separator::Comma, 23, 23),
        sep!(Separator::RightParen, 24, 24),
        sep!(Separator::Sub, 26, 26),
        lit!(123456: i32, 27, 33),
        sep!(Separator::LeftBrace, 34, 34),
        lit!(123: u32, 35, 40),
        sep!(Separator::RightBrace, 41, 41),
        lit!(123.0: r32, 42, 47),
        sep!(Separator::AddEq, 49, 50),
        lit!(123.0: r64, 52, 56),
        sep!(Separator::Div, 58, 58),
        lit!(123: u8, 60, 64),
        sep!(Separator::AndAnd, 66, 67),
        lit!(0: i32, 69, 74),
        sep!(Separator::RightBracket, 75, 75),
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
    test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] .. -=", [] expect vec![
        sep!(Separator::LeftBracket, 0, 0),
        lit!(123: i32, 1, 3),
        sep!(Separator::Mul, 5, 5),
        lit!(0x123: i32, 7, 11),
        sep!(Separator::Sub, 13, 13),
        lit!(0xAFF: i32, 15, 19),
        sep!(Separator::And, 21, 21),
        lit!(0o777: i32, 23, 27),
        sep!(Separator::OrOr, 29, 30),
        lit!(0: i32, 32, 36),
        sep!(Separator::NotEq, 38, 39),
        lit!(0b101010: i32, 41, 48),
        sep!(Separator::EqEq, 50, 51),
        lit!(0: i32, 53, 60),
        sep!(Separator::Arrow, 62, 63),
        lit!(123: i32, 65, 69),
        sep!(Separator::DotDot, 70, 71),
        lit!(0: i32, 73, 77),
        sep!(Separator::RightBracket, 78, 78),
        sep!(Separator::DotDot, 80, 81),
        sep!(Separator::SubEq, 83, 84),
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
    test_case!{ "[1, 2，3.5, 4。5】<<=", [] expect vec![  // not ascii char hint and recover
        sep!(Separator::LeftBracket, 0, 0),
        lit!(1: i32, 1, 1),
        sep!(Separator::Comma, 2, 2),
        lit!(2: i32, 4, 4),
        sep!(Separator::Comma, 5, 5),
        lit!(3.5: r64, 8, 10),
        sep!(Separator::Comma, 11, 11),
        lit!(4.5: r64, 13, 17),
        sep!(Separator::RightBracket, 18, 18),
        sep!(Separator::LtLtEq, 21, 23),
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
    test_case!{ "1..2.0r32", [] expect vec![  // range operator special case
        lit!(1: i32, 0, 0),
        sep!(Separator::DotDot, 1, 2),
        lit!(2.0: r32, 3, 8),
    ]}

    //           01234
    test_case!{ "2...3", [] expect vec![  // range operator special case 2
        lit!(2: i32, 0, 0),
        sep!(Separator::DotDot, 1, 2),
        sep!(Separator::Dot, 3, 3),
        lit!(3: i32, 4, 4),
    ]}

    //           0         1         2         
    //           012345678901234567890123456789
    test_case!{ "1.is_odd(), 123r64.to_string()", [Span::new(2, 7), Span::new(19, 27)] expect vec![  //  another special case
        lit!(1: i32, 0, 0),
        sep!(Separator::Dot, 1, 1),
        ident!(IsId::new(2), 2, 7),
        sep!(Separator::LeftParen, 8, 8),
        sep!(Separator::RightParen, 9, 9),
        sep!(Separator::Comma, 10, 10),
        lit!(123.0: r64, 12, 17),
        sep!(Separator::Dot, 18, 18),
        ident!(IsId::new(3), 19, 27),
        sep!(Separator::LeftParen, 28, 28),
        sep!(Separator::RightParen, 29, 29),
    ]}

    //           0           1          2
    //           01 234567 890 1234567890123456
    test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", [], ["hello"] expect vec![    // dispatch v1
        lit!(2: rstr, 0, 7),
        lit!('\u{1234}': char, 9, 16),
        lit!(12: i32, 18, 19),
        lit!(34: i32, 24, 25),
    ]}

    // bug from syntax::expr::postfix_expr
    //           0         1         2         3         4         5
    //           012345678901234567890123456789012345678901234567890123
    test_case!{ "1.a[[3](4, [5, 6], )](7, 8)() and[i32].bcd[10, 11, 12]", [Span::new(2, 2), Span::new(39, 41)] expect vec![
        lit!(1: i32, 0, 0),
        sep!(Separator::Dot, 1, 1),
        ident!(IsId::new(2), 2, 2),
        sep!(Separator::LeftBracket, 3, 3),
        sep!(Separator::LeftBracket, 4, 4),
        lit!(3: i32, 5, 5),
        sep!(Separator::RightBracket, 6, 6),
        sep!(Separator::LeftParen, 7, 7),
        lit!(4: i32, 8, 8),
        sep!(Separator::Comma, 9, 9),
        sep!(Separator::LeftBracket, 11, 11),
        lit!(5: i32, 12, 12),
        sep!(Separator::Comma, 13, 13),
        lit!(6: i32, 15, 15),
        sep!(Separator::RightBracket, 16, 16),
        sep!(Separator::Comma, 17, 17),
        sep!(Separator::RightParen, 19, 19),
        sep!(Separator::RightBracket, 20, 20),
        sep!(Separator::LeftParen, 21, 21),
        lit!(7: i32, 22, 22),
        sep!(Separator::Comma, 23, 23),
        lit!(8: i32, 25, 25),
        sep!(Separator::RightParen, 26, 26),
        sep!(Separator::LeftParen, 27, 27),
        sep!(Separator::RightParen, 28, 28),
        kw!(Keyword::And, 30, 32),
        sep!(Separator::LeftBracket, 33, 33),
        kw!(Keyword::I32, 34, 36),
        sep!(Separator::RightBracket, 37, 37),
        sep!(Separator::Dot, 38, 38),
        ident!(IsId::new(3), 39, 41),
        sep!(Separator::LeftBracket, 42, 42),
        lit!(10: i32, 43, 44),
        sep!(Separator::Comma, 45, 45),
        lit!(11: i32, 47, 48),
        sep!(Separator::Comma, 49, 49),
        lit!(12: i32, 51, 52),
        sep!(Separator::RightBracket, 53, 53),
    ], make_messages![
        Message::new(format!("{}: {:?}", strings::UseReservedKeyword, Keyword::And), vec![(Span::new(30, 32), String::new())]),
    ]}

    //           0         1     
    //           0123456789012345678
    test_case!{ "abc @abc @ @@ 1 @a", [Span::new(0, 2), Span::new(12, 12), Span::new(17, 17)], [""] expect vec![
        ident!(IsId::new(2), 0, 2),  // yeah
        label!(IsId::new(2), 4, 7),  // yeah
        label!(IsId::new(1), 9, 9),
        label!(IsId::new(3), 11, 12),
        lit!(1: i32, 14, 14),
        label!(IsId::new(4), 16, 17),
    ]}

    test_case!{ "@", [] expect vec![
        label!(IsId::new(1), 0, 0),
    ]}

    test_case!{ "a:", [Span::new(0, 0)] expect vec![
        ident!(IsId::new(2), 0, 0),
        sep!(Separator::Colon, 1, 1),
    ]}
    test_case!{ "@: {}", [] expect vec![
        label!(IsId::new(1), 0, 0),
        sep!(Separator::Colon, 1, 1),
        sep!(Separator::LeftBrace, 3, 3),
        sep!(Separator::RightBrace, 4, 4),
    ]}
}

#[test]
fn v4_base() { // remain the name of v4 here for memory

    // numeric, 123, 1:1-1:3
    // identifier, abc, 1:5-1:7
    // char, 'd', 1:9-1:11
    // seperator, comma, 1:12-1:12
    // seperator, leftbracket, 1:14-1:14
    // numeric, 1, 1:15-1:15
    // seperator, rightbracket, 1:16-1:16
    // EOF, 1:17-1:17
    // EOFs, 1:17-1:17
    let mut scx = make_source!("123 abc 'd', [1]");
    let chars = scx.entry("1");
    let mut messages = MessageCollection::new();
    let tokens = TokenStream::new(chars, &mut messages);

    assert_eq!(tokens.nth_token(0), &Token::Num(Numeric::I32(123)));
    assert_eq!(tokens.nth_span(0), Span::new(0, 2));

    assert_eq!(tokens.nth_token(1), &Token::Ident(IsId::new(2)));
    assert_eq!(tokens.nth_span(1), Span::new(4, 6));

    assert_eq!(tokens.nth_token(2), &Token::Char('d'));
    assert_eq!(tokens.nth_span(2), Span::new(8, 10));

    assert_eq!(tokens.nth_token(3), &Token::Sep(Separator::Comma));
    assert_eq!(tokens.nth_span(3), Span::new(11, 11));

    assert_eq!(tokens.nth_token(4), &Token::Sep(Separator::LeftBracket));
    assert_eq!(tokens.nth_span(4), Span::new(13, 13));

    assert_eq!(tokens.nth_token(5), &Token::Num(Numeric::I32(1)));
    assert_eq!(tokens.nth_span(5), Span::new(14, 14));

    assert_eq!(tokens.nth_token(6), &Token::Sep(Separator::RightBracket));
    assert_eq!(tokens.nth_span(6), Span::new(15, 15));

    assert_eq!(tokens.nth_token(7), &Token::EOF);
    assert_eq!(tokens.nth_span(7), Span::new(16, 16));

    assert_eq!(tokens.nth_token(8), &Token::EOF);
    assert_eq!(tokens.nth_span(8), Span::new(16, 16));

    assert_eq!(tokens.nth_token(9), &Token::EOF);
    assert_eq!(tokens.nth_span(9), Span::new(16, 16));

    assert_eq!(tokens.nth_token(42), &Token::EOF);
    assert_eq!(tokens.nth_span(42), Span::new(16, 16));
}
