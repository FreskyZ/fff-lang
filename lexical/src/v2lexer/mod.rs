
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char

mod num_lit_parser;
mod unicode_char;

use codepos::StringPosition;
use codemap::CodeChars;
use message::Message;
use message::MessageCollection;

use super::v1lexer::V1Token;
use super::v1lexer::V1Lexer;

use super::buf_lexer::ILexer;
use super::buf_lexer::BufLexer;

use super::LitValue;
use super::KeywordKind;
use super::SeperatorKind;
use self::num_lit_parser::parse_numeric_literal;

mod error_strings;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum V2Token {
    Literal(LitValue),
    Identifier(String),   // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Keyword(KeywordKind),
    Seperator(SeperatorKind),
    EOF,
    EOFs,
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V2Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V2Token::Literal(ref value) => write!(f, "{:?}", value),
            V2Token::Identifier(ref value) => write!(f, "Identifier {:?}", value),
            V2Token::Keyword(ref kind) => write!(f, "Keyword {:?}", kind),
            V2Token::Seperator(ref kind) => write!(f, "Seperator {:?}", kind),
            V2Token::EOF => write!(f, "EOF"),
            V2Token::EOFs => write!(f, "EOFs"),
        }
    }
}

trait IdentifierChar {

    fn is_identifier_start(&self) -> bool;
    fn is_identifier(&self) -> bool;

    fn is_numeric_literal_start(&self) -> bool;
    fn is_numeric_literal(&self) -> bool;

    fn is_seperator(&self) -> bool;
    
    fn pass_non_ascii_char(&self, strpos: StringPosition, messages: &mut MessageCollection) -> char; 
}
impl IdentifierChar for char {

    // Include chinese alphabetical char
    fn is_identifier_start(&self) -> bool {
        *self == '_' || self.is_alphabetic()
    }
    // Include digit
    fn is_identifier(&self) -> bool {
        *self == '_' || self.is_alphabetic() || self.is_digit(10)  
    }

    // Only digit, '.' start is not supported
    // Update: remove '-' here, 
    //     that is, take several weeks to impl '-' and 'E' feature in num lit, but finally remove the feature in v2
    //     leave the feature in num lit parser for future use of FromStr
    fn is_numeric_literal_start(&self) -> bool {
        self.is_digit(10)
    }
    // Only digit or ASCII letters or underscore
    fn is_numeric_literal(&self) -> bool {
        *self == '_' || self.is_digit(36) || *self == '.'
    }

    fn is_seperator(&self) -> bool {
        !self.is_identifier()
    }

    fn pass_non_ascii_char(&self, strpos: StringPosition, messages: &mut MessageCollection) -> char {
        use self::unicode_char::check_unicode_char;

        match check_unicode_char(*self) {
            Some((unicode_ch, unicode_name, ascii_ch, ascii_name)) => {
                messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
                    (strpos, ""), 
                ], vec![
                    &format!("Did you mean `{}`({}) by `{}`({})?", ascii_ch, ascii_name, unicode_ch, unicode_name),
                ]));
                ascii_ch
            }
            None => *self,
        }
    }
}

pub struct V2Lexer<'chs> {
    v1: BufLexer<V1Lexer<'chs>, V1Token>,
}
impl<'chs> ILexer<'chs, V2Token> for V2Lexer<'chs> {

    fn new(content_chars: CodeChars<'chs>, messages: &mut MessageCollection) -> V2Lexer<'chs> {
        V2Lexer { 
            v1: BufLexer::new(content_chars, messages),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageCollection) -> (V2Token, StringPosition) {

        #[derive(Debug)] // temp-test-only
        struct V15Token(char, StringPosition, char, StringPosition, char, StringPosition);

        #[derive(Debug)] // temp-test-only
        enum State {
            Nothing,
            InIdent(String, StringPosition),
            InNumLit(String, StringPosition),
        }

        let mut state = State::Nothing;
        let mut eofed = false;
        loop {
            self.v1.move_next(messages);
            let v15 = match self.v1.current_with_preview2() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::CharLiteral(ref value), pos, _2, _3, _4, _5) => {
                    return (V2Token::Literal(LitValue::Char(value.clone())), pos);
                }
                (&V1Token::EOF, eof_pos, _2, next_strpos, _4, nextnext_strpos) => {
                    // because if last token is ident, it will not finish because eof return early here
                    if eofed {
                        return (V2Token::EOF, eof_pos);
                    } else {
                        eofed = true;
                        V15Token(' ', eof_pos, ' ', next_strpos, ' ', nextnext_strpos)
                    }
                }
                (&V1Token::EOFs, eofs_pos, _2, _3, _4, _5) => {
                    return (V2Token::EOFs, eofs_pos);
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, &V1Token::Other(nextnext_ch), nextnext_strpos) => {
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    // let next_ch = next_ch.pass_non_ascii_char(next_strpos, messages);  // check next time
                    // let nextnext_ch = nextnext_ch.pass_non_ascii_char(nextnext_strpos, messages);
                    V15Token(ch, strpos, next_ch, next_strpos, nextnext_ch, nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, &V1Token::Other(next_ch), next_strpos, _4, nextnext_strpos) => { // even EOF can be regarded as space
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    // let next_ch = next_ch.pass_non_ascii_char(next_strpos, messages);
                    V15Token(ch, strpos, next_ch, next_strpos, ' ', nextnext_strpos)
                }
                (&V1Token::Other(ch), strpos, _2, next_strpos, _4, nextnext_strpos) => { 
                    let ch = ch.pass_non_ascii_char(strpos, messages);
                    V15Token(ch, strpos, ' ', next_strpos, ' ', nextnext_strpos)
                }
            };
            // println!("state is {:?}, v15 is {:?}", state, v15);

            match (state, v15) {
                (State::Nothing, V15Token(ch, strpos, next_ch, next_strpos, nextnext_ch, nextnext_strpos)) => {
                    if ch.is_identifier_start() {
                        let mut value = String::new();
                        value.push(ch);
                        state = State::InIdent(value, strpos);
                    } else if ch.is_numeric_literal_start() {
                        let mut value = String::new();
                        value.push(ch);
                        state = State::InNumLit(value, strpos);
                    } else {
                        match SeperatorKind::try_from3(ch, next_ch, nextnext_ch) { // the try_from will check 3, if not, check 2, if not, check 1
                            Some(sep) => match sep.len() { 
                                1 => {
                                    return (V2Token::Seperator(sep), strpos);
                                }
                                2 => {
                                    self.v1.prepare_skip1();
                                    return (V2Token::Seperator(sep), StringPosition::merge(strpos, next_strpos));
                                }
                                3 => {
                                    self.v1.prepare_skip1();
                                    self.v1.prepare_skip1();
                                    return (V2Token::Seperator(sep), StringPosition::merge(strpos, nextnext_strpos));
                                }
                                _ => unreachable!(),
                            },
                            None => state = State::Nothing,
                        }
                    }
                } 
                (State::InIdent(mut value, mut ident_strpos), V15Token(ch, strpos, _3, _4, _5, _6)) => {
                    if !ch.is_identifier() {
                        self.v1.prepare_dummy1();
                        match KeywordKind::try_from(&value) { 
                            Some(keyword) => match keyword {
                                KeywordKind::True => return (V2Token::Literal(LitValue::from(true)), ident_strpos),
                                KeywordKind::False => return (V2Token::Literal(LitValue::from(false)), ident_strpos),
                                other_keyword => return (V2Token::Keyword(other_keyword), ident_strpos),
                            },
                            None => return (V2Token::Identifier(value), ident_strpos),
                        }
                    } else {
                        value.push(ch);
                        ident_strpos = StringPosition::merge(ident_strpos, strpos);
                        state = State::InIdent(value, ident_strpos);
                    }
                }
                (State::InNumLit(mut value, mut num_lit_strpos), V15Token(ch, strpos, next_ch, _4, _5, _6)) => {
                    // 1. for `1..2` case; 2. for `1.some_member_fn()` case, not considered `123.f32` because it is invalid
                    if (ch == '.' && next_ch == '.')
                        || (ch == '.' && next_ch.is_identifier_start()) 
                        || !ch.is_numeric_literal() {
                        self.v1.prepare_dummy1();
                        let (num_lit_val, pos) = parse_numeric_literal(value, num_lit_strpos, messages);
                        return (V2Token::Literal(LitValue::Num(num_lit_val)), pos);
                    } else {
                        value.push(ch);
                        num_lit_strpos = StringPosition::merge(num_lit_strpos, strpos);
                        state = State::InNumLit(value, num_lit_strpos);
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[test]
fn v2_char_ext() {
    
    assert_eq!('a'.is_identifier_start(), true);
    assert_eq!('啊'.is_identifier_start(), true);
    assert_eq!(','.is_identifier_start(), false);
    assert_eq!('，'.is_identifier_start(), false);
    assert_eq!('_'.is_identifier_start(), true);
    assert_eq!('.'.is_identifier_start(), false);
    assert_eq!('1'.is_identifier_start(), false);

    assert_eq!('a'.is_identifier(), true);
    assert_eq!('啊'.is_identifier(), true);
    assert_eq!(','.is_identifier(), false);
    assert_eq!('，'.is_identifier(), false);
    assert_eq!('_'.is_identifier(), true);
    assert_eq!('.'.is_identifier(), false);
    assert_eq!('1'.is_identifier(), true);

    assert_eq!('a'.is_numeric_literal_start(), false);
    assert_eq!('啊'.is_numeric_literal_start(), false);
    assert_eq!(','.is_numeric_literal_start(), false);
    assert_eq!('，'.is_numeric_literal_start(), false);
    assert_eq!('_'.is_numeric_literal_start(), false);
    assert_eq!('1'.is_numeric_literal_start(), true);
    assert_eq!('.'.is_numeric_literal_start(), false);

    assert_eq!('a'.is_numeric_literal(), true);
    assert_eq!('啊'.is_numeric_literal(), false);
    assert_eq!(','.is_numeric_literal(), false);
    assert_eq!('，'.is_numeric_literal(), false);
    assert_eq!('_'.is_numeric_literal(), true);
    assert_eq!('1'.is_numeric_literal(), true);
    assert_eq!('.'.is_numeric_literal(), true);
    
    assert_eq!('a'.is_seperator(), false);
    assert_eq!('啊'.is_seperator(), false);
    assert_eq!(','.is_seperator(), true);
    assert_eq!('，'.is_seperator(), true);
    assert_eq!('_'.is_seperator(), false);
    assert_eq!('.'.is_seperator(), true);
    assert_eq!('1'.is_seperator(), false);
}

#[cfg(test)]
#[test]
fn v2_non_ascii_ch() {
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('.'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!('\\'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }
    
    {
        let messages = &mut MessageCollection::new();
        assert_eq!(';'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('。'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), '.');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_str_pos!(3, 4, 5, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('⧹'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), '\\');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_str_pos!(3, 4, 5, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", '\\', "Backslash", '⧹', "Big Reverse Solidus"),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {
        let messages = &mut MessageCollection::new();
        assert_eq!('；'.pass_non_ascii_char(make_str_pos!(3, 4, 5, 6), messages), ';');

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
            (make_str_pos!(3, 4, 5, 6), ""), 
        ], vec![
            &format!("Did you mean `{}`({}) by `{}`({})?", ';', "Semicolon", '；', "Fullwidth Semicolon"),
        ]));
        assert_eq!(messages, expect_messages);
    }
}

#[cfg(test)]
#[test]
fn v2_base() {
    use codemap::CodeMap;

    // Only to make decltype(V2Lexer as BufLexer::next(...)) to display better
    #[derive(Eq, PartialEq)]
    struct V2AndStrPos(V2Token, StringPosition);
    use std::fmt;
    impl From<(V2Token, StringPosition)> for V2AndStrPos {
        fn from(v2_and_strpos: (V2Token, StringPosition)) -> V2AndStrPos {
            V2AndStrPos(v2_and_strpos.0, v2_and_strpos.1)
        }
    }
    impl fmt::Debug for V2AndStrPos {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "\n({:?}, {:?})", self.0, self.1)
        }
    }

    macro_rules! test_case {
        ($program: expr, [$($expect: expr, )*] [$($expect_msg: expr, )*]) => ({
            let messages = &mut MessageCollection::new();
            let mut codemap = CodeMap::with_str($program);
            let mut v2lexer = V2Lexer::new(codemap.iter(), messages);
            let mut v2s = Vec::new();
            loop {
                match v2lexer.next(messages) {
                    (V2Token::EOFs, _) => break,
                    v2 => v2s.push(V2AndStrPos::from(v2)),
                }
            }

            assert_eq!(v2s, vec![$(V2AndStrPos::from($expect), )*]);

            let expect_messages = &mut MessageCollection::new();
            $(
                expect_messages.push($expect_msg);
            )*
            assert_eq!(messages, expect_messages);
        });
        ($program: expr, [$($expect: expr, )*]) => (test_case!($program, [$($expect, )*] []))
    }

    macro_rules! lit {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Literal(LitValue::from($val)), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! lit_num_none {
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Literal(LitValue::Num(None)), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! kw {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Keyword($val), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! ident {
        ($name: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Identifier($name.to_owned()), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! sep {
        ($sep: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Seperator($sep), make_str_pos!($row1, $col1, $row2, $col2))
        )
    }

    // row       1              2                     3
    // col       0        1     0        1         2  0
    // col       1234567890123 412345678901234567890 11234
    test_case!{ "var a = true;\nvar b = 789_123.456;\ndefg", [  // keyword, identifier, bool lit, num lit, seperator
            kw!(KeywordKind::Var, 1, 1, 1, 3),
            ident!("a", 1, 5, 1, 5),
            sep!(SeperatorKind::Assign, 1, 7, 1, 7),
            lit!(true, 1, 9, 1, 12),
            sep!(SeperatorKind::SemiColon, 1, 13, 1, 13),
            kw!(KeywordKind::Var, 2, 1, 2, 3),
            ident!("b", 2, 5, 2, 5),
            sep!(SeperatorKind::Assign, 2, 7, 2, 7),
            lit!(789123.4560000001, 2, 9, 2, 19),
            sep!(SeperatorKind::SemiColon, 2, 20, 2, 20),
            ident!("defg", 3, 1, 3, 4),
        ]
    }

    //           0          1            2
    //           1 2 34567890 123456 7 8901
    test_case!{ "一个chinese变量, a_中文_var", [  // chinese ident
            ident!("一个chinese变量", 1, 1, 1, 11),
            sep!(SeperatorKind::Comma, 1, 12, 1, 12),
            ident!("a_中文_var", 1, 14, 1, 21),
        ]
    }

    //           0        1         2         3         4         5         6         7
    //           1234567890123456789012345678901234567890123456789012345678901234567890123456
    test_case!{ "[1, 123 _ 1u64( 123.456,) -123_456{123u32}123f32 += 123.0 / 123u8 && 1024u8]", [  // different postfix\types of num lit, different types of sep
            sep!(SeperatorKind::LeftBracket, 1, 1, 1, 1),
            lit!(1, 1, 2, 1, 2),
            sep!(SeperatorKind::Comma, 1, 3, 1, 3),
            lit!(123, 1, 5, 1, 7),
            ident!("_", 1, 9, 1, 9),
            lit!(1u64, 1, 11, 1, 14),
            sep!(SeperatorKind::LeftParenthenes, 1, 15, 1, 15),
            lit!(123.456, 1, 17, 1, 23),
            sep!(SeperatorKind::Comma, 1, 24, 1, 24),
            sep!(SeperatorKind::RightParenthenes, 1, 25, 1, 25),
            sep!(SeperatorKind::Sub, 1, 27, 1, 27),
            lit!(123456, 1, 28, 1, 34),
            sep!(SeperatorKind::LeftBrace, 1, 35, 1, 35),
            lit!(123u32, 1, 36, 1, 41),
            sep!(SeperatorKind::RightBrace, 1, 42, 1, 42),
            lit!(123f32, 1, 43, 1, 48),
            sep!(SeperatorKind::AddAssign, 1, 50, 1, 51),
            lit!(123.0, 1, 53, 1, 57),
            sep!(SeperatorKind::Div, 1, 59, 1, 59),
            lit!(123u8, 1, 61, 1, 65),
            sep!(SeperatorKind::LogicalAnd, 1, 67, 1, 68),
            lit_num_none!(1, 70, 1, 75),
            sep!(SeperatorKind::RightBracket, 1, 76, 1, 76),
        ] [
            Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::IntegralOverflow),
                vec![(make_str_pos!(1, 70, 1, 75), String::new())],
                vec![error_strings::IntegralOverflowHelpMaxValue[1].to_owned()]
            ),
        ]
    }

    //           0        1         2         3         4         5         6         7         8
    //           1234567890123456789012345678901234567890123456789012345678901234567890123456789012345
    test_case!{ "[123 * 0x123 - 0xAFF & 0o777 || 0oXXX != 0b101010 == 0b123456 -> 0d123.. 0dABC] -- -=", [    // differnt prefix\base of num lit
            sep!(SeperatorKind::LeftBracket, 1, 1, 1, 1),
            lit!(123, 1, 2, 1, 4),
            sep!(SeperatorKind::Mul, 1, 6, 1, 6),
            lit!(0x123, 1, 8, 1, 12),
            sep!(SeperatorKind::Sub, 1, 14, 1, 14),
            lit!(0xAFF, 1, 16, 1, 20),
            sep!(SeperatorKind::BitAnd, 1, 22, 1, 22),
            lit!(0o777, 1, 24, 1, 28),
            sep!(SeperatorKind::LogicalOr, 1, 30, 1, 31),
            lit_num_none!(1, 33, 1, 37),
            sep!(SeperatorKind::NotEqual, 1, 39, 1, 40),
            lit!(0b101010, 1, 42, 1, 49),
            sep!(SeperatorKind::Equal, 1, 51, 1, 52),
            lit_num_none!(1, 54, 1, 61),
            sep!(SeperatorKind::NarrowRightArrow, 1, 63, 1, 64),
            lit!(123, 1, 66, 1, 70),
            sep!(SeperatorKind::Range, 1, 71, 1, 72),
            lit_num_none!(1, 74, 1, 78),
            sep!(SeperatorKind::RightBracket, 1, 79, 1, 79),
            sep!(SeperatorKind::Decrease, 1, 81, 1, 82),
            sep!(SeperatorKind::SubAssign, 1, 84, 1, 85),
        ] [
            Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
                vec![(make_str_pos!(1, 33, 1, 37), String::new())],
                vec![error_strings::IntLiteralAllowedChars[1].to_owned()]
            ),
            Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
                vec![(make_str_pos!(1, 54, 1, 61), String::new())],
                vec![error_strings::IntLiteralAllowedChars[0].to_owned()]
            ),
            Message::with_help(
                format!("{}, {}", error_strings::InvalidNumericLiteral, error_strings::InvalidCharInIntLiteral),
                vec![(make_str_pos!(1, 74, 1, 78), String::new())],
                vec![error_strings::IntLiteralAllowedChars[2].to_owned()]
            ),
        ]
    }

    //           0         1
    //           123456 7890123 45678
    test_case!{ "[1, 2，3.5, 4。5】<<=", [  // not ascii char hint and recover
            sep!(SeperatorKind::LeftBracket, 1, 1, 1, 1),
            lit!(1, 1, 2, 1, 2),
            sep!(SeperatorKind::Comma, 1, 3, 1, 3),
            lit!(2, 1, 5, 1, 5),
            sep!(SeperatorKind::Comma, 1, 6, 1, 6),
            lit!(3.5, 1, 7, 1, 9),
            sep!(SeperatorKind::Comma, 1, 10, 1, 10),
            lit!(4.5, 1, 12, 1, 14),
            sep!(SeperatorKind::RightBracket, 1, 15, 1, 15),
            sep!(SeperatorKind::ShiftLeftEqual, 1, 16, 1, 18),
        ] [
            Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
                (make_str_pos!(1, 6, 1, 6), ""), 
            ], vec![
                &format!("Did you mean `{}`({}) by `{}`({})?", ',', "Comma", '，', "Fullwidth Comma"),
            ]),
            Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
                (make_str_pos!(1, 13, 1, 13), ""), 
            ], vec![
                &format!("Did you mean `{}`({}) by `{}`({})?", '.', "Period", '。', "Ideographic Full Stop"),
            ]),
            Message::with_help_by_str(error_strings::UnexpectedNonASCIIChar, vec![
                (make_str_pos!(1, 15, 1, 15), ""), 
            ], vec![
                &format!("Did you mean `{}`({}) by `{}`({})?", ']', "Right Square Bracket", '】', "Right Black Lenticular Bracket"),
            ]),
        ]
    }

    //           123456789
    test_case!{ "1..2.0f32",  [  // range operator special case
            lit!(1, 1, 1, 1, 1),
            sep!(SeperatorKind::Range, 1, 2, 1, 3),
            lit!(2f32, 1, 4, 1, 9),
        ]
    }

    test_case!{ "2...3",  [  // range operator special case 2
            lit!(2, 1, 1, 1, 1),
            sep!(SeperatorKind::Range, 1, 2, 1, 3),
            sep!(SeperatorKind::Dot, 1, 4, 1, 4),
            lit!(3, 1, 5, 1, 5),
        ]
    }

    //           1234567890
    test_case!{ "1.is_odd()", [  //  another special case
            lit!(1, 1, 1, 1, 1),
            sep!(SeperatorKind::Dot, 1, 2, 1, 2),
            ident!("is_odd", 1, 3, 1, 8),
            sep!(SeperatorKind::LeftParenthenes, 1, 9, 1, 9),
            sep!(SeperatorKind::RightParenthenes, 1, 10, 1, 10),
        ]
    }

    //           0          1          2
    //           1 234567 8901 234567890123456
    test_case!{ "r\"hello\" '\\u1234' 12/**/34 ", [    // dispatch v1
            lit!("hello", 1, 1, 1, 8),
            lit!('\u{1234}', 1, 10, 1, 17),
            lit!(12, 1, 19, 1, 20),
            lit!(34, 1, 25, 1, 26),
        ]
    }
}
