
///! Level1 parser
///! input v0
///! remove line comment
///! report block comment as OtherChar ' '
///! find string literal with only '"' escaped
///! string literal is allowed to cross line, line end is regarded as \n
///! raw string literal supported, `r'C:\\abc'` or `R"C:\\abc"`

// TODO: check better usage of Message's StringPositions, because previously they use Position

mod escape_char_parser;
mod char_lit_parser;
mod string_lit_parser;
mod raw_string_lit_parser;
mod error_strings;

use std::str::Chars;
use codepos::Position;
use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

// use super::buf_lexer::ILexer;
use super::buf_lexer::IDetailLexer;
use super::buf_lexer::BufLexer;
use super::buf_lexer::BufToken;
use super::buf_lexer::LegacyBufLexer;

use super::v0lexer::V0Lexer;
use super::v0lexer::EOFCHAR;

use self::string_lit_parser::StringLiteralParser;
use self::string_lit_parser::StringLiteralParserResult;
use self::raw_string_lit_parser::RawStringLiteralParser;
use self::raw_string_lit_parser::RawStringLiteralParserResult;
use self::char_lit_parser::CharLiteralParser;
use self::char_lit_parser::CoverageRecorder;
use self::char_lit_parser::CharLiteralParserResult;

#[cfg(test)]
#[derive(Clone, Eq, PartialEq)]
pub enum V1Token {
    StringLiteral(Option<String>, StringPosition),
    RawStringLiteral(Option<String>, StringPosition),
    CharLiteral(Option<char>, StringPosition),
    Other { ch: char, pos: Position },
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum V1Token {
    StringLiteral(Option<String>, StringPosition),
    RawStringLiteral(Option<String>, StringPosition),
    CharLiteral(Option<char>, StringPosition),
    Other { ch: char, pos: Position },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V1Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V1Token::StringLiteral(ref value, ref pos) => write!(f, "String litera {:?} at {:?}", value, pos),
            V1Token::RawStringLiteral(ref value, ref pos) => write!(f, "Raw string literal {:?} at {:?}", value, pos),
            V1Token::CharLiteral(ref value, ref pos) => write!(f, "Char literal {:?} at {:?}", value, pos),
            V1Token::Other { ref ch, ref pos } => {
                write!(f, "Other {:?} at {:?}", ch, pos)
            }
        }
    }
}

pub struct V1Lexer<'chs> {
    v0: BufLexer<V0Lexer<'chs>, char>,
}
impl<'chs> IDetailLexer<'chs, V1Token> for V1Lexer<'chs> {

    fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> V1Lexer<'chs> {
        V1Lexer { 
            v0: BufLexer::new(content_chars, messages),
        }
    }
    
    fn position(&self) -> Position { Position::new() }

    // input v0, output stringliteral or otherchar without comment
    fn next(&mut self, messages: &mut MessageCollection) -> Option<V1Token> {
        // First there is quote, and anything inside is regarded as string literal, include `\n` as real `\n`
        // and then outside of quote pair there is comments, anything inside comment, // and /n, or /* and */ is regarded as comment
        
        let dummy_coverage_recorder = &mut CoverageRecorder::new();

        enum State {
            Nothing,
            InStringLiteral { parser: StringLiteralParser },
            InRawStringLiteral { parser: RawStringLiteralParser },
            InLineComment,
            InBlockComment { start_pos: Position },
            InCharLiteral { parser: CharLiteralParser },
        }

        let mut state = State::Nothing;
        loop {
            self.v0.move_next(messages);
            match self.v0.current_with_state(state) {
                (State::Nothing, &'/', _1, &'/', _4, _5, _6) => {                       // C1: in nothing, meet //
                    self.v0.prepare_skip1();
                    state = State::InLineComment;                                       
                }
                (State::Nothing, &'/', start_strpos, &'*', _4, _5, _6) => {             // C2: in nothing, meet /*
                    state = State::InBlockComment { start_pos: start_strpos.start_pos() };                  
                }
                (State::Nothing, &'"', start_strpos, _3, _4, _5, _6) => {               // C3: in nothing, meet "
                    state = State::InStringLiteral { parser: StringLiteralParser::new(start_strpos.start_pos()) };
                }
                (State::Nothing, &'r', start_strpos, &'"', _4, _5, _6)
                | (State::Nothing, &'R', start_strpos, &'"', _4, _5, _6) => {           // C4: in nothing, meet r" or R"
                    self.v0.prepare_skip1();                   
                    state = State::InRawStringLiteral { parser: RawStringLiteralParser::new(start_strpos.start_pos()) };
                }
                (State::Nothing, &'\'', start_strpos, _3, _4, _5, _6) => {              // C5: in nothing, meet '
                    state = State::InCharLiteral{ parser: CharLiteralParser::new(start_strpos.start_pos()) };
                }
                (State::Nothing, &EOFCHAR, _2, _3, _4, _5, _6) => {                     // C7: in nothing, meet EOF, return 
                    return None;
                }
                (State::Nothing, ch, strpos, _3, _4, _5, _6) => {                       // C6: in nothing, meet other, return
                    return Some(V1Token::Other{ ch: *ch, pos: strpos.start_pos() });
                }

                (State::InBlockComment{ start_pos }, &'*', _2, &'/', _4, _5, _6) => {   // C8: in block, meet */, return
                    self.v0.prepare_skip1();
                    return Some(V1Token::Other{ ch: ' ', pos: start_pos });
                }
                (State::InBlockComment{ start_pos }, &EOFCHAR, eof_pos, _3, _4, _5, _6) => {  // C10: in block, meet EOF, emit error, return
                    messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                        (StringPosition::double(start_pos), error_strings::BlockCommentStartHere),
                        (eof_pos, error_strings::EOFHere),
                    ]));
                    return None;
                }
                (State::InBlockComment{ start_pos }, _1, _2, _3, _4, _5, _6) => {       // C9: in block, continue block
                    state = State::InBlockComment{ start_pos: start_pos };
                }
                (State::InLineComment, &'\n', lf_pos, _3, _4, _5, _6) => {              // C11: in line, meet \n, return
                    return Some(V1Token::Other { ch: '\n', pos: lf_pos.start_pos() });
                }
                (State::InLineComment, &EOFCHAR, _2, _3, _4, _5, _6) => {               // C13: in line, meet EOF, return
                    return None;
                }
                (State::InLineComment, _1, _2, _3, _4, _5, _6) => {                     // C12: in line, continue line
                    state = State::InLineComment;
                }

                (State::InStringLiteral{ mut parser }, ch, pos, next_ch, _4, _5, _6) => {   // Cx: anything inside "" is none about this module
                    match parser.input(*ch, pos.start_pos(), *next_ch, messages) {
                        StringLiteralParserResult::WantMore => {
                            state = State::InStringLiteral{ parser: parser };
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InStringLiteral{ parser: parser };
                        }
                        StringLiteralParserResult::Finished(value, pos) => {
                            return Some(V1Token::StringLiteral(value, pos));
                        }
                    }
                }
                (State::InRawStringLiteral{ mut parser }, ch, strpos, _3, _4, _5, _6) => {     // Cx, anything inside r"" is none about this module
                    match parser.input(*ch, strpos.start_pos(), messages) {
                        RawStringLiteralParserResult::WantMore => {
                            state = State::InRawStringLiteral{ parser: parser };
                        }
                        RawStringLiteralParserResult::Finished(value, pos) => {
                            return Some(V1Token::RawStringLiteral(value, pos));
                        }
                    }
                }
                (State::InCharLiteral{ mut parser }, ch, strpos, next_ch, _4, _5, _6) => {     // Cx: anything inside '' is none about this module
                    match parser.input(*ch, strpos.start_pos(), *next_ch, messages, dummy_coverage_recorder) {
                        CharLiteralParserResult::WantMore => {
                            state = State::InCharLiteral{ parser: parser };
                        },
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InCharLiteral{ parser: parser };
                        }
                        CharLiteralParserResult::Finished(value, pos) => {
                            return Some(V1Token::CharLiteral(value, pos));
                        }
                    }
                }
            }
        }
    }
}

#[allow(dead_code)] // don't know what rustc is thinking
pub type BufV1Token = BufToken<V1Token>;
pub type BufV1Lexer<'chs> = LegacyBufLexer<V1Lexer<'chs>, V1Token>;

#[cfg(test)]
#[test]
fn v1_base() {

    macro_rules! test_case {
        ($program: expr, [$($expect: expr)*] [$($expect_msg: expr)*]) => ({
            let messages = &mut MessageCollection::new();
            let mut v1lexer = V1Lexer::new($program.chars(), messages);
            $(
                match v1lexer.next(messages) {
                    Some(v1) => assert_eq!(v1, $expect),
                    None => panic!("Unexpect end of iteration"),
                }
            )*
            match v1lexer.next(messages) {
                Some(v1) => panic!("Unexpected more symbol after expect: {:?}", v1),
                None => (),
            }
            
            let expect_messages = &mut MessageCollection::new();
            $(
                expect_messages.push($expect_msg);
            )*
            assert_eq!(messages, expect_messages);
        });
        ($program: expr, [$($expect: expr)*]) => ({
            test_case!($program, [$($expect)*] [])
        });
    }
    macro_rules! other {
        ($ch: expr, $row: expr, $col: expr) => (V1Token::Other{ ch: $ch, pos: make_pos!($row, $col) })
    }
    macro_rules! ch {
        ($ch: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V1Token::CharLiteral(Some($ch), StringPosition::from4($row1, $col1, $row2, $col2))
        );
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V1Token::CharLiteral(None, StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! string {
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
            (V1Token::StringLiteral(None, StringPosition::from4($row1, $col1, $row2, $col2)));
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
            (V1Token::StringLiteral(Some($val.to_owned()), StringPosition::from4($row1, $col1, $row2, $col2)))
    }
    macro_rules! rstring {
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
            (V1Token::RawStringLiteral(None, StringPosition::from4($row1, $col1, $row2, $col2)));
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
            (V1Token::RawStringLiteral(Some($val.to_owned()), StringPosition::from4($row1, $col1, $row2, $col2)))
    }

    // Line comment as \n
    test_case!{ "ABC//DEF\n",           // C6, C1, C12, C11, C7
        [
            other!('A', 1, 1)
            other!('B', 1, 2)
            other!('C', 1, 3)
            other!('\n', 1, 9)
        ]
    }
    // Line comment EOF is not error
    test_case!{ "ABC//DEF",             // C6, C1, C12, C13
        [
            other!('A', 1, 1)
            other!('B', 1, 2)
            other!('C', 1, 3)
        ]
    }

    // Block comment is ' '
    test_case!{ "A/*D\nEF*/GH",         // C6, C2, C9, C8
        [
            other!('A', 1, 1)
            other!(' ', 1, 2)
            other!('G', 2, 5)
            other!('H', 2, 6)
        ]
    }
    // EOF in block comment is error
    test_case!{ "A/*BC",                // C6, C2, C9, C10
        [
            other!('A', 1, 1)
        ]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 2, 1, 2), error_strings::BlockCommentStartHere),
                (make_str_pos!(1, 6, 1, 6), error_strings::EOFHere),
            ])
        ]
    }

    // String literal test cases
    test_case!{ r#""Hello, world!""#,
        [
            string!("Hello, world!", 1, 1, 1, 15, false)
        ]
    }
    test_case!{ r#""He"#,
        [
            string!(1, 1, 1, 4, false)
        ]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 4, 1, 4), error_strings::EOFHere)
            ])
        ]
    }
    test_case!{ r#""He\"l\"lo"#,
        [
            string!(1, 1, 1, 11, false)
        ]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 11, 1, 11), error_strings::EOFHere),
                (make_str_pos!(1, 7, 1, 7), error_strings::LastEscapedQuoteHere),
            ])
        ]
    }
    test_case!{ r#""H\t\n\0\'\"llo""#,
        [
            string!("H\t\n\0'\"llo", 1, 1, 1, 16, false)
        ]
    }
    test_case!{ r#""h\c\d\e\n\g""#,
        [
            string!(1, 1, 1, 13, false)
        ]
        [
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere.to_owned()),
                (make_str_pos!(1, 3, 1, 3), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'd'), vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere.to_owned()),
                (make_str_pos!(1, 5, 1, 5), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'e'), vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere.to_owned()),
                (make_str_pos!(1, 7, 1, 7), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'g'), vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere.to_owned()),
                (make_str_pos!(1, 11, 1, 11), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
        ]
    }
    test_case!{ r#""H\uABCDel""#,
        [
            string!("H\u{ABCD}el", 1, 1, 1, 11, false)
        ]
    }
    test_case!{ r#""H\uABCHel\uABCg""#,
        [
            string!(1, 1, 1, 17, false)
        ]
        [
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (make_str_pos!(1, 3, 1, 3), error_strings::UnicodeCharEscapeStartHere),
                (make_str_pos!(1, 8, 1, 8), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (make_str_pos!(1, 11, 1, 11), error_strings::UnicodeCharEscapeStartHere),
                (make_str_pos!(1, 16, 1, 16), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]
    }
    test_case!{ r#""H\U0011ABCD""#,
        [string!(1, 1, 1, 13, false)]
        [
            Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (make_str_pos!(1, 3, 1, 3), error_strings::UnicodeCharEscapeStartHere.to_owned()),
            ], vec![
                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]
    }
    test_case!{ r#""H\u""#,
        [string!(1, 1, 1, 5, false)]
        [
            Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 3, 1, 3), error_strings::UnicodeCharEscapeStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::StringLiteralEndHere),
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]
    }
    test_case!{ r#""h\U123"#,
        [string!(1, 1, 1, 8, false)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 8, 1, 8), error_strings::EOFHere)
            ])
        ]
    }
    test_case!{ r#""he\"#,
        [string!(1, 1, 1, 5, false)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::EOFHere)
            ])
        ]
    }

    // Raw string literal test cases
    test_case!{ r#"r"hell\u\no""#,
        [rstring!(r"hell\u\no", 1, 1, 1, 12, true)]
    }
    test_case!{ r#"R"he"#,
        [rstring!(1, 1, 1, 5, true)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::StringLiteralStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::EOFHere)
            ])
        ]
    }

    // Char literal test cases
    test_case!{ "'A'",
        [ch!('A', 1, 1, 1, 3)]
    }
    test_case!{ r"'\t'", 
        [ch!('\t', 1, 1, 1, 4)]
    }
    test_case!{ r"'\uABCD'",
        [ch!('\u{ABCD}', 1, 1, 1, 8)]
    }
    test_case!{ "''",
        [ch!(1, 1, 1, 2)]
        [
            Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
            ], vec![
                error_strings::CharLiteralSyntaxHelp1
            ])
        ]
    }
    test_case!{ "'ABC'",
        [ch!(1, 1, 1, 5)]
        [
            Message::new_by_str(error_strings::CharLiteralTooLong, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::CharLiteralEndHere),
            ])
        ]
    }
    test_case!{ r"'\c'",
        [ch!(1, 1, 1, 4)]
        [
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere.to_owned()),
                (make_str_pos!(1, 2, 1, 2), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
        ]
    }
    test_case!{ r"'\uBG'",
        [ch!(1, 1, 1, 6)]
        [
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (make_str_pos!(1, 2, 1, 2), error_strings::UnicodeCharEscapeStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
            Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 6, 1, 6), error_strings::CharLiteralEndHere)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax
            ])
        ]
    }
    test_case!{ r"'\U0011ABCD'",
        [ch!(1, 1, 1, 12)]
        [
            Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (make_str_pos!(1, 2, 1, 2), error_strings::UnicodeCharEscapeStartHere.to_owned()),
            ], vec![
                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]
    }
    test_case!{ r"'\na'",
        [ch!(1, 1, 1, 5)]
        [
            Message::new_by_str(error_strings::CharLiteralTooLong, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::CharLiteralEndHere),
            ])
        ]
    }
    test_case!{ r"'\uABCDA'",
        [ch!(1, 1, 1, 9)]
        [
            Message::new_by_str(error_strings::CharLiteralTooLong, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 9, 1, 9), error_strings::CharLiteralEndHere),
            ])
        ] 
    }
    test_case!{ "'",
        [ch!(1, 1, 1, 2)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 2, 1, 2), error_strings::EOFHere)
            ])
        ]
    }
    test_case!{ r"'\",
        [ch!(1, 1, 1, 3)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 3, 1, 3), error_strings::EOFHere)
            ])
        ]
    }
    test_case!{ r"'\u",
        [ch!(1, 1, 1, 4)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 4, 1, 4), error_strings::EOFHere)
            ])
        ]
    }
    test_case! { r"'A",
        [ch!(1, 1, 1, 3)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 3, 1, 3), error_strings::EOFHere)
            ])
        ]
    }
    test_case! { "'ABC",
        [ch!(1, 1, 1, 5)]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 5, 1, 5), error_strings::EOFHere)
            ])
        ]
    }
    test_case!{ r"'\'AB",
        [
            ch!(1, 1, 1, 6)
        ]
        [
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (make_str_pos!(1, 1, 1, 1), error_strings::CharLiteralStartHere),
                (make_str_pos!(1, 6, 1, 6), error_strings::EOFHere),
            ])
        ]
    }
}