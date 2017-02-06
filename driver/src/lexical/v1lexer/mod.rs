
///! Level1 parser
///! input v0
///! remove line comment
///! report block comment as OtherChar ' '
///! find string literal with only '"' escaped
///! string literal is allowed to cross line, line end is regarded as \n
///! raw string literal supported, `r'C:\\abc'` or `R"C:\\abc"`

mod escape_char_parser;
mod char_lit_parser;
mod string_lit_parser;
mod raw_string_lit_parser;

use std::str::Chars;
use codepos::Position;
use message::LexicalMessage as Message;
use message::MessageEmitter;

use lexical::v0lexer::V0Token;
use lexical::v0lexer::BufV0Token;
use lexical::v0lexer::BufV0Lexer;

use lexical::buf_lexer::IDetailLexer;
use lexical::buf_lexer::BufToken;
use lexical::buf_lexer::BufLexer;

use lexical::symbol_type::char_literal::CharLiteral;
use lexical::symbol_type::string_literal::StringLiteral;

use lexical::v1lexer::string_lit_parser::StringLiteralParser;
use lexical::v1lexer::string_lit_parser::StringLiteralParserResult;
use lexical::v1lexer::raw_string_lit_parser::RawStringLiteralParser;
use lexical::v1lexer::raw_string_lit_parser::RawStringLiteralParserResult;
use lexical::v1lexer::char_lit_parser::CharLiteralParser;
use lexical::v1lexer::char_lit_parser::CoverageRecorder;
use lexical::v1lexer::char_lit_parser::CharLiteralParserResult;

#[cfg(test)]
#[derive(Clone, Eq, PartialEq)]
pub enum V1Token {
    StringLiteral { inner: StringLiteral },
    CharLiteral { inner: CharLiteral },
    Other { ch: char, pos: Position },
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum V1Token {
    StringLiteral { inner: StringLiteral },
    CharLiteral { inner: CharLiteral },
    Other { ch: char, pos: Position },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V1Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V1Token::StringLiteral { ref inner} => {
                write!(f, "{:?}", inner)
            }
            V1Token::CharLiteral{ ref inner } => {
                write!(f, "{:?}", inner)
            }
            V1Token::Other { ref ch, ref pos } => {
                write!(f, "Other {:?} at {:?}", ch, pos)
            }
        }
    }
}

pub struct V1Lexer<'chs> {
    v0: BufV0Lexer<'chs>,
}
impl<'chs> From<Chars<'chs>> for V1Lexer<'chs> {
    fn from(content_chars: Chars<'chs>) -> V1Lexer {
        V1Lexer { 
            v0: BufV0Lexer::from(content_chars),
        }
    }
}

impl<'chs> IDetailLexer<'chs, V1Token> for V1Lexer<'chs> {
    
    fn position(&self) -> Position { self.v0.inner().position() }

    // input v0, output stringliteral or otherchar without comment
    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V1Token> {
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
            let bufv0 = self.v0.next(messages);
            match state {
                State::Nothing => {
                    match bufv0 {
                        Some(BufV0Token{ token: V0Token{ ch: '/', pos: _1 }, next: Some(V0Token{ ch: '/', pos: _2 }) }) => {
                            self.v0.skip1(messages);
                            state = State::InLineComment;                                       // C1: in nothing, meet //
                        }
                        Some(BufV0Token{ token: V0Token{ ch: '/', pos }, next: Some(V0Token{ ch: '*', pos: _1 }) }) => {
                            state = State::InBlockComment { start_pos: pos };                   // C2: in nothing, meet /*
                        }
                        Some(BufV0Token{ token: V0Token{ ch: '"', pos }, next: _1 }) => {       // C3: in nothing, meet "
                            state = State::InStringLiteral { parser: StringLiteralParser::new(pos) };
                        }
                        Some(BufV0Token { token: V0Token { ch: 'r', pos }, next: Some(V0Token { ch: '"', pos: _1 }) })
                            | Some(BufV0Token { token: V0Token { ch: 'R', pos }, next: Some(V0Token { ch: '"', pos: _1 }) }) => {
                            self.v0.skip1(messages);                                            // C4: in nothing, meet r" or R"
                            state = State::InRawStringLiteral { parser: RawStringLiteralParser::new(pos) };
                        }
                        Some(BufV0Token{ token: V0Token{ ch: '\'', pos }, next: _1 }) => {      // C5: in nothing, meet '
                            state = State::InCharLiteral{ parser: CharLiteralParser::new(pos) };
                        }
                        Some(BufV0Token{ token: V0Token{ ch, pos }, next: _1 }) => {
                            return Some(V1Token::Other{ ch: ch, pos: pos });                    // C6: in nothing, meet other, return
                        }
                        None => { return None; }                                                // C7: in nothing, meet EOF, return 
                    }
                }
                State::InBlockComment { ref start_pos } => {
                    match bufv0 {
                        Some(BufV0Token{ token: V0Token { ch: '*', pos: _1 }, next: Some(V0Token{ ch: '/', pos: _2 }) }) => {
                            self.v0.skip1(messages);
                            return Some(V1Token::Other{ ch: ' ', pos: *start_pos });            // C8: in block, meet */, return
                        }
                        Some(_) => {
                            // state = State::InBlockComment{ start_pos: start_pos };           // C9: in block, continue block
                        }
                        None => {
                            messages.push(Message::UnexpectedEndofFileInBlockComment { block_start: *start_pos, eof_pos: self.position() });
                            return None;                                                        // C10: in block, meet EOF, emit error, return
                        }
                    }
                }
                State::InLineComment => {
                    match bufv0 {
                        Some(BufV0Token{ token: V0Token { ch: '\n', pos }, next: _1 }) => {
                            return Some(V1Token::Other { ch: '\n', pos: pos });                 // C11: in line, meet \n, return
                        }
                        Some(_) => {
                            // state = State::InLineComment;                                    // C12: in line, continue line
                        }
                        None => {
                            return None;                                                        // C13: in line, meet EOF, return
                        }
                    }
                }
                State::InStringLiteral { ref mut parser } => {
                    match match bufv0 {
                        Some(BufV0Token{ token: V0Token{ ch, pos }, next: Some(V0Token{ ch: next_ch, pos: _1 }) }) => {
                            parser.input(Some(ch), pos, Some(next_ch), messages)
                        }
                        Some(BufV0Token{ token: V0Token { ch, pos }, next: None }) => {        // Cx: anything inside "" is none about this module
                            parser.input(Some(ch), pos, None, messages)
                        }
                        None => {
                            parser.input(None, self.position(), None, messages)
                        }
                    } {
                        StringLiteralParserResult::WantMore => (), // continue
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.skip1(messages);
                        }
                        StringLiteralParserResult::Finished(literal) => {
                            return Some(V1Token::StringLiteral{ inner: literal });
                        }
                    }
                }
                State::InRawStringLiteral { ref mut parser } => {
                    match match bufv0 {
                        Some(BufV0Token{ token: V0Token { ch, pos }, next: _2 }) => {          // Cx, anything inside r"" is none about this module
                            parser.input(Some(ch), pos, messages)
                        }
                        None => {
                            parser.input(None, self.position(), messages)
                        }
                    } {
                        RawStringLiteralParserResult::WantMore => (),
                        RawStringLiteralParserResult::Finished(literal) => {
                            return Some(V1Token::StringLiteral{ inner: literal });
                        }
                    }
                }
                State::InCharLiteral { ref mut parser } => {
                    match match bufv0 {
                        Some(BufV0Token{ token: V0Token{ ch, pos }, next: Some(V0Token{ ch: next_ch, pos: _1 }) }) => {
                            parser.input(Some(ch), pos, Some(next_ch), messages, dummy_coverage_recorder)
                        }
                        Some(BufV0Token{ token: V0Token { ch, pos }, next: None }) => {        // Cx: anything inside '' is none about this module
                            parser.input(Some(ch), pos, None, messages, dummy_coverage_recorder)
                        }
                        None => {
                            parser.input(None, self.position(), None, messages, dummy_coverage_recorder)
                        }
                    } {
                        CharLiteralParserResult::WantMore => (), // continue
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.skip1(messages);
                        }
                        CharLiteralParserResult::Finished(literal) => {
                            return Some(V1Token::CharLiteral{ inner: literal });
                        }
                    }
                }
            }
        }
    }
}

pub type BufV1Token = BufToken<V1Token>;
pub type BufV1Lexer<'chs> = BufLexer<V1Lexer<'chs>, V1Token>;

#[cfg(test)]
mod tests {
    use super::V1Token;
    use super::V1Lexer;
    use codepos::Position;
    use codepos::StringPosition;
    use message::LexicalMessage as Message;
    use message::MessageEmitter;
    use lexical::buf_lexer::IDetailLexer;
    use lexical::symbol_type::string_literal::StringLiteral;
    use lexical::symbol_type::char_literal::CharLiteral;

    #[test]
    fn v1_base() {

        macro_rules! test_case {
            ($program: expr, [$($expect: expr)*] [$($expect_msg: expr)*]) => ({
                let mut v1lexer = V1Lexer::from($program.chars());
                let messages = &mut MessageEmitter::new();
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
                
                let expect_messages = &mut MessageEmitter::new();
                $(
                    expect_messages.push($expect_msg);
                )*
                assert_eq!(messages, expect_messages);
            });
            ($program: expr, [$($expect: expr)*]) => ({
                test_case!($program, [$($expect)*] [])
            });
        }
        macro_rules! is_o {
            ($ch: expr, $row: expr, $col: expr) => (V1Token::Other{ ch: $ch, pos: make_pos!($row, $col) })
        }
        macro_rules! is_char {
            ($ch: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
                V1Token::CharLiteral{ inner: CharLiteral{ value: Some($ch), pos: StringPosition::from4($row1, $col1, $row2, $col2) } }
            );
            ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
                V1Token::CharLiteral{ inner: CharLiteral{ value: None, pos: StringPosition::from4($row1, $col1, $row2, $col2) } }
            )
        }
        macro_rules! is_string {
            ($row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
                (V1Token::StringLiteral { inner: StringLiteral::new(None, StringPosition::from4($row1, $col1, $row2, $col2), $is_raw) });
            ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr) => 
                (V1Token::StringLiteral { inner: StringLiteral::new2($val, StringPosition::from4($row1, $col1, $row2, $col2), $is_raw) })
        }

        // Line comment as \n
        test_case!{ "ABC//DEF\n",           // C6, C1, C12, C11, C7
            [
                is_o!('A', 1, 1)
                is_o!('B', 1, 2)
                is_o!('C', 1, 3)
                is_o!('\n', 1, 9)
            ]
        }
        // Line comment EOF is not error
        test_case!{ "ABC//DEF",             // C6, C1, C12, C13
            [
                is_o!('A', 1, 1)
                is_o!('B', 1, 2)
                is_o!('C', 1, 3)
            ]
        }

        // Block comment is ' '
        test_case!{ "A/*D\nEF*/GH",         // C6, C2, C9, C8
            [
                is_o!('A', 1, 1)
                is_o!(' ', 1, 2)
                is_o!('G', 2, 5)
                is_o!('H', 2, 6)
            ]
        }
        // EOF in block comment is error
        test_case!{ "A/*BC",                // C6, C2, C9, C10
            [
                is_o!('A', 1, 1)
            ]
            [
                Message::UnexpectedEndofFileInBlockComment{
                    block_start: make_pos!(1, 2),
                    eof_pos: make_pos!(1, 6),
                }
            ]
        }

        // String literal test cases
        test_case!{ r#""Hello, world!""#,
            [
                is_string!("Hello, world!", 1, 1, 1, 15, false)
            ]
        }
        test_case!{ r#""He"#,
            [
                is_string!(1, 1, 1, 4, false)
            ]
            [
                Message::UnexpectedEndofFileInStringLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 4), 
                    hint_escaped_quote_pos: None,
                }
            ]
        }
        test_case!{ r#""He\"l\"lo"#,
            [
                is_string!(1, 1, 1, 11, false)
            ]
            [
                Message::UnexpectedEndofFileInStringLiteral{
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 11), 
                    hint_escaped_quote_pos: Some(make_pos!(1, 7)),
                }
            ]
        }
        test_case!{ r#""H\t\n\0\'\"llo""#,
            [
                is_string!("H\t\n\0'\"llo", 1, 1, 1, 16, false)
            ]
        }
        test_case!{ r#""h\c\d\e\n\g""#,
            [
                is_string!(1, 1, 1, 13, false)
            ]
            [
                Message::UnrecognizedEscapeCharInStringLiteral{ 
                    literal_start: make_pos!(1, 1), unrecogonize_pos: make_pos!(1, 3), unrecogonize_escape: 'c' }
                Message::UnrecognizedEscapeCharInStringLiteral{ 
                    literal_start: make_pos!(1, 1), unrecogonize_pos: make_pos!(1, 5), unrecogonize_escape: 'd' }
                Message::UnrecognizedEscapeCharInStringLiteral{ 
                    literal_start: make_pos!(1, 1), unrecogonize_pos: make_pos!(1, 7), unrecogonize_escape: 'e' }
                Message::UnrecognizedEscapeCharInStringLiteral{ 
                    literal_start: make_pos!(1, 1), unrecogonize_pos: make_pos!(1, 11), unrecogonize_escape: 'g' }
            ]
        }
        test_case!{ r#""H\uABCDel""#,
            [
                is_string!("H\u{ABCD}el", 1, 1, 1, 11, false)
            ]
        }
        test_case!{ r#""H\uABCHel\uABCg""#,
            [
                is_string!(1, 1, 1, 17, false)
            ]
            [
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: make_pos!(1, 3), unexpected_char_pos: make_pos!(1, 8), unexpected_char: 'H' }
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: make_pos!(1, 11), unexpected_char_pos: make_pos!(1, 16), unexpected_char: 'g' }
            ]
        }
        test_case!{ r#""H\U0011ABCD""#,
            [is_string!(1, 1, 1, 13, false)]
            [
                Message::IncorrectUnicodeCharEscapeValue{ 
                    escape_start: make_pos!(1, 3), 
                    raw_value: "0011ABCD".to_owned() 
                }
            ]
        }
        test_case!{ r#""H\u""#,
            [is_string!(1, 1, 1, 5, false)]
            [
                Message::UnexpectedStringLiteralEndInUnicodeCharEscape{
                    literal_start: make_pos!(1, 1), 
                    escape_start: make_pos!(1, 3), 
                    unexpected_end_pos: make_pos!(1, 5),
                }
            ]
        }
        test_case!{ r#""h\U123"#,
            [is_string!(1, 1, 1, 8, false)]
            [
                Message::UnexpectedEndofFileInStringLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 8), 
                    hint_escaped_quote_pos: None,
                }
            ]
        }
        test_case!{ r#""he\"#,
            [is_string!(1, 1, 1, 5, false)]
            [
                Message::UnexpectedEndofFileInStringLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 5), 
                    hint_escaped_quote_pos: None,
                }
            ]
        }

        // Raw string literal test cases
        test_case!{ r#"r"hell\u\no""#,
            [is_string!(r"hell\u\no", 1, 1, 1, 12, true)]
        }
        test_case!{ r#"R"he"#,
            [is_string!(1, 1, 1, 5, true)]
            [
                Message::UnexpectedEndofFileInStringLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 5), 
                    hint_escaped_quote_pos: None,
                }
            ]
        }

        // Char literal test cases
        test_case!{ "'A'",
            [is_char!('A', 1, 1, 1, 3)]
        }
        test_case!{ r"'\t'", 
            [is_char!('\t', 1, 1, 1, 4)]
        }
        test_case!{ r"'\uABCD'",
            [is_char!('\u{ABCD}', 1, 1, 1, 8)]
        }
        test_case!{ "''",
            [is_char!(1, 1, 1, 2)]
            [Message::EmptyCharLiteral{ pos: make_pos!(1, 1) }]
        }
        test_case!{ "'ABC'",
            [is_char!(1, 1, 1, 5)]
            [Message::CharLiteralTooLong{ start_pos: make_pos!(1, 1) }]
        }
        test_case!{ r"'\c'",
            [is_char!(1, 1, 1, 4)]
            [
                Message::UnrecognizedEscapeCharInCharLiteral{
                    literal_start: make_pos!(1, 1),
                    unrecogonize_pos: make_pos!(1, 2),
                    unrecogonize_escape: 'c',
                }
            ]
        }
        test_case!{ r"'\uBG'",
            [is_char!(1, 1, 1, 6)]
            [
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: make_pos!(1, 2),
                    unexpected_char_pos: make_pos!(1, 5),
                    unexpected_char: 'G' }
                Message::UnexpectedCharLiteralEndInUnicodeCharEscape {
                    literal_start: make_pos!(1, 1),
                    escape_start: make_pos!(1, 2),
                    unexpected_end_pos: make_pos!(1, 6) }
            ]
        }
        test_case!{ r"'\U0011ABCD'",
            [is_char!(1, 1, 1, 12)]
            [
                Message::IncorrectUnicodeCharEscapeValue{
                    escape_start: make_pos!(1, 2),
                    raw_value: "0011ABCD".to_owned()
                }
            ]
        }
        test_case!{ r"'\na'",
            [is_char!(1, 1, 1, 5)]
            [Message::CharLiteralTooLong{ start_pos: make_pos!(1, 1) }]
        }
        test_case!{ r"'\uABCDA'",
            [is_char!(1, 1, 1, 9)]
            [Message::CharLiteralTooLong{ start_pos: make_pos!(1, 1) }] 
        }
        test_case!{ "'",
            [is_char!(1, 1, 1, 2)]
            [
                Message::UnexpectedEndofFileInCharLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 2), 
                }
            ]
        }
        test_case!{ r"'\",
            [is_char!(1, 1, 1, 3)]
            [
                Message::UnexpectedEndofFileInCharLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 3), 
                }
            ]
        }
        test_case!{ r"'\u",
            [is_char!(1, 1, 1, 4)]
            [
                Message::UnexpectedEndofFileInCharLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 4), 
                }
            ]
        }
        test_case! { r"'A",
            [is_char!(1, 1, 1, 3)]
            [
                Message::UnexpectedEndofFileInCharLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 3), 
                }
            ]
        }
        test_case! { "'ABC",
            [is_char!(1, 1, 1, 5)]
            [
                Message::UnexpectedEndofFileInCharLiteral{ 
                    literal_start: make_pos!(1, 1), 
                    eof_pos: make_pos!(1, 5) 
                }
            ]
        }
        test_case!{ r"'\'AB",
            [
                is_char!(1, 1, 1, 3)
                is_o!('A', 1, 4)
                is_o!('B', 1, 5)
            ]
            [Message::InvalidEscapeInCharLiteral{ start_pos: make_pos!(1, 1) }]
        }
    }
}