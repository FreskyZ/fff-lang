///! fff-lang
///!
///! lexical/v1, input v0
///! remove line comment
///! report block comment as OtherChar ' '
///! find string literal with only '"' escaped
///! string literal is allowed to cross line, line end is regarded as \n
///! raw string literal supported, `r'C:\\abc'` or `R"C:\\abc"`

mod char_lit_parser;
mod error_strings;
mod escape_char_parser;
mod raw_string_lit_parser;
mod string_lit_parser;

use codemap::Span;
use codemap::CodeChars;
use codemap::EOFCHAR;
use codemap::EOFSCHAR;
use message::Message;
use message::MessageCollection;

use super::buf_lexer::ILexer;
use super::buf_lexer::BufLexer;

use self::string_lit_parser::StringLiteralParser;
use self::string_lit_parser::StringLiteralParserResult;
use self::raw_string_lit_parser::RawStringLiteralParser;
use self::raw_string_lit_parser::RawStringLiteralParserResult;
use self::char_lit_parser::CharLiteralParser;
use self::char_lit_parser::CharLiteralParserResult;

// CodeChars wrapper
struct V0Lexer<'a>(CodeChars<'a>);
impl<'a> ILexer<'a, char> for V0Lexer<'a> {
    fn new(chars: CodeChars<'a>, _: &mut MessageCollection) -> V0Lexer<'a> { V0Lexer(chars) }
    fn next(&mut self, _: &mut MessageCollection) -> (char, Span) {
        let ret_val = self.0.next();
        (ret_val.0, ret_val.1.double())
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum V1Token {
    StringLiteral(Option<String>),
    RawStringLiteral(Option<String>),
    CharLiteral(Option<char>),
    Other(char),
    EOF,
    EOFs,
}

pub struct V1Lexer<'chs> {
    v0: BufLexer<V0Lexer<'chs>, char>,
}
impl<'chs> ILexer<'chs, V1Token> for V1Lexer<'chs> {

    fn new(content_chars: CodeChars<'chs>, messages: &mut MessageCollection) -> V1Lexer<'chs> {
        V1Lexer { 
            v0: BufLexer::new(content_chars, messages),
        }
    }

    // input v0, output stringliteral or otherchar without comment
    fn next(&mut self, messages: &mut MessageCollection) -> (V1Token, Span) {
        // First there is quote, and anything inside is regarded as string literal, include `\n` as real `\n`
        // and then outside of quote pair there is comments, anything inside comment, // and /n, or /* and */ is regarded as comment
        
        enum State {
            Nothing,
            InStringLiteral { parser: StringLiteralParser },
            InRawStringLiteral { parser: RawStringLiteralParser },
            InLineComment,
            InBlockComment { start_span: Span },
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
                (State::Nothing, &'/', start_span, &'*', _4, _5, _6) => {             // C2: in nothing, meet /*
                    state = State::InBlockComment{ start_span };                  
                }
                (State::Nothing, &'"', start_span, _3, _4, _5, _6) => {               // C3: in nothing, meet "
                    state = State::InStringLiteral { parser: StringLiteralParser::new(start_span.get_start_pos()) };
                }
                (State::Nothing, &'r', start_span, &'"', _4, _5, _6)
                | (State::Nothing, &'R', start_span, &'"', _4, _5, _6) => {           // C4: in nothing, meet r" or R"
                    self.v0.prepare_skip1();                   
                    state = State::InRawStringLiteral { parser: RawStringLiteralParser::new(start_span.get_start_pos()) };
                }
                (State::Nothing, &'\'', start_span, _3, _4, _5, _6) => {              // C5: in nothing, meet '
                    state = State::InCharLiteral{ parser: CharLiteralParser::new(start_span.get_start_pos()) };
                }
                (State::Nothing, &EOFCHAR, eof_pos, _3, _4, _5, _6) => {                // C7: in nothing, meet EOF, return 
                    return (V1Token::EOF, eof_pos);
                }
                (State::Nothing, &EOFSCHAR, eofs_pos, _3, _4, _5, _6) => {              // C13, directly redirect EOFs
                    return (V1Token::EOFs, eofs_pos);
                }
                (State::Nothing, ch, strpos, _3, _4, _5, _6) => {                       // C6: in nothing, meet other, return
                    return (V1Token::Other(*ch), strpos);
                }
                (State::InBlockComment{ start_span }, &'*', _2, &'/', end_pos, _5, _6) => {   // C8: in block, meet */, return
                    self.v0.prepare_skip1();
                    return (V1Token::Other(' '), start_span.merge(&end_pos));
                }
                (State::InBlockComment{ start_span }, &EOFCHAR, eof_pos, _3, _4, _5, _6) => {  // C10: in block, meet EOF, emit error, return
                    messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                        (start_span, error_strings::BlockCommentStartHere),
                        (eof_pos, error_strings::EOFHere),
                    ]));
                    return (V1Token::EOF, eof_pos);
                }
                (State::InBlockComment{ start_span }, _1, _2, _3, _4, _5, _6) => {       // C9: in block, continue block
                    state = State::InBlockComment{ start_span };
                }
                (State::InLineComment, &'\n', lf_pos, _3, _4, _5, _6) => {              // C11: in line, meet \n, return
                    return (V1Token::Other('\n'), lf_pos);
                }
                (State::InLineComment, &EOFCHAR, eof_pos, _3, _4, _5, _6) => {          // C13: in line, meet EOF, return
                    return (V1Token::EOF, eof_pos);
                }
                (State::InLineComment, _1, _2, _3, _4, _5, _6) => {                     // C12: in line, continue line
                    state = State::InLineComment;
                }

                (State::InStringLiteral{ mut parser }, ch, pos, next_ch, _4, _5, _6) => {   // Cx: anything inside "" is none about this module
                    match parser.input(*ch, pos.get_start_pos(), *next_ch, messages) {
                        StringLiteralParserResult::WantMore => {
                            state = State::InStringLiteral{ parser: parser };
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InStringLiteral{ parser: parser };
                        }
                        StringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOFCHAR { // if EOFCHAR was consumed by str lit parser, it will not be returned as EOF, which is not designed by feature
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::StringLiteral(value), pos);
                        }
                    }
                }
                (State::InRawStringLiteral{ mut parser }, ch, strpos, _3, _4, _5, _6) => {     // Cx, anything inside r"" is none about this module
                    match parser.input(*ch, strpos.get_start_pos(), messages) {
                        RawStringLiteralParserResult::WantMore => {
                            state = State::InRawStringLiteral{ parser: parser };
                        }
                        RawStringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOFCHAR { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::RawStringLiteral(value), pos);
                        }
                    }
                }
                (State::InCharLiteral{ mut parser }, ch, strpos, next_ch, _4, _5, _6) => {     // Cx: anything inside '' is none about this module
                    match parser.input(*ch, strpos.get_start_pos(), *next_ch, messages) {
                        CharLiteralParserResult::WantMore => {
                            state = State::InCharLiteral{ parser: parser };
                        },
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InCharLiteral{ parser: parser };
                        }
                        CharLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOFCHAR { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::CharLiteral(value), pos);
                        }
                    }
                }
            }
        }
    }
}

#[cfg(test)]
#[test]
fn v1_base() {
    use codemap::CodeMap;

    macro_rules! test_case {
        ($program: expr, [$($expect: expr)*], $expect_msgs: expr) => ({
            println!("Case {} at {}:", $program, line!());
            let messages = &mut MessageCollection::new();
            let codemap = CodeMap::with_test_str($program);
            let mut v1lexer = V1Lexer::new(codemap.iter(), messages);
            $(
                match v1lexer.next(messages) {
                    v1 => assert_eq!(v1, $expect),
                }
            )*

            let next = v1lexer.next(messages);
            if next.0 != V1Token::EOF {
                panic!("next is not EOF but {:?}", next);
            }
            if v1lexer.next(messages).0 != V1Token::EOFs {
                panic!("next next is not EOFs");
            }
            match v1lexer.next(messages) {
                (V1Token::EOFs, _) => (),
                v1 => panic!("Unexpected more symbol after eofs: {:?}", v1),
            }

            assert_eq!(messages, &$expect_msgs);
            println!("");
        });
        ($program: expr, [$($expect: expr)*]) => ({
            test_case!($program, [$($expect)*], make_messages![])
        });
    }
    macro_rules! other {
        ($ch: expr, $char_id: expr) => ((V1Token::Other($ch), make_span!($char_id, $char_id)));
        ($ch: expr, $start_id: expr, $end_id: expr) => ((V1Token::Other($ch), make_span!($start_id, $end_id)))
    }
    macro_rules! ch {
        ($ch: expr, $start_id: expr, $end_id: expr) => 
            ((V1Token::CharLiteral(Some($ch)), make_span!($start_id, $end_id)));
        ($start_id: expr, $end_id: expr) =>
            ((V1Token::CharLiteral(None), make_span!($start_id, $end_id)))
    }
    macro_rules! string {
        ($start_id: expr, $end_id: expr) => 
            ((V1Token::StringLiteral(None), make_span!($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => 
            ((V1Token::StringLiteral(Some($val.to_owned())), make_span!($start_id, $end_id)))
    }
    macro_rules! rstring {
        ($start_id: expr, $end_id: expr) => 
            ((V1Token::RawStringLiteral(None), make_span!($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => 
            ((V1Token::RawStringLiteral(Some($val.to_owned())), make_span!($start_id, $end_id)))
    }

    // Line comment as \n
    test_case!{ "ABC//DEF\n", [         // C6, C1, C12, C11, C7
        other!('A', 0)
        other!('B', 1)
        other!('C', 2)
        other!('\n', 8)
    ]}
    // Line comment EOF is not error
    test_case!{ "ABC//DEF", [           // C6, C1, C12, C13
        other!('A', 0)
        other!('B', 1)
        other!('C', 2)
    ]}

    // Block comment is ' ', but has correct position
    //           01234 567890
    test_case!{ "A/*D\nEF*/GH", [       // C6, C2, C9, C8
        other!('A', 0)
        other!(' ', 1, 8)
        other!('G', 9)
        other!('H', 10)
    ]}
    // EOF in block comment is error
    test_case!{ "A/*BC", [              // C6, C2, C9, C10
        other!('A', 0)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(1, 1), error_strings::BlockCommentStartHere),
            (make_span!(5, 5), error_strings::EOFHere),
        ])
    ]}

    // String literal test cases
    test_case!{ r#""Hello, world!""#, [
        string!("Hello, world!", 0, 14)
    ]}
    test_case!{ r#""He"#, [
        string!(0, 3)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(3, 3), error_strings::EOFHere)
        ])
    ]}
    test_case!{ r#""He\"l\"lo"#, [
        string!(0, 10)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(10, 10), error_strings::EOFHere),
            (make_span!(6, 6), error_strings::LastEscapedQuoteHere),
        ])
    ]}
    test_case!{ r#""H\t\n\0\'\"llo""#, [
        string!("H\t\n\0'\"llo", 0, 15)
    ]}
    test_case!{ r#""h\c\d\e\n\g""#, [
        string!(0, 12)
    ], make_messages![
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere.to_owned()),
            (make_span!(2, 2), error_strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'd'), vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere.to_owned()),
            (make_span!(4, 4), error_strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'e'), vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere.to_owned()),
            (make_span!(6, 6), error_strings::UnknownCharEscapeHere.to_owned()),
        ]),
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'g'), vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere.to_owned()),
            (make_span!(10, 10), error_strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]}
    test_case!{ r#""H\uABCDel""#, [
        string!("H\u{ABCD}el", 0, 10)
    ]} //          01234567890123456
    test_case!{ r#""H\uABCHel\uABgC""#, [
        string!(0, 16)
    ], make_messages![
        Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (make_span!(2, 2), error_strings::UnicodeCharEscapeStartHere),
            (make_span!(7, 7), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]),
        Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (make_span!(10, 10), error_strings::UnicodeCharEscapeStartHere),
            (make_span!(14, 14), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]} //          0123456789012
    test_case!{ r#""H\U0011ABCD""#, [
        string!(0, 12)
    ], make_messages![
        Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (make_span!(2, 11), error_strings::UnicodeCharEscapeHere.to_owned()),
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}
    test_case!{ r#""H\u""#, [
        string!(0, 4)
    ], make_messages![
        Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(2, 2), error_strings::UnicodeCharEscapeStartHere),
            (make_span!(4, 4), error_strings::StringLiteralEndHere),
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]}
    test_case!{ r#""h\U123"#, [
        string!(0, 7)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(7, 7), error_strings::EOFHere)
        ])
    ]}
    test_case!{ r#""he\"#, [
        string!(0, 4)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}

    // Raw string literal test cases
    test_case!{ r#"r"hell\u\no""#, [
        rstring!(r"hell\u\no", 0, 11)
    ]}
    test_case!{ r#"R"he"#, [
        rstring!(0, 4)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}

    // Char literal test cases
    test_case!{ "'A'", [
        ch!('A', 0, 2)
    ]}
    test_case!{ r"'\t'", [
        ch!('\t', 0, 3)
    ]}
    test_case!{ r"'\uABCD'", [
        ch!('\u{ABCD}', 0, 7)
    ]}
    test_case!{ "''", [
        ch!(0, 1)
    ], make_messages![
        Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere), 
        ], vec![
            error_strings::CharLiteralSyntaxHelp1
        ])
    ]}
    test_case!{ "'ABC'", [
        ch!(0, 4)
    ], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),
        ])
    ]}
    test_case!{ r"'\c'", [
        ch!(0, 3)
    ], make_messages![
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (make_span!(0, 0), error_strings::CharLiteralStartHere.to_owned()), 
            (make_span!(1, 1), error_strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]} //         012345
    test_case!{ r"'\uBG'", [
        ch!(0, 5)
    ], make_messages![
        Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (make_span!(1, 1), error_strings::UnicodeCharEscapeStartHere), 
            (make_span!(4, 4), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]),
        Message::with_help_by_str(error_strings::UnexpectedCharLiteralEnd, vec![
            (make_span!(0, 5), error_strings::CharLiteralHere),
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax
        ])
    ]}
    test_case!{ r"'\U0011ABCD'", [
        ch!(0, 11)
    ], make_messages![
        Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (make_span!(1, 10), error_strings::UnicodeCharEscapeHere.to_owned()), 
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]} //         01234
    test_case!{ r"'\na'", [
        ch!(0, 4)
    ], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),   
        ])
    ]} //         012345678
    test_case!{ r"'\uABCDA'", [
        ch!(0, 8)
    ], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 8), error_strings::CharLiteralHere),      
        ])
    ]}
    test_case!{ "'", [
        ch!(0, 0)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::CharLiteralHere),    
            (make_span!(1, 1), error_strings::EOFHere)
        ])
    ]}
    test_case!{ r"'\", [
        ch!(0, 1)                                                    
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere),      
            (make_span!(2, 2), error_strings::EOFHere)
        ])
    ]}
    test_case!{ r"'\u", [
        ch!(0, 2)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 2), error_strings::CharLiteralHere),
            (make_span!(3, 3), error_strings::EOFHere)
        ])
    ]}
    test_case! { r"'A", [
        ch!(0, 1)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere),
            (make_span!(2, 2), error_strings::EOFHere)
        ])
    ]}
    test_case! { "'ABC", [
        ch!(0, 3)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 3), error_strings::CharLiteralHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}
    test_case!{ r"'\'AB", [
        ch!(0, 4)
    ], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),
            (make_span!(5, 5), error_strings::EOFHere),
        ])
    ]}
}