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

use crate::source::{Span, SourceCodeIter, EOF_CHAR};
use crate::diagnostics::Message;
use super::{ILexer, BufLexer, StrLitValue, ParseSession};
use string_lit_parser::{StringLiteralParser, StringLiteralParserResult};
use raw_string_lit_parser::{RawStringLiteralParser, RawStringLiteralParserResult};
use char_lit_parser::{CharLiteralParser, CharLiteralParserResult};

// SourceCodeIter wrapper
struct V0Lexer<'a>(SourceCodeIter<'a>);
impl<'a> ILexer<'a, char> for V0Lexer<'a> {
    fn new(source: SourceCodeIter<'a>) -> V0Lexer<'a> { V0Lexer(source) }
    fn next(&mut self, _: &mut ParseSession) -> (char, Span) {
        let ret_val = self.0.next();
        (ret_val.0, ret_val.1.as_span())
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum V1Token {
    EOF,
    StringLiteral(Option<StrLitValue>),
    RawStringLiteral(Option<StrLitValue>),
    CharLiteral(Option<char>),
    Other(char),
}
impl Default for V1Token { fn default() -> V1Token { V1Token::EOF } }

pub struct V1Lexer<'chs> {
    v0: BufLexer<V0Lexer<'chs>, char>,
}
impl<'chs> ILexer<'chs, V1Token> for V1Lexer<'chs> {

    fn new(source: SourceCodeIter<'chs>) -> V1Lexer<'chs> {
        V1Lexer { v0: BufLexer::new(source) }
    }

    fn next(&mut self, sess: &mut ParseSession) -> (V1Token, Span) {
        
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
            self.v0.move_next(sess);
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
                (State::Nothing, &EOF_CHAR, eof_pos, _3, _4, _5, _6) => {                // C7: in nothing, meet EOF, return 
                    return (V1Token::EOF, eof_pos);
                }
                (State::Nothing, ch, strpos, _3, _4, _5, _6) => {                       // C6: in nothing, meet other, return
                    return (V1Token::Other(*ch), strpos);
                }
                (State::InBlockComment{ start_span }, &'*', _2, &'/', end_pos, _5, _6) => {   // C8: in block, meet */, return
                    self.v0.prepare_skip1();
                    return (V1Token::Other(' '), start_span.merge(&end_pos));
                }
                (State::InBlockComment{ start_span }, &EOF_CHAR, eof_pos, _3, _4, _5, _6) => {  // C10: in block, meet EOF, emit error, return
                    sess.messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
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
                (State::InLineComment, &EOF_CHAR, eof_pos, _3, _4, _5, _6) => {          // C13: in line, meet EOF, return
                    return (V1Token::EOF, eof_pos);
                }
                (State::InLineComment, _1, _2, _3, _4, _5, _6) => {                     // C12: in line, continue line
                    state = State::InLineComment;
                }

                (State::InStringLiteral{ mut parser }, ch, pos, next_ch, _4, _5, _6) => {   // Cx: anything inside "" is none about this module
                    match parser.input(*ch, pos.get_start_pos(), *next_ch, sess.messages) {
                        StringLiteralParserResult::WantMore => {
                            state = State::InStringLiteral{ parser: parser };
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InStringLiteral{ parser: parser };
                        }
                        StringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF_CHAR { // if EOF_CHAR was consumed by str lit parser, it will not be returned as EOF, which is not designed by feature
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::StringLiteral(value.map(|v| StrLitValue::Simple(sess.symbols.intern(v)))), pos);
                        }
                    }
                }
                (State::InRawStringLiteral{ mut parser }, ch, strpos, _3, _4, _5, _6) => {     // Cx, anything inside r"" is none about this module
                    match parser.input(*ch, strpos.get_start_pos(), sess.messages) {
                        RawStringLiteralParserResult::WantMore => {
                            state = State::InRawStringLiteral{ parser: parser };
                        }
                        RawStringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF_CHAR { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::RawStringLiteral(value.map(|v| StrLitValue::Simple(sess.symbols.intern(v)))), pos);
                        }
                    }
                }
                (State::InCharLiteral{ mut parser }, ch, strpos, next_ch, _4, _5, _6) => {     // Cx: anything inside '' is none about this module
                    match parser.input(*ch, strpos.get_start_pos(), *next_ch, sess.messages) {
                        CharLiteralParserResult::WantMore => {
                            state = State::InCharLiteral{ parser: parser };
                        },
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InCharLiteral{ parser: parser };
                        }
                        CharLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF_CHAR { // same as str lit parser
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
    use crate::source::SourceCode;
    use crate::source::SymbolCollection;
    use crate::diagnostics::MessageCollection;

    fn test_case_full(src: &str, symbols: SymbolCollection, expect_tokens: Vec<(V1Token, Span)>, expect_messages: MessageCollection) {
        let mut actual_messages = MessageCollection::new();
        let mut symbols = symbols;
        let mut sess = ParseSession::new(&mut actual_messages, &mut symbols);
        let source = SourceCode::with_test_str(0, src);
        let mut v1lexer = V1Lexer::new(source.iter());
        for expect_token in expect_tokens {
            assert_eq!(v1lexer.next(&mut sess), expect_token);
        }
        let next = v1lexer.next(&mut sess);
        if next.0 != V1Token::EOF { panic!("next is not EOF but {:?}", next); }

        assert_eq!(sess.messages, &expect_messages);
    }

    macro_rules! test_case {
        ($src: expr, $expect_tokens: expr) =>
            (test_case_full($src, SymbolCollection::new(), $expect_tokens, MessageCollection::new()));
        (with symbol, $src: expr, $symbols: expr, $expect_tokens: expr) => 
            (test_case_full($src, $symbols, $expect_tokens, MessageCollection::new()));
        (with message, $src: expr, $expect_tokens: expr, $expect_messages: expr) => 
            (test_case_full($src, SymbolCollection::new(), $expect_tokens, $expect_messages));
    }
    macro_rules! n { // normal dispatch to next level
        ($ch: expr, $char_id: expr) => ((V1Token::Other($ch), make_span!($char_id, $char_id)));
        ($ch: expr, $start_id: expr, $end_id: expr) => ((V1Token::Other($ch), make_span!($start_id, $end_id)))
    }
    macro_rules! ch_lit {
        ($ch: expr, $start_id: expr, $end_id: expr) => ((V1Token::CharLiteral(Some($ch)), make_span!($start_id, $end_id)));
        ($start_id: expr, $end_id: expr) => ((V1Token::CharLiteral(None), make_span!($start_id, $end_id)))
    }
    macro_rules! str_lit {
        ($start_id: expr, $end_id: expr) => ((V1Token::StringLiteral(None), make_span!($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => ((V1Token::StringLiteral(Some(StrLitValue::Simple($val))), make_span!($start_id, $end_id)))
    }
    macro_rules! rstr_lit {
        ($start_id: expr, $end_id: expr) => ((V1Token::RawStringLiteral(None), make_span!($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => ((V1Token::RawStringLiteral(Some(StrLitValue::Simple($val))), make_span!($start_id, $end_id)))
    }

    // Line comment as \n
    test_case!{ "ABC//DEF\n", vec![n!('A', 0), n!('B', 1), n!('C', 2), n!('\n', 8) ] }       // C6, C1, C12, C11, C7
    // Line comment EOF is not error   
    test_case!{ "ABC//DEF", vec![n!('A', 0), n!('B', 1), n!('C', 2) ] }                      // C6, C1, C12, C13

    // Block comment is ' ', but has correct position
    test_case!{ "A/*D\nEF*/GH", vec![n!('A', 0), n!(' ', 1, 8), n!('G', 9), n!('H', 10)] }   // C6, C2, C9, C8
    // EOF in block comment is error 
    test_case!{ with message, "A/*BC", vec![n!('A', 0)], make_messages![                     // C6, C2, C9, C10
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(1, 1), error_strings::BlockCommentStartHere),
            (make_span!(5, 5), error_strings::EOFHere),
        ])
    ]}

    // String literal test cases
    test_case!{ with symbol, r#""Hello, world!""#, 
        make_symbols!["Hello, world!"], vec![str_lit!(make_id!(1), 0, 14)] 
    }
    test_case!{ with message, r#""He"#, vec![str_lit!(0, 3)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(3, 3), error_strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r#""He\"l\"lo"#, vec![str_lit!(0, 10)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(10, 10), error_strings::EOFHere),
            (make_span!(6, 6), error_strings::LastEscapedQuoteHere),
        ])
    ]}
    test_case!{ with symbol, r#""H\t\n\0\'\"llo""#,
        make_symbols!["H\t\n\0'\"llo"], vec![str_lit!(make_id!(1), 0, 15)]
    }
    test_case!{ with message, r#""h\c\d\e\n\g""#, vec![str_lit!(0, 12)], make_messages![
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
    test_case!{ with symbol, r#""H\uABCDel""#, 
        make_symbols!["H\u{ABCD}el"], vec![str_lit!(make_id!(1), 0, 10)]
    } //                          0123456789012345
    test_case!{ with message, r#""H\uABCHel\uABgC""#, vec![str_lit!(0, 16)], make_messages![ // TODO: accordint to new char lit parser's policy, this should be str_lit!(0, 15)
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
    ]} //                         012345678901
    test_case!{ with message, r#""H\U0011ABCD""#, vec![str_lit!(0, 12)], make_messages![ // TODO: same as before, this should be str_lit!(0, 11)
        Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (make_span!(2, 11), error_strings::UnicodeCharEscapeHere.to_owned()),
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}
    test_case!{ with message, r#""H\u""#, vec![str_lit!(0, 4)], make_messages![
        Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(2, 2), error_strings::UnicodeCharEscapeStartHere),
            (make_span!(4, 4), error_strings::StringLiteralEndHere),
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]}
    test_case!{ with message, r#""h\U123"#, vec![str_lit!(0, 7)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(7, 7), error_strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r#""he\"#, vec![str_lit!(0, 4)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}

    // Raw string literal test cases
    test_case!{ with symbol, r#"r"hell\u\no""#, 
        make_symbols![r"hell\u\no"], vec![rstr_lit!(make_id!(1), 0, 11)]
    }
    test_case!{ with message, r#"R"he"#, vec![rstr_lit!(0, 4)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::StringLiteralStartHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}

    // Char literal test cases
    test_case!{ "'A'", vec![ch_lit!('A', 0, 2)] }
    test_case!{ r"'\t'", vec![ch_lit!('\t', 0, 3)] }
    test_case!{ r"'\uABCD'", vec![ch_lit!('\u{ABCD}', 0, 7)] }
    test_case!{ with message, "''", vec![ch_lit!(0, 1)], make_messages![
        Message::with_help_by_str(error_strings::EmptyCharLiteral, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere), 
        ], vec![
            error_strings::CharLiteralSyntaxHelp1
        ])
    ]}
    test_case!{ with message, "'ABC'", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),
        ])
    ]}
    test_case!{ with message, r"'\c'", vec![ch_lit!(0, 3)], make_messages![
        Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (make_span!(0, 0), error_strings::CharLiteralStartHere.to_owned()), 
            (make_span!(1, 1), error_strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]} //         012345
    test_case!{ with message, r"'\uBG'", vec![ch_lit!(0, 5)], make_messages![
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
    test_case!{ with message, r"'\U0011ABCD'", vec![ch_lit!(0, 11)], make_messages![
        Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (make_span!(1, 10), error_strings::UnicodeCharEscapeHere.to_owned()), 
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}
    test_case!{ with message, r"'\na'", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),   
        ])
    ]}
    test_case!{ with message, r"'\uABCDA'", vec![ch_lit!(0, 8)], make_messages![
        Message::new_by_str(error_strings::CharLiteralTooLong, vec![
            (make_span!(0, 8), error_strings::CharLiteralHere),      
        ])
    ]}
    test_case!{ with message, "'", vec![ch_lit!(0, 0)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 0), error_strings::CharLiteralHere),    
            (make_span!(1, 1), error_strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\", vec![ch_lit!(0, 1)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere),      
            (make_span!(2, 2), error_strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\u", vec![ch_lit!(0, 2)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 2), error_strings::CharLiteralHere),
            (make_span!(3, 3), error_strings::EOFHere)
        ])
    ]}
    test_case! { with message, r"'A", vec![ch_lit!(0, 1)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 1), error_strings::CharLiteralHere),
            (make_span!(2, 2), error_strings::EOFHere)
        ])
    ]}
    test_case! { with message, "'ABC", vec![ch_lit!(0, 3)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 3), error_strings::CharLiteralHere),
            (make_span!(4, 4), error_strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\'AB", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (make_span!(0, 4), error_strings::CharLiteralHere),
            (make_span!(5, 5), error_strings::EOFHere),
        ])
    ]}
}