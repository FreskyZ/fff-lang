///! fff-lang
///!
///! lexical/v1, input v0
///! remove line comment
///! report block comment as OtherChar ' '
///! find string literal with only '"' escaped
///! string literal is allowed to cross line, line end is regarded as \n
///! raw string literal supported, `r'C:\\abc'` or `R"C:\\abc"`

use crate::source::{Span, SourceChars, FileSystem, IsId, EOF};
use crate::diagnostics::{Message, strings};

use super::{ILexer, BufLexer, ParseSession};
use super::literal::string::{StringLiteralParser, StringLiteralParserResult};
use super::literal::raw_string::{RawStringLiteralParser, RawStringLiteralParserResult};
use super::literal::char::{CharLiteralParser, CharLiteralParserResult};

// SourceCodeIter wrapper
pub struct V0Lexer<'a, F>(pub SourceChars<'a, F>);
impl<'a, F> ILexer<'a, F, char> for V0Lexer<'a, F> where F: FileSystem {
    fn new(source: SourceChars<'a, F>) -> Self { Self(source) }
    fn next(&mut self, _: &mut ParseSession) -> (char, Span) {
        let ret_val = self.0.next();
        (ret_val.0, ret_val.1.into())
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum V1Token {
    EOF,
    StringLiteral(IsId),
    RawStringLiteral(IsId),
    CharLiteral(char),
    Other(char),
}
impl Default for V1Token { fn default() -> V1Token { V1Token::EOF } }

pub struct V1Lexer<'chs, F> {
    pub v0: BufLexer<V0Lexer<'chs, F>, char, F>,
}
impl<'chs, F> ILexer<'chs, F, V1Token> for V1Lexer<'chs, F> where F: FileSystem {

    fn new(source: SourceChars<'chs, F>) -> V1Lexer<'chs, F> {
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
                    state = State::InStringLiteral { parser: StringLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &'r', start_span, &'"', _4, _5, _6)
                | (State::Nothing, &'R', start_span, &'"', _4, _5, _6) => {           // C4: in nothing, meet r" or R"
                    self.v0.prepare_skip1();                   
                    state = State::InRawStringLiteral { parser: RawStringLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &'\'', start_span, _3, _4, _5, _6) => {              // C5: in nothing, meet '
                    state = State::InCharLiteral{ parser: CharLiteralParser::new(start_span.start) };
                }
                (State::Nothing, &EOF, eof_pos, _3, _4, _5, _6) => {                // C7: in nothing, meet EOF, return 
                    return (V1Token::EOF, eof_pos);
                }
                (State::Nothing, ch, span, _3, _4, _5, _6) => {                       // C6: in nothing, meet other, return
                    return (V1Token::Other(*ch), span);
                }
                (State::InBlockComment{ start_span }, &'*', _2, &'/', end_pos, _5, _6) => {   // C8: in block, meet */, return
                    self.v0.prepare_skip1();
                    return (V1Token::Other(' '), start_span + end_pos);
                }
                (State::InBlockComment{ start_span }, &EOF, eof_pos, _3, _4, _5, _6) => {  // C10: in block, meet EOF, emit error, return
                    sess.messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                        (start_span, strings::BlockCommentStartHere),
                        (eof_pos, strings::EOFHere),
                    ]));
                    return (V1Token::EOF, eof_pos);
                }
                (State::InBlockComment{ start_span }, _1, _2, _3, _4, _5, _6) => {       // C9: in block, continue block
                    state = State::InBlockComment{ start_span };
                }
                (State::InLineComment, &'\n', lf_pos, _3, _4, _5, _6) => {              // C11: in line, meet \n, return
                    return (V1Token::Other('\n'), lf_pos);
                }
                (State::InLineComment, &EOF, eof_pos, _3, _4, _5, _6) => {          // C13: in line, meet EOF, return
                    return (V1Token::EOF, eof_pos);
                }
                (State::InLineComment, _1, _2, _3, _4, _5, _6) => {                     // C12: in line, continue line
                    state = State::InLineComment;
                }

                (State::InStringLiteral{ mut parser }, ch, pos, next_ch, _4, _5, _6) => {   // Cx: anything inside "" is none about this module
                    match parser.input(*ch, pos.start, *next_ch, sess.messages) {
                        StringLiteralParserResult::WantMore => {
                            state = State::InStringLiteral{ parser: parser };
                        },
                        StringLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InStringLiteral{ parser: parser };
                        }
                        StringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // if EOF was consumed by str lit parser, it will not be returned as EOF, which is not designed by feature
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::StringLiteral(value.map(|v| self.v0.lexer.0.intern(&v)).unwrap_or(IsId::new(1))), pos);
                        }
                    }
                }
                (State::InRawStringLiteral{ mut parser }, ch, span, _3, _4, _5, _6) => {     // Cx, anything inside r"" is none about this module
                    match parser.input(*ch, span.start, sess.messages) {
                        RawStringLiteralParserResult::WantMore => {
                            state = State::InRawStringLiteral{ parser: parser };
                        }
                        RawStringLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::RawStringLiteral(value.map(|v| self.v0.lexer.0.intern(&v)).unwrap_or(IsId::new(1))), pos);
                        }
                    }
                }
                (State::InCharLiteral{ mut parser }, ch, span, next_ch, _4, _5, _6) => {     // Cx: anything inside '' is none about this module
                    match parser.input(*ch, span.start, *next_ch, sess.messages) {
                        CharLiteralParserResult::WantMore => {
                            state = State::InCharLiteral{ parser: parser };
                        },
                        CharLiteralParserResult::WantMoreWithSkip1 => {
                            self.v0.prepare_skip1();
                            state = State::InCharLiteral{ parser: parser };
                        }
                        CharLiteralParserResult::Finished(value, pos) => {
                            if *ch == EOF { // same as str lit parser
                                self.v0.prepare_dummy1();
                            }
                            return (V1Token::CharLiteral(value.unwrap_or_default()), pos);
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
    use crate::source::{SourceContext, VirtualFileSystem, IsId, make_source};
    use crate::diagnostics::MessageCollection;

    fn test_case_full(mut scx: SourceContext<VirtualFileSystem>, symbols: &[&'static str], expect_tokens: Vec<(V1Token, Span)>, expect_messages: MessageCollection) {
        let mut actual_messages = MessageCollection::new();
        let mut sess = ParseSession::new(&mut actual_messages);
        let mut chars = scx.entry("1");
        for symbol in symbols {
            chars.intern(*symbol);
        }
        let mut v1lexer = V1Lexer::new(chars);
        for expect_token in expect_tokens {
            assert_eq!(v1lexer.next(&mut sess), expect_token);
        }
        let next = v1lexer.next(&mut sess);
        if next.0 != V1Token::EOF { panic!("next is not EOF but {:?}", next); }

        assert_eq!(sess.messages, &expect_messages);
    }

    macro_rules! test_case {
        ($src:literal, $expect_tokens:expr) =>
            (test_case_full(make_source!($src), &[], $expect_tokens, MessageCollection::new()));
        (with symbol, $src:literal, $($symbol:literal,)+ $expect_tokens:expr) => 
            (test_case_full(make_source!($src), &[$($symbol)+], $expect_tokens, MessageCollection::new()));
        (with message, $src:expr, $expect_tokens:expr, $expect_messages:expr) => 
            (test_case_full(make_source!($src), &[], $expect_tokens, $expect_messages));
    }
    macro_rules! n { // normal dispatch to next level
        ($ch: expr, $char_id: expr) => ((V1Token::Other($ch), Span::new($char_id, $char_id)));
        ($ch: expr, $start_id: expr, $end_id: expr) => ((V1Token::Other($ch), Span::new($start_id, $end_id)))
    }
    macro_rules! ch_lit {
        ($ch: expr, $start_id: expr, $end_id: expr) => ((V1Token::CharLiteral($ch), Span::new($start_id, $end_id)));
        ($start_id: expr, $end_id: expr) => ((V1Token::CharLiteral('\0'), Span::new($start_id, $end_id)))
    }
    macro_rules! str_lit {
        ($start_id: expr, $end_id: expr) => ((V1Token::StringLiteral(IsId::new(1)), Span::new($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => ((V1Token::StringLiteral($val), Span::new($start_id, $end_id)))
    }
    macro_rules! rstr_lit {
        ($start_id: expr, $end_id: expr) => ((V1Token::RawStringLiteral(IsId::new(1)), Span::new($start_id, $end_id)));
        ($val: expr, $start_id: expr, $end_id: expr) => ((V1Token::RawStringLiteral($val), Span::new($start_id, $end_id)))
    }

    // Line comment as \n
    test_case!{ "ABC//DEF\n", vec![n!('A', 0), n!('B', 1), n!('C', 2), n!('\n', 8) ] }       // C6, C1, C12, C11, C7
    // Line comment EOF is not error   
    test_case!{ "ABC//DEF", vec![n!('A', 0), n!('B', 1), n!('C', 2) ] }                      // C6, C1, C12, C13

    // Block comment is ' ', but has correct position
    test_case!{ "A/*D\nEF*/GH", vec![n!('A', 0), n!(' ', 1, 8), n!('G', 9), n!('H', 10)] }   // C6, C2, C9, C8
    // EOF in block comment is error 
    test_case!{ with message, "A/*BC", vec![n!('A', 0)], make_messages![                     // C6, C2, C9, C10
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(1, 1), strings::BlockCommentStartHere),
            (Span::new(5, 5), strings::EOFHere),
        ])
    ]}

    // String literal test cases
    test_case!{ with symbol, r#""Hello, world!""#, 
        "Hello, world!", vec![str_lit!(IsId::new(2), 0, 14)] 
    }
    test_case!{ with message, r#""He"#, vec![str_lit!(0, 3)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(3, 3), strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r#""He\"l\"lo"#, vec![str_lit!(0, 10)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(10, 10), strings::EOFHere),
            (Span::new(6, 6), strings::LastEscapedQuoteHere),
        ])
    ]}
    test_case!{ with symbol, r#""H\t\n\0\'\"llo""#,
        "H\t\n\0'\"llo", vec![str_lit!(IsId::new(2), 0, 15)]
    }
    test_case!{ with message, r#""h\c\d\e\n\g""#, vec![str_lit!(0, 12)], make_messages![
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
    test_case!{ with symbol, r#""H\uABCDel""#, 
        "H\u{ABCD}el", vec![str_lit!(IsId::new(2), 0, 10)]
    } //                          0123456789012345
    test_case!{ with message, r#""H\uABCHel\uABgC""#, vec![str_lit!(0, 16)], make_messages![ // TODO: according to new char lit parser's policy, this should be str_lit!(0, 15)
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
    ]} //                         012345678901
    test_case!{ with message, r#""H\U0011ABCD""#, vec![str_lit!(0, 12)], make_messages![ // TODO: same as before, this should be str_lit!(0, 11)
        Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (Span::new(2, 11), strings::UnicodeCharEscapeHere.to_owned()),
        ], vec![
            format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}
    test_case!{ with message, r#""H\u""#, vec![str_lit!(0, 4)], make_messages![
        Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(2, 2), strings::UnicodeCharEscapeStartHere),
            (Span::new(4, 4), strings::StringLiteralEndHere),
        ], vec![
            strings::UnicodeCharEscapeHelpSyntax,
        ])
    ]}
    test_case!{ with message, r#""h\U123"#, vec![str_lit!(0, 7)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(7, 7), strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r#""he\"#, vec![str_lit!(0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}

    // Raw string literal test cases
    test_case!{ with symbol, r#"r"hell\u\no""#, 
        r"hell\u\no", vec![rstr_lit!(IsId::new(2), 0, 11)]
    }
    test_case!{ with message, r#"R"he"#, vec![rstr_lit!(0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::StringLiteralStartHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}

    // Char literal test cases
    test_case!{ "'A'", vec![ch_lit!('A', 0, 2)] }
    test_case!{ r"'\t'", vec![ch_lit!('\t', 0, 3)] }
    test_case!{ r"'\uABCD'", vec![ch_lit!('\u{ABCD}', 0, 7)] }
    test_case!{ with message, "''", vec![ch_lit!(0, 1)], make_messages![
        Message::with_help_by_str(strings::EmptyCharLiteral, vec![
            (Span::new(0, 1), strings::CharLiteralHere), 
        ], vec![
            strings::CharLiteralSyntaxHelp1
        ])
    ]}
    test_case!{ with message, "'ABC'", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 4), strings::CharLiteralHere),
        ])
    ]}
    test_case!{ with message, r"'\c'", vec![ch_lit!(0, 3)], make_messages![
        Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
            (Span::new(0, 0), strings::CharLiteralStartHere.to_owned()), 
            (Span::new(1, 1), strings::UnknownCharEscapeHere.to_owned()),
        ])
    ]} //         012345
    test_case!{ with message, r"'\uBG'", vec![ch_lit!(0, 5)], make_messages![
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
    test_case!{ with message, r"'\U0011ABCD'", vec![ch_lit!(0, 11)], make_messages![
        Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (Span::new(1, 10), strings::UnicodeCharEscapeHere.to_owned()), 
        ], vec![
            format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            strings::UnicodeCharEscapeHelpValue.to_owned(),
        ])
    ]}
    test_case!{ with message, r"'\na'", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 4), strings::CharLiteralHere),   
        ])
    ]}
    test_case!{ with message, r"'\uABCDA'", vec![ch_lit!(0, 8)], make_messages![
        Message::new_by_str(strings::CharLiteralTooLong, vec![
            (Span::new(0, 8), strings::CharLiteralHere),      
        ])
    ]}
    test_case!{ with message, "'", vec![ch_lit!(0, 0)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 0), strings::CharLiteralHere),    
            (Span::new(1, 1), strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\", vec![ch_lit!(0, 1)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 1), strings::CharLiteralHere),      
            (Span::new(2, 2), strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\u", vec![ch_lit!(0, 2)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 2), strings::CharLiteralHere),
            (Span::new(3, 3), strings::EOFHere)
        ])
    ]}
    test_case! { with message, r"'A", vec![ch_lit!(0, 1)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 1), strings::CharLiteralHere),
            (Span::new(2, 2), strings::EOFHere)
        ])
    ]}
    test_case! { with message, "'ABC", vec![ch_lit!(0, 3)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 3), strings::CharLiteralHere),
            (Span::new(4, 4), strings::EOFHere)
        ])
    ]}
    test_case!{ with message, r"'\'AB", vec![ch_lit!(0, 4)], make_messages![
        Message::new_by_str(strings::UnexpectedEOF, vec![
            (Span::new(0, 4), strings::CharLiteralHere),
            (Span::new(5, 5), strings::EOFHere),
        ])
    ]}
}