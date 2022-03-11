///! fff-lang
///!
///! string literal parser

use crate::source::{FileSystem, Position, Span, IsId, EOF};
use crate::diagnostics::{Message, MessageCollection, strings};
use super::escape::{EscapeCharParser, EscapeCharSimpleCheckResult, EscapeCharParserResult};
use super::super::{Parser, StringLiteralType, Token};

// Escape issues about string literal and char literal
// all escapes: \t, \n, \r, \0, \\, \", \', \uxxxx, \Uxxxxxxxx, are all supported in char and string literal
// when meet \, start parsing escape char literal
// if meet EOF, string report unexpected EOF in string, char report unexpected EOF in char
// if meet end of literal, e.g.  '\uAA' or "123\U45678", report incorrect unicode escape error
// specially  
//     '\', char parser will find next is not ' and immediately stop char parser and report a special error for this
//     "\", string parser will record this escape position and this will most probably cause unexpected EOF in string and string parser will report it

impl<'ecx, 'scx, F> Parser<'ecx, 'scx, F> where F: FileSystem {

    fn parse_normal_string_literal(&mut self) -> (Token, Span) {

        let start_position = self.current.1;
        let mut raw = String::new();
        let mut last_escape_quote_position = None; // indicate if string normal end is unexpectedly escaped
        let mut escape_parser = None;
        let mut has_failed = false;

        self.eat(); // eat beginning double quote
        loop {
            match (self.current.0, self.current.1, self.peek1.0) {
                ('\\', _1, EOF) => {                                                // C4, \EOF, ignore
                    // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                    self.eatnc();
                }
                ('\\', slash_pos, next_ch) => {
                    match EscapeCharParser::simple_check(next_ch) {
                        EscapeCharSimpleCheckResult::Normal(ch) => {                    // C1, normal escape
                            raw.push(ch);
                            if ch == '"' {
                                last_escape_quote_position = Some(slash_pos);
                            }
                            self.eatnc();
                            self.eatnc();
                        } 
                        EscapeCharSimpleCheckResult::Invalid(ch) => {                   // C2, error normal escape, emit error and continue
                            self.diagnostics.push(Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, ch), vec![
                                (start_position.into(), strings::StringLiteralStartHere.to_owned()),
                                (slash_pos.into(), strings::UnknownCharEscapeHere.to_owned()),
                            ]));
                            has_failed = true;
                            self.eatnc();
                            self.eatnc();
                        }
                        EscapeCharSimpleCheckResult::Unicode(more) => {               // C3, start unicode escape
                            escape_parser = Some((more, slash_pos));
                            self.eatnc();
                            self.eatnc();
                        }
                    }
                }
                ('"', pos, _2) => {
                    // String finished, check if is parsing escape
                    if let Some((_, escape_start_position)) = escape_parser {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        self.diagnostics.push(Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
                            (start_position.into(), strings::StringLiteralStartHere),
                            (escape_start_position.into(), strings::UnicodeCharEscapeStartHere),
                            (pos.into(), strings::StringLiteralEndHere),
                        ], vec![
                            strings::UnicodeCharEscapeHelpSyntax,
                        ]));
                        self.eatnc();
                        return (Token::Str(IsId::new(1), StringLiteralType::Normal), start_position + pos);
                    } else {                                                       // C7, normal end, return
                        self.eatnc();
                        return (Token::Str(if has_failed { IsId::new(1) } else { self.chars.intern(&raw) }, StringLiteralType::Normal), start_position + pos);
                    }
                }
                (EOF, pos, _2) => {
                    match last_escape_quote_position {                                      // C12: in string, meet EOF, emit error, return 
                        Some(escaped_quote_pos_hint) => 
                            self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                                (start_position.into(), strings::StringLiteralStartHere),
                                (pos.into(), strings::EOFHere),
                                (escaped_quote_pos_hint.into(), strings::LastEscapedQuoteHere),
                            ])),
                        None => 
                            self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                                (start_position.into(), strings::StringLiteralStartHere),
                                (pos.into(), strings::EOFHere),
                            ]))
                    }
                    self.eatnc();
                    return (Token::Str(IsId::new(1), StringLiteralType::Normal), start_position + pos);
                }
                (ch, pos, _2) => {
                    // Normal in string
                    let mut need_reset_escape_parser = false;
                    match escape_parser {
                        Some((ref mut escape_parser, escape_start_position)) => {
                            match escape_parser.input(ch, (escape_start_position, pos), self.diagnostics) {
                                EscapeCharParserResult::WantMore => (),            // C8, in unicode escape, (may be fail and) want more
                                EscapeCharParserResult::Failed => {
                                    has_failed = true;
                                    need_reset_escape_parser = true;                    // C9, in unicode escape, not hex char or last not unicode codepoint value, finish
                                }
                                EscapeCharParserResult::Success(ch) => {                // C10, in unicode escape, success, finish
                                    need_reset_escape_parser = true;
                                    raw.push(ch);
                                }
                            }
                        }
                        None => {
                            raw.push(ch);                                          // C11, most plain
                        }
                    }
                    if need_reset_escape_parser {  
                        escape_parser = None;
                    }
                    self.eatnc();
                }
            }
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Debug)]
pub enum RawStringLiteralParserResult { 
    WantMore, 
    Finished(Option<String>, Span),
}

#[derive(Debug)]
pub struct RawStringLiteralParser {
    raw: String,
    start_pos: Position,
}
impl RawStringLiteralParser {

    pub fn new(start_pos: Position) -> RawStringLiteralParser {
        RawStringLiteralParser {
            raw: String::new(),
            start_pos,
        }
    }

    pub fn input(&mut self, ch: char, pos: Position, messages: &mut MessageCollection) -> RawStringLiteralParserResult {
        match (ch, pos) {
            ('"', pos) => {                                               // C1: in raw string, meet ", finish, return
                return RawStringLiteralParserResult::Finished(Some(self.raw.clone()), self.start_pos + pos);
            }
            (EOF, pos) => {                                                    // C3: in raw string, meet EOF, emit error, return  
                messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![ 
                    (self.start_pos.into(), strings::StringLiteralStartHere),
                    (pos.into(), strings::EOFHere),
                ]));
                return RawStringLiteralParserResult::Finished(None, self.start_pos + pos);
            }
            (ch, _1) => {                                                 // C2: in raw string, meet other, continue
                self.raw.push(ch);
                return RawStringLiteralParserResult::WantMore;
            }
        }
    }
}

impl<'ecx, 'scx, F> Parser<'ecx, 'scx, F> where F: FileSystem {

    pub(in super::super) fn parse_string_literal(&mut self, literal_type: StringLiteralType) -> (Token, Span) {
        match literal_type {
            StringLiteralType::Normal => return self.parse_normal_string_literal(),
            StringLiteralType::Raw => {
                let mut parser = RawStringLiteralParser::new(self.current.1);
                self.eat();
                self.eat();
                loop {
                    match parser.input(self.current.0, self.current.1, self.diagnostics) {
                        RawStringLiteralParserResult::WantMore => {
                            self.eatnc();
                        },
                        RawStringLiteralParserResult::Finished(value, span) => {
                            self.eatnc();
                            return (Token::Str(value.map(|v| self.chars.intern(&v)).unwrap_or(IsId::new(1)), StringLiteralType::Raw), span);
                        }
                    }
                }
            },
            _ => unreachable!(),
        }
    }
}

#[cfg(test)] #[test]
fn raw_string_lit_parser() {
    use self::RawStringLiteralParserResult::*;

    let dummy_pos = Position::new(0);
    let spec_pos1 = Position::new(34);
    let spec_pos2 = Position::new(78);
    let spec_pos4 = Position::new(1516);

    {   // r"hell\u\no", normal, C1, C2
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('\\', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('u', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('\\', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('n', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('"', spec_pos4, messages), 
            Finished(Some(r"hell\u\no".to_owned()), spec_pos1 + spec_pos4));

        assert_eq!(messages, expect_messages);
    }

    {   // R"he$, normal, C2, C3
        let mut parser = RawStringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, messages), WantMore);
        assert_eq!(parser.input(EOF, spec_pos2, messages), 
            Finished(None, spec_pos1 + spec_pos2));

        expect_messages.push(Message::new_by_str(strings::UnexpectedEOF, vec![ 
            (spec_pos1.into(), strings::StringLiteralStartHere),
            (spec_pos2.into(), strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }
}

#[cfg(test)] #[test]
fn str_lit_parse() {
    // use self::StringLiteralParserResult::*;

    // let dummy_pos = Position::new(0);
    // let spec_pos1 = Position::new(34);
    // let spec_pos2 = Position::new(78);
    // let spec_pos3 = Position::new(1112);
    // let spec_pos4 = Position::new(1516);

    // {   // "Hello, world!", most normal,                                    C11, C5, C7
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new(); 
    //     assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
    //     assert_eq!(parser.input('o', dummy_pos, 'o', messages), WantMore);
    //     assert_eq!(parser.input(',', dummy_pos, ',', messages), WantMore);
    //     assert_eq!(parser.input(' ', dummy_pos, ' ', messages), WantMore);
    //     assert_eq!(parser.input('w', dummy_pos, 'w', messages), WantMore);
    //     assert_eq!(parser.input('o', dummy_pos, 'o', messages), WantMore);
    //     assert_eq!(parser.input('r', dummy_pos, 'r', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('d', dummy_pos, 'd', messages), WantMore);
    //     assert_eq!(parser.input('!', dummy_pos, EOF, messages), WantMore);
    //     assert_eq!(parser.input('"', spec_pos2, EOF, messages), 
    //         Finished(Some("Hello, world!".to_owned()), spec_pos1 + spec_pos2));

    //     assert_eq!(messages, &MessageCollection::new());
    // }

    // {   // "He$, unexpected end, no last escaped quote hint                 C11, C12
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input(EOF, spec_pos2, EOF, messages), 
    //         Finished(None, spec_pos1 + spec_pos2));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::new_by_str(strings::UnexpectedEOF, vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere),
    //             (spec_pos2.into(), strings::EOFHere),
    //         ])
    //     ]);
    // }

    // {   // "He\"l\"lo$, unexpected EOF, last escaped quote recorded         C11, C1, C12
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, '"', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('l', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos3, '"', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
    //     assert_eq!(parser.input('o', dummy_pos, EOF, messages), WantMore);
    //     assert_eq!(parser.input(EOF, spec_pos4, EOF, messages), 
    //         Finished(None, spec_pos1 + spec_pos4));

    //     assert_eq!(messages, &make_messages![
    //         Message::new_by_str(strings::UnexpectedEOF, vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere),
    //             (spec_pos4.into(), strings::EOFHere),
    //             (spec_pos3.into(), strings::LastEscapedQuoteHere),
    //         ])
    //     ]);
    // }

    // {   // "H\t\n\0\'\"llo", normal escape                                  C11, C1, C7
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', dummy_pos, 't', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', dummy_pos, 'n', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', dummy_pos, '0', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', dummy_pos, '\'', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', dummy_pos, '"', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
    //     assert_eq!(parser.input('o', dummy_pos, '"', messages), WantMore);
    //     assert_eq!(parser.input('"', spec_pos4, '$', messages), 
    //         Finished(Some("H\t\n\0\'\"llo".to_owned()), spec_pos1 + spec_pos4));

    //     assert_eq!(messages, &make_messages![]);
    // }

    // {   // "h\c\d\e\n\g", error normal escape                               C11, C3, C2
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'c', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', spec_pos3, 'd', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', spec_pos2, 'e', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', dummy_pos, 'n', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('\\', spec_pos3, 'g', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('"', spec_pos4, '$', messages),
    //         Finished(None, spec_pos1 + spec_pos4));

    //     assert_eq!(messages, &make_messages![
    //         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'c'), vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere.to_owned()),
    //             (spec_pos2.into(), strings::UnknownCharEscapeHere.to_owned()),
    //         ]),
    //         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'd'), vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere.to_owned()),
    //             (spec_pos3.into(), strings::UnknownCharEscapeHere.to_owned()),
    //         ]),
    //         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'e'), vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere.to_owned()),
    //             (spec_pos2.into(), strings::UnknownCharEscapeHere.to_owned()),
    //         ]),
    //         Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, 'g'), vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere.to_owned()),
    //             (spec_pos3.into(), strings::UnknownCharEscapeHere.to_owned()),
    //         ])
    //     ]);
    // }

    // {   // "H\uABCDel", unicode escape                                      C11, C3, C8, C10, C7
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
    //     assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
    //     assert_eq!(parser.input('C', dummy_pos, 'D', messages), WantMore);
    //     assert_eq!(parser.input('D', dummy_pos, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, '"', messages), WantMore);
    //     assert_eq!(parser.input('"', spec_pos3, '$', messages), 
    //         Finished(Some("H\u{ABCD}el".to_owned()), spec_pos1 + spec_pos3));
        
    //     assert_eq!(messages, &make_messages![]);
    // }

    // {   // "H\uABCHel\uABCg", unicode escape error                          C11, C3, C8, C9, C7
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
    //     assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
    //     assert_eq!(parser.input('C', dummy_pos, 'H', messages), WantMore);
    //     assert_eq!(parser.input('H', spec_pos3, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
    //     assert_eq!(parser.input('l', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
    //     assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
    //     assert_eq!(parser.input('C', dummy_pos, 'g', messages), WantMore);
    //     assert_eq!(parser.input('g', spec_pos4, '"', messages), WantMore);
    //     assert_eq!(parser.input('"', spec_pos4, '$', messages), 
    //         Finished(None, spec_pos1 + spec_pos4));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
    //             (spec_pos2.into(), strings::UnicodeCharEscapeStartHere),
    //             (spec_pos3.into(), strings::UnicodeCharEscapeInvalidChar)
    //         ], vec![
    //             strings::UnicodeCharEscapeHelpSyntax,
    //         ]),
    //         Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
    //             (spec_pos3.into(), strings::UnicodeCharEscapeStartHere),
    //             (spec_pos4.into(), strings::UnicodeCharEscapeInvalidChar)
    //         ], vec![
    //             strings::UnicodeCharEscapeHelpSyntax,
    //         ])
    //     ]);
    // }

    // {   // "H\U0011ABCD", unicode escape error 2                            C11, C3, C8, C9, C7
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'U', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('0', dummy_pos, '0', messages), WantMore);
    //     assert_eq!(parser.input('0', dummy_pos, '1', messages), WantMore);
    //     assert_eq!(parser.input('1', dummy_pos, '1', messages), WantMore);
    //     assert_eq!(parser.input('1', dummy_pos, 'A', messages), WantMore);
    //     assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
    //     assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
    //     assert_eq!(parser.input('C', dummy_pos, 'D', messages), WantMore);
    //     assert_eq!(parser.input('D', spec_pos4, '"', messages), WantMore);
    //     assert_eq!(parser.input('"', spec_pos3, '$', messages),
    //         Finished(None, spec_pos1 + spec_pos3));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
    //             (spec_pos2 + spec_pos4, strings::UnicodeCharEscapeHere.to_owned()),
    //         ], vec![
    //             format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
    //             strings::UnicodeCharEscapeHelpValue.to_owned(),
    //         ])
    //     ]);
    // }

    // {   // "H\u", unexpected EOL in unicode escape                          C11, C3, C5
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('"', spec_pos3, '$', messages), 
    //         Finished(None, spec_pos1 + spec_pos3));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere),
    //             (spec_pos2.into(), strings::UnicodeCharEscapeStartHere),
    //             (spec_pos3.into(), strings::StringLiteralEndHere),
    //         ], vec![
    //             strings::UnicodeCharEscapeHelpSyntax,
    //         ])
    //     ]);
    // }

    // {   // "h\U123$, unexpected EOF in unicode escape                       C11, C3, C8, C12
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('h', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, 'U', messages), WantMoreWithSkip1);
    //     assert_eq!(parser.input('1', dummy_pos, '2', messages), WantMore);
    //     assert_eq!(parser.input('2', dummy_pos, '3', messages), WantMore);
    //     assert_eq!(parser.input('3', dummy_pos, EOF, messages), WantMore);
    //     assert_eq!(parser.input(EOF, spec_pos3, EOF, messages), 
    //         Finished(None, spec_pos1 + spec_pos3));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::new_by_str(strings::UnexpectedEOF, vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere),
    //             (spec_pos3.into(), strings::EOFHere),
    //         ])
    //     ]);
    // }

    // {   // "he\$, unexpected EOF exactly after \                            C11, C4
    //     let mut parser = StringLiteralParser::new(spec_pos1);
    //     let messages = &mut MessageCollection::new();
    //     assert_eq!(parser.input('h', dummy_pos, 'e', messages), WantMore);
    //     assert_eq!(parser.input('e', dummy_pos, '\\', messages), WantMore);
    //     assert_eq!(parser.input('\\', spec_pos2, EOF, messages), WantMore);
    //     assert_eq!(parser.input(EOF, spec_pos3, EOF, messages), 
    //         Finished(None, spec_pos1 + spec_pos3));
        
    //     assert_eq!(messages, &make_messages![
    //         Message::new_by_str(strings::UnexpectedEOF, vec![
    //             (spec_pos1.into(), strings::StringLiteralStartHere),
    //             (spec_pos3.into(), strings::EOFHere),
    //         ])
    //     ]);
    // }
}