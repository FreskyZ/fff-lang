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
