///! fff-lang
///!
///! string literal parser

use crate::source::{FileSystem, Span, IsId, EOF};
use crate::diagnostics::{Message, strings};
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

    pub(in super::super) fn parse_normal_string_literal(&mut self) -> (Token, Span) {

        let start_position = self.current.1;
        let mut raw = String::new();
        let mut last_escape_quote_position = None; // indicate if string normal end is unexpectedly escaped
        let mut escape_parser = None;
        let mut has_failed = false;

        self.eat(); // eat beginning double quote
        loop {
            match (self.current.0, self.peek1.0) {
                ('\\', EOF) => {                                                // C4, \EOF, ignore
                    // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                    self.eat();
                }
                ('\\', next_ch) => {
                    match EscapeCharParser::simple_check(next_ch) {
                        EscapeCharSimpleCheckResult::Normal(ch) => {                    // C1, normal escape
                            raw.push(ch);
                            if ch == '"' {
                                last_escape_quote_position = Some(self.current.1);
                            }
                            self.eat();
                            self.eat();
                        } 
                        EscapeCharSimpleCheckResult::Invalid(ch) => {                   // C2, error normal escape, emit error and continue
                            self.diagnostics.push(Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, ch), vec![
                                (start_position.into(), strings::StringLiteralStartHere.to_owned()),
                                (self.current.1.into(), strings::UnknownCharEscapeHere.to_owned()),
                            ]));
                            has_failed = true;
                            self.eat();
                            self.eat();
                        }
                        EscapeCharSimpleCheckResult::Unicode(more) => {               // C3, start unicode escape
                            escape_parser = Some((more, self.current.1));
                            self.eat();
                            self.eat();
                        }
                    }
                }
                ('"', _2) => {
                    // String finished, check if is parsing escape
                    if let Some((_, escape_start_position)) = escape_parser {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        self.diagnostics.push(Message::with_help_by_str(strings::UnexpectedStringLiteralEnd, vec![
                            (start_position.into(), strings::StringLiteralStartHere),
                            (escape_start_position.into(), strings::UnicodeCharEscapeStartHere),
                            (self.current.1.into(), strings::StringLiteralEndHere),
                        ], vec![
                            strings::UnicodeCharEscapeHelpSyntax,
                        ]));
                        let span = start_position + self.current.1;
                        self.eat();
                        return (Token::Str(IsId::new(1), StringLiteralType::Normal), span);
                    } else {                      
                        let span = start_position + self.current.1;                            // C7, normal end, return
                        self.eat();
                        return (Token::Str(if has_failed { IsId::new(1) } else { self.chars.intern(&raw) }, StringLiteralType::Normal), span);
                    }
                }
                (EOF, _2) => {
                    match last_escape_quote_position {                                      // C12: in string, meet EOF, emit error, return 
                        Some(escaped_quote_pos_hint) => 
                            self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                                (start_position.into(), strings::StringLiteralStartHere),
                                (self.current.1.into(), strings::EOFHere),
                                (escaped_quote_pos_hint.into(), strings::LastEscapedQuoteHere),
                            ])),
                        None => 
                            self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                                (start_position.into(), strings::StringLiteralStartHere),
                                (self.current.1.into(), strings::EOFHere),
                            ]))
                    }
                    let span = start_position + self.current.1;
                    self.eat();
                    return (Token::Str(IsId::new(1), StringLiteralType::Normal), span);
                }
                (ch, _2) => {
                    // Normal in string
                    let mut need_reset_escape_parser = false;
                    match escape_parser {
                        Some((ref mut escape_parser, escape_start_position)) => {
                            match escape_parser.input(ch, (escape_start_position, self.current.1), self.diagnostics) {
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
                    self.eat();
                }
            }
        }
    }

    pub(in super::super) fn parse_raw_string_literal(&mut self) -> (Token, Span) {
        let start_position = self.current.1;
        let mut raw = String::new();
        self.eat(); // eat beginning r
        self.eat(); // eat beginning quote
        loop {
            match self.current.0 {
                '"' => {                                               // C1: in raw string, meet ", finish, return
                    let span = start_position + self.current.1;
                    self.eat();
                    return (Token::Str(self.chars.intern(&raw), StringLiteralType::Raw), span);
                }
                EOF => {                                                    // C3: in raw string, meet EOF, emit error, return  
                    self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![ 
                        (start_position.into(), strings::StringLiteralStartHere),
                        (self.current.1.into(), strings::EOFHere),
                    ]));
                    let span = start_position + self.current.1;
                    self.eat();
                    return (Token::Str(IsId::new(1), StringLiteralType::Raw), span);
                }
                other => {                                                 // C2: in raw string, meet other, continue
                    raw.push(other);
                    self.eat();
                }
            }
        }
    }
}
