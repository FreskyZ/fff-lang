///! fff-lang
///!
///! Character literal parser

use crate::source::{FileSystem, Position, Span, EOF};
use crate::diagnostics::{Message, strings};
use super::escape::{EscapeCharParser, EscapeCharSimpleCheckResult, EscapeCharParserResult};
use super::super::{Parser, Token};

impl<'ecx, 'scx, F> Parser<'ecx, 'scx, F> where F: FileSystem {
    
    pub(in super::super) fn parse_char_literal(&mut self) -> (Token, Span) {

        let mut all_span = self.current.1.into();
        let mut escape_parser: Option<(EscapeCharParser, Position)> = None;
        let mut has_failed = false;
        let mut prepare_to_too_long = false;
        let mut raw = String::new(); // allow arbitrary length char literal when parsing after raise error after that

        self.eat();
        loop {
            if raw.len() == 0 {       // Waiting for first char
                let mut need_reset_parser = false;
                let mut need_set_parser = false;
                let mut need_set_parser_value = None;
                match (&mut escape_parser, self.current.0, self.peek1.0) {
                    (&mut Some(_), EOF, _3) => {  // another 'u123$  // C4, first char is EOF
                        self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (all_span, strings::CharLiteralHere),
                            (self.current.1.into(), strings::EOFHere)
                        ]));
                        self.eatnc();
                        return (Token::Char('\0'), all_span);
                    }
                    (&mut Some((ref mut escape_parser, escape_start_position)), ch, _3) => {
                        all_span += self.current.1;
                        if ch == '\'' { // '\'' should report unexpected EOL
                            self.diagnostics.push(Message::with_help_by_str(strings::UnexpectedCharLiteralEnd, vec![
                                (all_span, strings::CharLiteralHere),
                            ], vec![
                                strings::UnicodeCharEscapeHelpSyntax
                            ]));
                            self.eatnc();
                            return (Token::Char('\0'), all_span);
                        }

                        match escape_parser.input(ch, (escape_start_position, self.current.1), self.diagnostics) {
                            EscapeCharParserResult::WantMore => { // C1, in first char as unicode escape, continue
                            },    
                            EscapeCharParserResult::Failed => {        // Invalid unicode escape, message already emitted, return INVALID_CHAR
                                raw.push('\0');
                                has_failed = true;
                                need_reset_parser = true; // C2, in first char as unicode escape, 
                            }
                            EscapeCharParserResult::Success(ch) => {
                                raw.push(ch);   // Success, waiting for '
                                need_reset_parser = true;// C3, in first char as unicode escape, success
                            }
                        }
                        // WantMore  // wait to reset until out of match self.escape_parser
                    }
                    (&mut None, '\'', _3) => { // C5, empty
                        all_span += self.current.1;
                        self.diagnostics.push(Message::with_help_by_str(strings::EmptyCharLiteral, vec![
                            (all_span, strings::CharLiteralHere),
                        ], vec![
                            strings::CharLiteralSyntaxHelp1
                        ]));
                        self.eatnc();
                        return (Token::Char('\0'), all_span);
                    }
                    (&mut None, EOF, _3) => {  // C6, '$, report EOF in char literal
                        self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (all_span, strings::CharLiteralHere),
                            (self.current.1.into(), strings::EOFHere)
                        ]));
                        self.eatnc();
                        return (Token::Char('\0'), all_span);
                    }
                    (&mut None, '\\', EOF) => {                // C10, '\$
                        all_span += self.current.1;
                        let eof_pos = self.current.1.offset(1); // in this case, `\` is always 1 byte wide
                        self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (all_span, strings::CharLiteralHere),
                            (eof_pos.into(), strings::EOFHere)
                        ]));
                        self.eatnc();
                        return (Token::Char('\0'), all_span);
                    }
                    (&mut None, '\\', next_ch) => {   // if is escape, try escape
                        all_span += self.current.1.offset(1); // `\`
                        match EscapeCharParser::simple_check(next_ch) {
                            EscapeCharSimpleCheckResult::Normal(ch) => {
                                raw.push(ch); // C7, normal simple escape
                                self.eatnc();
                                self.eatnc();
                                continue;
                            }
                            EscapeCharSimpleCheckResult::Invalid(ch) => {
                                self.diagnostics.push(Message::new(format!("{} '\\{}'", strings::UnknownCharEscape, ch), vec![
                                    (all_span.start.into(), strings::CharLiteralStartHere.to_owned()),
                                    (self.current.1.into(), strings::UnknownCharEscapeHere.to_owned()),
                                ]));
                                raw.push('\0');
                                has_failed = true; // C8, invalid simple escape
                                self.eatnc();
                                self.eatnc();
                                continue;
                            }
                            EscapeCharSimpleCheckResult::Unicode(p) => {
                                need_set_parser = true;
                                need_set_parser_value = Some((p, self.current.1));        // C9, start unicode parser for first char
                            }
                        }
                    }
                    (&mut None, ch, _3) => {
                        raw.push(ch);
                        all_span += self.current.1;               // C11, most normal a char
                        self.eatnc();
                        continue;
                    }
                }
                if need_reset_parser {
                    escape_parser = None;                                   // C12, reset escape parser, only and must with C2, C3
                }
                if need_set_parser {  // C9 fix 
                    escape_parser = need_set_parser_value;
                    self.eatnc();
                    self.eatnc();
                } else {
                    self.eatnc();
                }                                       // C13, only and must with C1, C2, C3
            } else {  // Already processed first char
                    // No possibility for a unicode parser here, just wait for a ', if not, report too long
                match self.current.0 {
                    EOF => {
                        self.diagnostics.push(Message::new_by_str(strings::UnexpectedEOF, vec![
                            (all_span, strings::CharLiteralHere),
                            (self.current.1.into(), strings::EOFHere)
                        ]));                               // C14, 'ABCD$
                        self.eatnc();
                        return (Token::Char('\0'), all_span);
                    }
                    '\'' => { // Normally successed                           // C15, most normal finish
                        let all_span = all_span + self.current.1;
                        if prepare_to_too_long {                       // C19, actual report too long
                            self.diagnostics.push(Message::new_by_str(strings::CharLiteralTooLong, vec![
                                (all_span, strings::CharLiteralHere),
                            ]));
                        }
                        self.eatnc();
                        return (if !has_failed && !prepare_to_too_long { Token::Char(raw.chars().next().unwrap()) } else { Token::Char('\0') }, all_span);
                    }
                    _ => {
                        if !prepare_to_too_long { // 'AB... is too long, report only once                       // C16, too long and prepare to report
                            prepare_to_too_long = true;
                            has_failed = true;     // if too longed, devalidate the buffer
                        }
                        all_span += self.current.1;                           // C17, too long and return
                        self.eatnc();
                    }
                }
            }
        }
    }
}
