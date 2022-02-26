///! fff-lang
///!
///! string literal parser

use crate::source::{CharPos, Span, EOF_CHAR};
use crate::diagnostics::{Message, MessageCollection};
use super::escape_char_parser::{EscapeCharParser, EscapeCharSimpleCheckResult, EscapeCharParserResult};
use super::error_strings;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub enum StringLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(Option<String>, Span),
}

// Escape issues about string literal and char literal
// all escapes: \t, \n, \r, \0, \\, \", \', \uxxxx, \Uxxxxxxxx, are all supported in char and string literal
// when meet \, start parsing escape char literal
// if meet EOF, string report unexpected EOF in string, char report unexpected EOF in char
// if meet end of literal, e.g.  '\uAA' or "123\U45678", report incorrect unicode escape error
// specially  
//     '\', char parser will find next is not ' and immediately stop char parser and report a special error for this
//     "\", string parser will record this escape position and this will most probably cause unexpected EOF in string and string parser will report it

#[cfg_attr(test, derive(Debug))]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: CharPos,
    last_escape_quote_pos: Option<CharPos>,
    has_failed: bool,
    escape_parser: Option<EscapeCharParser>, // Not none means is parsing escape
    escape_start_pos: CharPos,
}
impl StringLiteralParser {

    /// new with start position
    pub fn new(start_pos: CharPos) -> StringLiteralParser {
        StringLiteralParser{
            raw: String::new(),
            start_pos,
            last_escape_quote_pos: None, 
            has_failed: false,
            escape_parser: None,
            escape_start_pos: CharPos::default(),
        }
    }

    /// Try get string literal, use in state machine of v1, 
    pub fn input(&mut self, ch: char, pos: CharPos, next_ch: char, messages: &mut MessageCollection) -> StringLiteralParserResult {

        match (ch, pos, next_ch) {
            ('\\', _1, EOF_CHAR) => {                                                // C4, \EOF, ignore
                // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                return StringLiteralParserResult::WantMore;
            }
            ('\\', slash_pos, next_ch) => {
                match EscapeCharParser::simple_check(next_ch) {
                    EscapeCharSimpleCheckResult::Normal(ch) => {                    // C1, normal escape
                        self.raw.push(ch);
                        if ch == '"' {
                            self.last_escape_quote_pos = Some(slash_pos);
                        }
                        return StringLiteralParserResult::WantMoreWithSkip1;
                    } 
                    EscapeCharSimpleCheckResult::Invalid(ch) => {                   // C2, error normal escape, emit error and continue
                        messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, ch), vec![
                            (self.start_pos.as_span(), error_strings::StringLiteralStartHere.to_owned()),
                            (slash_pos.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
                        ]));
                        self.has_failed = true;
                        return StringLiteralParserResult::WantMoreWithSkip1;
                    }
                    EscapeCharSimpleCheckResult::Unicode(parser) => {               // C3, start unicode escape
                        self.escape_start_pos = slash_pos;
                        self.escape_parser = Some(parser);
                        return StringLiteralParserResult::WantMoreWithSkip1;
                    }
                }
            }
            ('"', pos, _2) => {
                // String finished, check if is parsing escape
                match self.escape_parser {
                    Some(ref _parser) => {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        messages.push(Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
                            (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
                            (self.escape_start_pos.as_span(), error_strings::UnicodeCharEscapeStartHere),
                            (pos.as_span(), error_strings::StringLiteralEndHere),
                        ], vec![
                            error_strings::UnicodeCharEscapeHelpSyntax,
                        ]));
                        return StringLiteralParserResult::Finished(None, self.start_pos.merge(&pos));
                    }
                    None => {                                                       // C7, normal EOL, return
                        return StringLiteralParserResult::Finished(
                            if self.has_failed { None } else { Some(self.raw.clone()) }, 
                            self.start_pos.merge(&pos)
                        );
                    }
                }
            }
            (EOF_CHAR, pos, _2) => {
                match self.last_escape_quote_pos {                                      // C12: in string, meet EOF, emit error, return 
                    Some(escaped_quote_pos_hint) => 
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
                            (pos.as_span(), error_strings::EOFHere),
                            (escaped_quote_pos_hint.as_span(), error_strings::LastEscapedQuoteHere),
                        ])),
                    None => 
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (self.start_pos.as_span(), error_strings::StringLiteralStartHere),
                            (pos.as_span(), error_strings::EOFHere),
                        ]))
                }
                return StringLiteralParserResult::Finished(None, self.start_pos.merge(&pos));
            }
            (ch, pos, _2) => {
                // Normal in string
                let mut need_reset_escape_parser = false;
                match self.escape_parser {
                    Some(ref mut parser) => {
                        match parser.input(ch, (self.escape_start_pos, pos), messages) {
                            EscapeCharParserResult::WantMore => (),            // C8, in unicode escape, (may be fail and) want more
                            EscapeCharParserResult::Failed => {
                                self.has_failed = true;
                                need_reset_escape_parser = true;                    // C9, in unicode escape, not hex char or last not unicode codepoint value, finish
                            }
                            EscapeCharParserResult::Success(ch) => {                // C10, in unicode escape, success, finish
                                need_reset_escape_parser = true;
                                self.raw.push(ch);
                            }
                        }
                    }
                    None => {
                        self.raw.push(ch);                                          // C11, most plain
                    }
                }
                if need_reset_escape_parser {  
                    self.escape_parser = None;
                }
                return StringLiteralParserResult::WantMore;
            }
        }
    }
}


#[cfg(test)] #[test]
fn str_lit_parse() {
    use self::StringLiteralParserResult::*;

    let dummy_pos = CharPos::default();
    let spec_pos1 = make_charpos!(34);
    let spec_pos2 = make_charpos!(78);
    let spec_pos3 = make_charpos!(1112);
    let spec_pos4 = make_charpos!(1516);

    {   // "Hello, world!", most normal,                                    C11, C5, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new(); 
        assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, 'o', messages), WantMore);
        assert_eq!(parser.input(',', dummy_pos, ',', messages), WantMore);
        assert_eq!(parser.input(' ', dummy_pos, ' ', messages), WantMore);
        assert_eq!(parser.input('w', dummy_pos, 'w', messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, 'o', messages), WantMore);
        assert_eq!(parser.input('r', dummy_pos, 'r', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('d', dummy_pos, 'd', messages), WantMore);
        assert_eq!(parser.input('!', dummy_pos, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input('"', spec_pos2, EOF_CHAR, messages), 
            Finished(Some("Hello, world!".to_owned()), spec_pos1.merge(&spec_pos2)));

        assert_eq!(messages, &MessageCollection::new());
    }

    {   // "He$, unexpected end, no last escaped quote hint                 C11, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos2, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos2)));
        
        assert_eq!(messages, &make_messages![
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                (spec_pos2.as_span(), error_strings::EOFHere),
            ])
        ]);
    }

    {   // "He\"l\"lo$, unexpected EOF, last escaped quote recorded         C11, C1, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, '"', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('l', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos3, '"', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos4, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos4)));

        assert_eq!(messages, &make_messages![
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                (spec_pos4.as_span(), error_strings::EOFHere),
                (spec_pos3.as_span(), error_strings::LastEscapedQuoteHere),
            ])
        ]);
    }

    {   // "H\t\n\0\'\"llo", normal escape                                  C11, C1, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', dummy_pos, 't', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', dummy_pos, 'n', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', dummy_pos, '0', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', dummy_pos, '\'', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', dummy_pos, '"', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('l', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, 'o', messages), WantMore);
        assert_eq!(parser.input('o', dummy_pos, '"', messages), WantMore);
        assert_eq!(parser.input('"', spec_pos4, '$', messages), 
            Finished(Some("H\t\n\0\'\"llo".to_owned()), spec_pos1.merge(&spec_pos4)));

        assert_eq!(messages, &make_messages![]);
    }

    {   // "h\c\d\e\n\g", error normal escape                               C11, C3, C2
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'c', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', spec_pos3, 'd', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', spec_pos2, 'e', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', dummy_pos, 'n', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('\\', spec_pos3, 'g', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('"', spec_pos4, '$', messages),
            Finished(None, spec_pos1.merge(&spec_pos4)));

        assert_eq!(messages, &make_messages![
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere.to_owned()),
                (spec_pos2.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
            ]),
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'd'), vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere.to_owned()),
                (spec_pos3.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
            ]),
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'e'), vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere.to_owned()),
                (spec_pos2.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
            ]),
            Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'g'), vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere.to_owned()),
                (spec_pos3.as_span(), error_strings::UnknownCharEscapeHere.to_owned()),
            ])
        ]);
    }

    {   // "H\uABCDel", unicode escape                                      C11, C3, C8, C10, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
        assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
        assert_eq!(parser.input('C', dummy_pos, 'D', messages), WantMore);
        assert_eq!(parser.input('D', dummy_pos, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, '"', messages), WantMore);
        assert_eq!(parser.input('"', spec_pos3, '$', messages), 
            Finished(Some("H\u{ABCD}el".to_owned()), spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![]);
    }

    {   // "H\uABCHel\uABCg", unicode escape error                          C11, C3, C8, C9, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
        assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
        assert_eq!(parser.input('C', dummy_pos, 'H', messages), WantMore);
        assert_eq!(parser.input('H', spec_pos3, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, 'l', messages), WantMore);
        assert_eq!(parser.input('l', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos3, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
        assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
        assert_eq!(parser.input('C', dummy_pos, 'g', messages), WantMore);
        assert_eq!(parser.input('g', spec_pos4, '"', messages), WantMore);
        assert_eq!(parser.input('"', spec_pos4, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos4)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (spec_pos2.as_span(), error_strings::UnicodeCharEscapeStartHere),
                (spec_pos3.as_span(), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ]),
            Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                (spec_pos3.as_span(), error_strings::UnicodeCharEscapeStartHere),
                (spec_pos4.as_span(), error_strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]);
    }

    {   // "H\U0011ABCD", unicode escape error 2                            C11, C3, C8, C9, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'U', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('0', dummy_pos, '0', messages), WantMore);
        assert_eq!(parser.input('0', dummy_pos, '1', messages), WantMore);
        assert_eq!(parser.input('1', dummy_pos, '1', messages), WantMore);
        assert_eq!(parser.input('1', dummy_pos, 'A', messages), WantMore);
        assert_eq!(parser.input('A', dummy_pos, 'B', messages), WantMore);
        assert_eq!(parser.input('B', dummy_pos, 'C', messages), WantMore);
        assert_eq!(parser.input('C', dummy_pos, 'D', messages), WantMore);
        assert_eq!(parser.input('D', spec_pos4, '"', messages), WantMore);
        assert_eq!(parser.input('"', spec_pos3, '$', messages),
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (spec_pos2.merge(&spec_pos4), error_strings::UnicodeCharEscapeHere.to_owned()),
            ], vec![
                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]);
    }

    {   // "H\u", unexpected EOL in unicode escape                          C11, C3, C5
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('H', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'u', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('"', spec_pos3, '$', messages), 
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                (spec_pos2.as_span(), error_strings::UnicodeCharEscapeStartHere),
                (spec_pos3.as_span(), error_strings::StringLiteralEndHere),
            ], vec![
                error_strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]);
    }

    {   // "h\U123$, unexpected EOF in unicode escape                       C11, C3, C8, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, 'U', messages), WantMoreWithSkip1);
        assert_eq!(parser.input('1', dummy_pos, '2', messages), WantMore);
        assert_eq!(parser.input('2', dummy_pos, '3', messages), WantMore);
        assert_eq!(parser.input('3', dummy_pos, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos3, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                (spec_pos3.as_span(), error_strings::EOFHere),
            ])
        ]);
    }

    {   // "he\$, unexpected EOF exactly after \                            C11, C4
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        assert_eq!(parser.input('h', dummy_pos, 'e', messages), WantMore);
        assert_eq!(parser.input('e', dummy_pos, '\\', messages), WantMore);
        assert_eq!(parser.input('\\', spec_pos2, EOF_CHAR, messages), WantMore);
        assert_eq!(parser.input(EOF_CHAR, spec_pos3, EOF_CHAR, messages), 
            Finished(None, spec_pos1.merge(&spec_pos3)));
        
        assert_eq!(messages, &make_messages![
            Message::new_by_str(error_strings::UnexpectedEOF, vec![
                (spec_pos1.as_span(), error_strings::StringLiteralStartHere),
                (spec_pos3.as_span(), error_strings::EOFHere),
            ])
        ]);
    }
}