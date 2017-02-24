
// String literal parser

use codepos::Position;
use codepos::StringPosition;
use message::Message;
use message::MessageCollection;
use super::super::symbol_type::string_literal::StringLiteral;
use super::escape_char_parser::EscapeCharParser;
use super::escape_char_parser::EscapeCharSimpleCheckResult;
use super::escape_char_parser::EscapeCharParserResult;
use super::error_strings;

#[cfg(test)]
#[derive(Debug)]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
    escape_parser: Option<EscapeCharParser>, // Not none means is parsing escape
    escape_start_pos: Position,
}
#[cfg(not(test))]
pub struct StringLiteralParser {
    raw: String, 
    start_pos: Position,
    last_escape_quote_pos: Option<Position>,
    has_failed: bool,
    escape_parser: Option<EscapeCharParser>,
    escape_start_pos: Position,
}

// Escape issues about string literal and char literal
// all escapes: \t, \n, \r, \0, \\, \", \', \uxxxx, \Uxxxxxxxx, are all supported in char and string literal
// when meet \, start parsing escape char literal
// if meet EOF, string report unexpected EOF in string, char report unexpected EOF in char
// if meet end of literal, e.g.  '\uAA' or "123\U45678", report incorrect unicode escape error
// specially  
//     '\', char parser will find next is not ' and immediately stop char parser and report a special error for this
//     "\", string parser will record this escape position and this will most probably cause unexpected EOF in string and string parser will report it

#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub enum StringLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(StringLiteral),
}
#[cfg(not(test))]
pub enum StringLiteralParserResult {
    WantMore,
    WantMoreWithSkip1,
    Finished(StringLiteral),
}

impl StringLiteralParser {

    /// new with start position
    pub fn new(start_pos: Position) -> StringLiteralParser {
        StringLiteralParser{
            raw: String::new(),
            start_pos: start_pos, 
            last_escape_quote_pos: None, 
            has_failed: false,
            escape_parser: None,
            escape_start_pos: Position::new(),
        }
    }

    /// Try get string literal, use in state machine of v1, 
    /// ch is none means get none, next_ch is none means next is none
    pub fn input(&mut self, ch: Option<char>, pos: Position, next_ch: Option<char>, messages: &mut MessageCollection) -> StringLiteralParserResult {

        match (ch, pos, next_ch) {
            (Some('\\'), slash_pos, Some(next_ch)) => {
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
                            (StringPosition::double(self.start_pos), error_strings::StringLiteralStartHere.to_owned()),
                            (StringPosition::double(slash_pos), error_strings::UnknownCharEscapeHere.to_owned()),
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
            (Some('\\'), _pos, None) => {                                            // C4, \EOF, ignore
                // Do nothing here, `"abc\udef$` reports EOF in string error, not end of string or EOF in escape error
                return StringLiteralParserResult::WantMore;
            }
            (Some('"'), pos, _1) => {
                // String finished, check if is parsing escape
                match self.escape_parser {
                    Some(ref _parser) => {                                           // C5, \uxxx\EOL, emit error and return
                        // If still parsing, it is absolutely failed
                        messages.push(Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
                            (StringPosition::double(self.start_pos), error_strings::StringLiteralStartHere),
                            (StringPosition::double(self.escape_start_pos), error_strings::UnicodeCharEscapeStartHere),
                            (StringPosition::double(pos), error_strings::StringLiteralEndHere),
                        ], vec![
                            error_strings::UnicodeCharEscapeHelpSyntax,
                        ]));
                        return StringLiteralParserResult::Finished(StringLiteral::new(None, StringPosition::from2(self.start_pos, pos), false));
                    }
                    None => {                                                       // C7, normal EOL, return
                        return StringLiteralParserResult::Finished(StringLiteral::new(
                            if self.has_failed { None } else { Some(self.raw.clone()) }, StringPosition::from2(self.start_pos, pos), false));
                    }
                }
            }
            (Some(ch), pos, _2) => {
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
            (None, pos, _2) => {
                match self.last_escape_quote_pos {                                      // C12: in string, meet EOF, emit error, return 
                    Some(escaped_quote_pos_hint) => 
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::StringLiteralStartHere),
                            (StringPosition::double(pos), error_strings::EOFHere),
                            (StringPosition::double(escaped_quote_pos_hint), error_strings::LastEscapedQuoteHere),
                        ])),
                    None => 
                        messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
                            (StringPosition::double(self.start_pos), error_strings::StringLiteralStartHere),
                            (StringPosition::double(pos), error_strings::EOFHere),
                        ]))
                }
                return StringLiteralParserResult::Finished(StringLiteral::new(None, StringPosition::from2(self.start_pos, pos), false));
            }
        }
    }
}


#[cfg(test)]
#[test]
fn str_lit_parser() {
    use self::StringLiteralParserResult::*;

    let dummy_pos = Position::new();
    let spec_pos1 = make_pos!(12, 34);
    let spec_pos2 = make_pos!(56, 78);
    let spec_pos3 = make_pos!(910, 1112);
    let spec_pos4 = make_pos!(1314, 1516);

    {   // "Hello, world!", most normal,                                    C11, C5, C7
        let mut parser = StringLiteralParser::new(make_pos!(12, 34));
        let messages = &mut MessageCollection::new(); 
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('o'), messages), WantMore);
        assert_eq!(parser.input(Some('o'), dummy_pos, Some('o'), messages), WantMore);
        assert_eq!(parser.input(Some(','), dummy_pos, Some(','), messages), WantMore);
        assert_eq!(parser.input(Some(' '), dummy_pos, Some(' '), messages), WantMore);
        assert_eq!(parser.input(Some('w'), dummy_pos, Some('w'), messages), WantMore);
        assert_eq!(parser.input(Some('o'), dummy_pos, Some('o'), messages), WantMore);
        assert_eq!(parser.input(Some('r'), dummy_pos, Some('r'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('d'), dummy_pos, Some('d'), messages), WantMore);
        assert_eq!(parser.input(Some('!'), dummy_pos, None, messages), WantMore);
        assert_eq!(parser.input(Some('"'), make_pos!(56, 78), None, messages), 
            Finished(StringLiteral::new2("Hello, world!", StringPosition::from4(12, 34, 56, 78), false)));

        assert_eq!(messages, &MessageCollection::new());
    }

    {   // "He$, unexpected end, no last escaped quote hint                 C11, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();  
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(None, spec_pos2, None, messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos2), false)));

        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
            (StringPosition::double(spec_pos2), error_strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "He\"l\"lo$, unexpected EOF, last escaped quote recorded         C11, C1, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('"'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos3, Some('"'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('o'), messages), WantMore);
        assert_eq!(parser.input(Some('o'), dummy_pos, None, messages), WantMore);
        assert_eq!(parser.input(None, spec_pos4, None, messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos4), false)));

        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
            (StringPosition::double(spec_pos4), error_strings::EOFHere),
            (StringPosition::double(spec_pos3), error_strings::LastEscapedQuoteHere),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "H\t\n\0\'\"llo", normal escape                                  C11, C1, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('t'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('n'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('0'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('\''), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('"'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('o'), messages), WantMore);
        assert_eq!(parser.input(Some('o'), dummy_pos, Some('"'), messages), WantMore);
        assert_eq!(parser.input(Some('"'), spec_pos4, Some('$'), messages), 
            Finished(StringLiteral::new2("H\t\n\0\'\"llo", StringPosition::from2(spec_pos1, spec_pos4), false)));

        assert_eq!(messages, expect_messages);
    }

    {   // "h\c\d\e\n\g", error normal escape                               C11, C3, C2
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('c'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), spec_pos3, Some('d'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('e'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), dummy_pos, Some('n'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('\\'), spec_pos3, Some('g'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('"'), spec_pos4, Some('$'), messages),
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos4), false)));

        expect_messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'c'), vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere.to_owned()),
            (StringPosition::double(spec_pos2), error_strings::UnknownCharEscapeHere.to_owned()),
        ]));
        expect_messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'd'), vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere.to_owned()),
            (StringPosition::double(spec_pos3), error_strings::UnknownCharEscapeHere.to_owned()),
        ]));
        expect_messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'e'), vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere.to_owned()),
            (StringPosition::double(spec_pos2), error_strings::UnknownCharEscapeHere.to_owned()),
        ]));
        expect_messages.push(Message::new(format!("{} '\\{}'", error_strings::UnknownCharEscape, 'g'), vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere.to_owned()),
            (StringPosition::double(spec_pos3), error_strings::UnknownCharEscapeHere.to_owned()),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "H\uABCDel", unicode escape                                      C11, C3, C8, C10, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('u'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('A'), dummy_pos, Some('B'), messages), WantMore);
        assert_eq!(parser.input(Some('B'), dummy_pos, Some('C'), messages), WantMore);
        assert_eq!(parser.input(Some('C'), dummy_pos, Some('D'), messages), WantMore);
        assert_eq!(parser.input(Some('D'), dummy_pos, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('"'), messages), WantMore);
        assert_eq!(parser.input(Some('"'), spec_pos3, Some('$'), messages), 
            Finished(StringLiteral::new(Some("H\u{ABCD}el".to_owned()), StringPosition::from2(spec_pos1, spec_pos3), false)));
        
        assert_eq!(messages, expect_messages);
    }

    {   // "H\uABCHel\uABCg", unicode escape error                          C11, C3, C8, C9, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('u'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('A'), dummy_pos, Some('B'), messages), WantMore);
        assert_eq!(parser.input(Some('B'), dummy_pos, Some('C'), messages), WantMore);
        assert_eq!(parser.input(Some('C'), dummy_pos, Some('H'), messages), WantMore);
        assert_eq!(parser.input(Some('H'), spec_pos3, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('l'), messages), WantMore);
        assert_eq!(parser.input(Some('l'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos3, Some('u'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('A'), dummy_pos, Some('B'), messages), WantMore);
        assert_eq!(parser.input(Some('B'), dummy_pos, Some('C'), messages), WantMore);
        assert_eq!(parser.input(Some('C'), dummy_pos, Some('g'), messages), WantMore);
        assert_eq!(parser.input(Some('g'), spec_pos4, Some('"'), messages), WantMore);
        assert_eq!(parser.input(Some('"'), spec_pos4, Some('$'), messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos4), false)));
        
        expect_messages.push(Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (StringPosition::double(spec_pos2), error_strings::UnicodeCharEscapeStartHere),
            (StringPosition::double(spec_pos3), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]));
        expect_messages.push(Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (StringPosition::double(spec_pos3), error_strings::UnicodeCharEscapeStartHere),
            (StringPosition::double(spec_pos4), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "H\U0011ABCD", unicode escape error 2                            C11, C3, C8, C9, C7
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('U'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('0'), dummy_pos, Some('0'), messages), WantMore);
        assert_eq!(parser.input(Some('0'), dummy_pos, Some('1'), messages), WantMore);
        assert_eq!(parser.input(Some('1'), dummy_pos, Some('1'), messages), WantMore);
        assert_eq!(parser.input(Some('1'), dummy_pos, Some('A'), messages), WantMore);
        assert_eq!(parser.input(Some('A'), dummy_pos, Some('B'), messages), WantMore);
        assert_eq!(parser.input(Some('B'), dummy_pos, Some('C'), messages), WantMore);
        assert_eq!(parser.input(Some('C'), dummy_pos, Some('D'), messages), WantMore);
        assert_eq!(parser.input(Some('D'), dummy_pos, Some('"'), messages), WantMore);
        assert_eq!(parser.input(Some('"'), spec_pos3, Some('$'), messages),
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos3), false)));
        
        expect_messages.push(Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (StringPosition::double(spec_pos2), error_strings::UnicodeCharEscapeStartHere.to_owned()),
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD"),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "H\u", unexpected EOL in unicode escape                          C11, C3, C5
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('H'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('u'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('"'), spec_pos3, Some('$'), messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos3), false)));
        
        expect_messages.push(Message::with_help_by_str(error_strings::UnexpectedStringLiteralEnd, vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
            (StringPosition::double(spec_pos2), error_strings::UnicodeCharEscapeStartHere),
            (StringPosition::double(spec_pos3), error_strings::StringLiteralEndHere),
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "h\U123$, unexpected EOF in unicode escape                       C11, C3, C8, C12
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('h'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, Some('U'), messages), WantMoreWithSkip1);
        assert_eq!(parser.input(Some('1'), dummy_pos, Some('2'), messages), WantMore);
        assert_eq!(parser.input(Some('2'), dummy_pos, Some('3'), messages), WantMore);
        assert_eq!(parser.input(Some('3'), dummy_pos, None, messages), WantMore);
        assert_eq!(parser.input(None, spec_pos3, None, messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos3), false)));
        
        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
            (StringPosition::double(spec_pos3), error_strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // "he\$, unexpected EOF exactly after \                            C11, C4
        let mut parser = StringLiteralParser::new(spec_pos1);
        let messages = &mut MessageCollection::new();
        let expect_messages = &mut MessageCollection::new();
        assert_eq!(parser.input(Some('h'), dummy_pos, Some('e'), messages), WantMore);
        assert_eq!(parser.input(Some('e'), dummy_pos, Some('\\'), messages), WantMore);
        assert_eq!(parser.input(Some('\\'), spec_pos2, None, messages), WantMore);
        assert_eq!(parser.input(None, spec_pos3, None, messages), 
            Finished(StringLiteral::new(None, StringPosition::from2(spec_pos1, spec_pos3), false)));
        
        expect_messages.push(Message::new_by_str(error_strings::UnexpectedEOF, vec![
            (StringPosition::double(spec_pos1), error_strings::StringLiteralStartHere),
            (StringPosition::double(spec_pos3), error_strings::EOFHere),
        ]));
        assert_eq!(messages, expect_messages);
    }
}