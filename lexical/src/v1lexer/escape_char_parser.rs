
// Escape char parser

use codemap::Position;
use codemap::StringPosition;
use message::Message;
use message::MessageCollection;

use super::error_strings;

#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub struct EscapeCharParser {
    expect_size: usize, // \u expect 4 hex, \U expect 8 hex
    buf: String,
    value: u32,
    has_failed: bool
}
#[cfg(not(test))]
pub struct EscapeCharParser {
    expect_size: usize, // \u expect 4 hex, \U expect 8 hex
    buf: String,
    value: u32,
    has_failed: bool
}

pub enum EscapeCharSimpleCheckResult {
    Normal(char),
    Invalid(char),
    Unicode(EscapeCharParser),
}
#[cfg(test)]
#[derive(Debug, Eq, PartialEq)]
pub enum EscapeCharParserResult {
    WantMore,               // Sucess and want more or unexpected char but keep until got 4 or 8 char
    Failed,                 // Unexpected char fail, or, u32 value is not char fail, finished, error emitted
    Success(char),          // Succeed and result
}
#[cfg(not(test))]
pub enum EscapeCharParserResult {
    WantMore,               // Sucess and want more or unexpected char but keep until got 4 or 8 char
    Failed,                 // Unexpected char fail, or, u32 value is not char fail, finished, error emitted
    Success(char),          // Succeed and result
}

// 16(F plus 1) powered
// 17/2/23: Now use shift left instead
// const FP1_POWERED: [u32; 8] = [1_u32, 0x10_u32, 0x100_u32, 0x1000_u32, 0x1000_0_u32, 0x1000_00_u32, 0x1000_000_u32, 0x1000_0000_u32];

use std::char;
impl EscapeCharParser {

    fn new(expect_size: usize) -> EscapeCharParser {
        EscapeCharParser{ 
            expect_size: expect_size, 
            buf: String::new(),
            value: 0, 
            has_failed: false,
        }
    }

    // \t, \n, \r, \0, \\, \", \', \uxxxx, \Uxxxxxxxx, are all supported in char and string literal
    pub fn simple_check(next_ch: char) -> EscapeCharSimpleCheckResult {
        match next_ch {
            't' => EscapeCharSimpleCheckResult::Normal('\t'),
            'n' => EscapeCharSimpleCheckResult::Normal('\n'),
            'r' => EscapeCharSimpleCheckResult::Normal('\r'),
            '0' => EscapeCharSimpleCheckResult::Normal('\0'),
            '\\' => EscapeCharSimpleCheckResult::Normal('\\'),
            '"' => EscapeCharSimpleCheckResult::Normal('"'),
            '\'' => EscapeCharSimpleCheckResult::Normal('\''),
            'a' => EscapeCharSimpleCheckResult::Normal('\u{7}'),
            'b' => EscapeCharSimpleCheckResult::Normal('\u{8}'),
            'f' => EscapeCharSimpleCheckResult::Normal('\u{C}'),
            'v' => EscapeCharSimpleCheckResult::Normal('\u{B}'),
            'u' => EscapeCharSimpleCheckResult::Unicode(EscapeCharParser::new(4)),
            'U' => EscapeCharSimpleCheckResult::Unicode(EscapeCharParser::new(8)),
            other => EscapeCharSimpleCheckResult::Invalid(other),
        }
    }

    // pos for message: (escape_start_pos, current_pos)
    /// ATTENTION: if returned finished but continue input, may cause algorithm overflow panic
    pub fn input(&mut self, ch: char, pos_for_message: (Position, Position), messages: &mut MessageCollection) -> EscapeCharParserResult {
        
        self.buf.push(ch);   // because expect size check rely on buf length, so push regardless will happen
        if self.has_failed {
            if self.buf.len() < self.expect_size {
                return EscapeCharParserResult::WantMore;                               // C1
            } else {
                return EscapeCharParserResult::Failed;                                 // C2
            }
        }

        match ch.to_digit(16) {
            Some(digit) => {
                self.value += digit << (4 * (self.expect_size - self.buf.len()));   // `digit << (4 * i)` is same as `digit * (16 ** i)`
                
                if self.buf.len() == self.expect_size {
                    match char::from_u32(self.value) {
                        Some(ch) => EscapeCharParserResult::Success(ch),               // C3
                        None => {
                            messages.push(Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
                                (StringPosition::double(pos_for_message.0), error_strings::UnicodeCharEscapeStartHere.to_owned()),
                            ], vec![
                                format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, self.buf.clone()),
                                error_strings::UnicodeCharEscapeHelpValue.to_owned(),
                            ]));
                            EscapeCharParserResult::Failed                             // C4
                        }
                    }
                } else {
                    EscapeCharParserResult::WantMore                                   // C5
                }
            }
            None => {
                messages.push(Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
                    (StringPosition::double(pos_for_message.0), error_strings::UnicodeCharEscapeStartHere),
                    (StringPosition::double(pos_for_message.1), error_strings::UnicodeCharEscapeInvalidChar)
                ], vec![
                    error_strings::UnicodeCharEscapeHelpSyntax,
                ]));
                self.has_failed = true;
                if self.buf.len() < self.expect_size {
                    EscapeCharParserResult::WantMore                                   // C6
                } else {
                    EscapeCharParserResult::Failed                                     // C7
                }
            }
        }
    }
}

#[cfg(test)]
#[test]
fn escape_char_parser() {
    use self::EscapeCharParserResult::*;

    {   // \u2764      => '\u{2764}' | '❤'                          C3, C5, 4
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(), Position::new());
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('7', poss, messages), WantMore);
        assert_eq!(parser.input('6', poss, messages), WantMore);
        assert_eq!(parser.input('4', poss, messages), Success('\u{2764}'));
    }
            
    {   // \U00020E70 => 𠹰                                         C3, C5, 8
        let mut parser = EscapeCharParser::new(8);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(), Position::new());
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('E', poss, messages), WantMore);
        assert_eq!(parser.input('7', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), Success('\u{20E70}'));
    }

    {   // \U0011ABCD => out of range                               C3, C4, C5
        let mut parser = EscapeCharParser::new(8);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(), Position::new());
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('A', poss, messages), WantMore);
        assert_eq!(parser.input('B', poss, messages), WantMore);
        assert_eq!(parser.input('C', poss, messages), WantMore);
        assert_eq!(parser.input('D', (make_pos!(12, 34), Position::new()), messages), Failed);

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help(error_strings::InvalidUnicodeCharEscape.to_owned(), vec![
            (make_str_pos!(12, 34, 12, 34), error_strings::UnicodeCharEscapeStartHere.to_owned()),
        ], vec![
            format!("{}{}", error_strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
            error_strings::UnicodeCharEscapeHelpValue.to_owned(),
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // \uH123 => early error char                               C1, C2, C6
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(), Position::new());
        assert_eq!(parser.input('H', (make_pos!(12, 34), make_pos!(56, 78)), messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('3', poss, messages), Failed);

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (make_str_pos!(12, 34, 12, 34), error_strings::UnicodeCharEscapeStartHere),
            (make_str_pos!(56, 78, 56, 78), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]));
        assert_eq!(messages, expect_messages);
    }

    {   // \u123g => last error char                                C3, C5, C7
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(), Position::new());
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('3', poss, messages), WantMore);
        assert_eq!(parser.input('g', (make_pos!(78, 65), make_pos!(43, 21)), messages), Failed);

        let expect_messages = &mut MessageCollection::new();
        expect_messages.push(Message::with_help_by_str(error_strings::InvalidUnicodeCharEscape, vec![
            (make_str_pos!(78, 65, 78, 65), error_strings::UnicodeCharEscapeStartHere),
            (make_str_pos!(43, 21, 43, 21), error_strings::UnicodeCharEscapeInvalidChar)
        ], vec![
            error_strings::UnicodeCharEscapeHelpSyntax,
        ]));
        assert_eq!(messages, expect_messages);
    }
}