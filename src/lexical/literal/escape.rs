///! fff-lang
///!
///!
///! string and char literal's escape char parser

use crate::source::Position;
use crate::diagnostics::{Message, MessageCollection, strings};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct EscapeCharParser {
    expect_size: usize, // \u expect 4 hex, \U expect 8 hex
    buf: String,
    value: u32,
    has_failed: bool
}

#[derive(Debug)]
pub enum EscapeCharSimpleCheckResult {
    Normal(char),
    Invalid(char),
    Unicode(EscapeCharParser),
}
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
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
        let escape_start_span = pos_for_message.0.into();
        let current_span = pos_for_message.1.into();

        match ch.to_digit(16) {
            Some(digit) => {
                self.value += digit << (4 * (self.expect_size - self.buf.len()));   // `digit << (4 * i)` is same as `digit * (16 ** i)`
                
                if self.buf.len() == self.expect_size {
                    match char::from_u32(self.value) {
                        Some(ch) => EscapeCharParserResult::Success(ch),               // C3
                        None => {
                            messages.push(Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
                                (escape_start_span + current_span, strings::UnicodeCharEscapeHere.to_owned()),
                            ], vec![
                                format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, self.buf.clone()),
                                strings::UnicodeCharEscapeHelpValue.to_owned(),
                            ]));
                            EscapeCharParserResult::Failed                             // C4
                        }
                    }
                } else {
                    EscapeCharParserResult::WantMore                                   // C5
                }
            }
            None => {
                messages.push(Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
                    (escape_start_span, strings::UnicodeCharEscapeStartHere),
                    (current_span, strings::UnicodeCharEscapeInvalidChar)
                ], vec![
                    strings::UnicodeCharEscapeHelpSyntax,
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

#[cfg(test)] #[test]
fn escape_char_parser() {
    use crate::source::Span;
    use self::EscapeCharParserResult::*;

    {   // \u2764      => '\u{2764}' | '❤'                          C3, C5, 4
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(0), Position::new(0));
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('7', poss, messages), WantMore);
        assert_eq!(parser.input('6', poss, messages), WantMore);
        assert_eq!(parser.input('4', poss, messages), Success('\u{2764}'));
    }
            
    {   // \U00020E70 => 𠹰                                         C3, C5, 8
        let mut parser = EscapeCharParser::new(8);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(0), Position::new(0));
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
        let poss = (Position::new(0), Position::new(0));
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('0', poss, messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('A', poss, messages), WantMore);
        assert_eq!(parser.input('B', poss, messages), WantMore);
        assert_eq!(parser.input('C', poss, messages), WantMore);
        assert_eq!(parser.input('D', (Position::new(34), Position::new(56)), messages), Failed);

        assert_eq!(messages, &make_messages![
            Message::with_help(strings::InvalidUnicodeCharEscape.to_owned(), vec![
                (Span::new(34, 56), strings::UnicodeCharEscapeHere.to_owned()),
            ], vec![
                format!("{}{}", strings::UnicodeCharEscapeCodePointValueIs, "0011ABCD".to_owned()),
                strings::UnicodeCharEscapeHelpValue.to_owned(),
            ])
        ]);
    }

    {   // \uH123 => early error char                               C1, C2, C6
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(0), Position::new(0));
        assert_eq!(parser.input('H', (Position::new(34), Position::new(78)), messages), WantMore);
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('3', poss, messages), Failed);

        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
                (Span::new(34, 34), strings::UnicodeCharEscapeStartHere),
                (Span::new(78, 78), strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]);
    }

    {   // \u123g => last error char                                C3, C5, C7
        let mut parser = EscapeCharParser::new(4);
        let messages = &mut MessageCollection::new();
        let poss = (Position::new(0), Position::new(0));
        assert_eq!(parser.input('1', poss, messages), WantMore);
        assert_eq!(parser.input('2', poss, messages), WantMore);
        assert_eq!(parser.input('3', poss, messages), WantMore);
        assert_eq!(parser.input('g', (Position::new(65), Position::new(21)), messages), Failed);

        assert_eq!(messages, &make_messages![
            Message::with_help_by_str(strings::InvalidUnicodeCharEscape, vec![
                (Span::new(65, 65), strings::UnicodeCharEscapeStartHere),
                (Span::new(21, 21), strings::UnicodeCharEscapeInvalidChar)
            ], vec![
                strings::UnicodeCharEscapeHelpSyntax,
            ])
        ]);
    }
}