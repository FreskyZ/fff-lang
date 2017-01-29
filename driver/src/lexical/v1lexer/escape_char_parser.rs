
// Escape char parser

use common::Position;
use message::LexicalMessage as Message;
use message::MessageEmitter;

test_only_attr!{
    [derive(Debug, Eq, PartialEq)]
    ![]
    pub struct EscapeCharParser {
        expect_size: usize, // \u expect 4 hex, \U expect 8 hex
        temp: String,
        value: u32,
        has_failed: bool
    }
}

pub enum EscapeCharSimpleCheckResult {
    Normal(char),
    Invalid(char),
    Unicode(EscapeCharParser),
}

test_only_attr!{
    [derive(Debug, Eq, PartialEq)]
    ![]
    pub enum EscapeCharParserResult {
        WantMore,               // Sucess and want more or unexpected char but keep until got 4 or 8 char
        Failed,                 // Unexpected char fail, or, u32 value is not char fail, finished, error emitted
        Success(char),          // Succeed and result
    }
}

// 16(F plus 1) powered
const FP1_POWERED: [u32; 8] = 
    [1_u32, 0x10_u32, 0x100_u32, 0x1000_u32, 0x1000_0_u32, 0x1000_00_u32, 0x1000_000_u32, 0x1000_0000_u32];

use std::char;
impl EscapeCharParser {

    fn new(expect_size: usize) -> EscapeCharParser {
        EscapeCharParser{ 
            expect_size: expect_size, 
            temp: String::new(),
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
    pub fn input(&mut self, ch: char, pos_for_message: (Position, Position), messages: &mut MessageEmitter) -> EscapeCharParserResult {
        
        self.temp.push(ch);   // because expect size check rely on temp length, so push regardless will happen
        if self.has_failed {
            if self.temp.len() < self.expect_size {
                return EscapeCharParserResult::WantMore;                               // C1
            } else {
                return EscapeCharParserResult::Failed;                                 // C2
            }
        }

        match ch.to_digit(16) {
            Some(digit) => {
                self.value += digit * FP1_POWERED[self.expect_size - self.temp.len()];
                
                if self.temp.len() == self.expect_size {
                    match char::from_u32(self.value) {
                        Some(ch) => EscapeCharParserResult::Success(ch),               // C3
                        None => {
                            messages.push(Message::IncorrectUnicodeCharEscapeValue {
                                escape_start: pos_for_message.0,
                                raw_value: self.temp.clone(), 
                            });
                            EscapeCharParserResult::Failed                             // C4
                        }
                    }
                } else {
                    EscapeCharParserResult::WantMore                                   // C5
                }
            }
            None => {
                messages.push(Message::UnexpectedCharInUnicodeCharEscape {
                    escape_start: pos_for_message.0,
                    unexpected_char_pos: pos_for_message.1,
                    unexpected_char: ch,        
                });
                self.has_failed = true;
                if self.temp.len() < self.expect_size {
                    EscapeCharParserResult::WantMore                                   // C6
                } else {
                    EscapeCharParserResult::Failed                                     // C7
                }
            }
        }
    }
}

#[cfg(test)]
pub fn escape_char_parser_new(expect_size: usize) -> EscapeCharParser {
    EscapeCharParser::new(expect_size)
}

#[cfg(test)]
mod tests {
    
    #[test]
    fn escape_char_parser_test() {
        use super::escape_char_parser_new;
        use super::EscapeCharParserResult::*;
        use common::Position;
        use message::LexicalMessage as Message;
        use message::MessageEmitter;

        {   // \u2764      => '\u{2764}' | '❤'                          C3, C5, 4
            let mut parser = escape_char_parser_new(4);
            let messages = &mut MessageEmitter::new();
            let poss = (Position::new(), Position::new());
            assert_eq!(parser.input('2', poss, messages), WantMore);
            assert_eq!(parser.input('7', poss, messages), WantMore);
            assert_eq!(parser.input('6', poss, messages), WantMore);
            assert_eq!(parser.input('4', poss, messages), Success('\u{2764}'));
        }
                
        {   // \U00020E70 => 𠹰                                         C3, C5, 8
            let mut parser = escape_char_parser_new(8);
            let messages = &mut MessageEmitter::new();
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
            let mut parser = escape_char_parser_new(8);
            let messages = &mut MessageEmitter::new();
            let poss = (Position::new(), Position::new());
            assert_eq!(parser.input('0', poss, messages), WantMore);
            assert_eq!(parser.input('0', poss, messages), WantMore);
            assert_eq!(parser.input('1', poss, messages), WantMore);
            assert_eq!(parser.input('1', poss, messages), WantMore);
            assert_eq!(parser.input('A', poss, messages), WantMore);
            assert_eq!(parser.input('B', poss, messages), WantMore);
            assert_eq!(parser.input('C', poss, messages), WantMore);
            assert_eq!(parser.input('D', (Position::from((12, 34)), Position::new()), messages), Failed);

            let expect_messages = &mut MessageEmitter::new();
            expect_messages.push(Message::IncorrectUnicodeCharEscapeValue{ escape_start: Position::from((12, 34)), raw_value: "0011ABCD".to_owned() });
            assert_eq!(messages, expect_messages);
        }

        {   // \uH123 => early error char                               C1, C2, C6
            let mut parser = escape_char_parser_new(4);
            let messages = &mut MessageEmitter::new();
            let poss = (Position::new(), Position::new());
            assert_eq!(parser.input('H', (Position::from((12, 34)), Position::from((56, 78))), messages), WantMore);
            assert_eq!(parser.input('1', poss, messages), WantMore);
            assert_eq!(parser.input('2', poss, messages), WantMore);
            assert_eq!(parser.input('3', poss, messages), Failed);

            let expect_messages = &mut MessageEmitter::new();
            expect_messages.push(
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: Position::from((12, 34)), unexpected_char_pos: Position::from((56, 78)), unexpected_char: 'H' });
            assert_eq!(messages, expect_messages);
        }

        {   // \u123g => last error char                                C3, C5, C7
            let mut parser = escape_char_parser_new(4);
            let messages = &mut MessageEmitter::new();
            let poss = (Position::new(), Position::new());
            assert_eq!(parser.input('1', poss, messages), WantMore);
            assert_eq!(parser.input('2', poss, messages), WantMore);
            assert_eq!(parser.input('3', poss, messages), WantMore);
            assert_eq!(parser.input('g', (Position::from((78, 65)), Position::from((43, 21))), messages), Failed);

            let expect_messages = &mut MessageEmitter::new();
            expect_messages.push(
                Message::UnexpectedCharInUnicodeCharEscape{ 
                    escape_start: Position::from((78, 65)), unexpected_char_pos: Position::from((43, 21)), unexpected_char: 'g' });
            assert_eq!(messages, expect_messages);
        }
    }
}