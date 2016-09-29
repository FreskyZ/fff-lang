
// Character literal

use common::Position;
use common::StringPosition;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct CharLiteral {
    pub raw: String,
    pub ch: char,
    pub pos: StringPosition,
    pub has_failed: bool,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct CharLiteral {
    pub raw: String,
    pub ch: char,
    pub pos: StringPosition,
    pub has_failed: bool,
}

impl CharLiteral {

    pub fn from(raw: String, pos: StringPosition) -> CharLiteral {
        CharLiteral {
            raw: raw, 
            ch: ' ',
            pos: pos,
            has_failed: false,
        }
    }
}

use std::fmt;
impl fmt::Debug for CharLiteral {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Char literal {:?} at {:?}{}", 
            self.raw, self.pos, 
            if self.has_failed {
                ", has failed".to_owned()
            } else {
                format!(", with value {:?}", self.ch)
            }) 
    }
}

#[derive(Debug)]
pub struct EscapeCharParser {
    expect_size: usize, // \u expect 4 hex, \U expect 8 hex
    temp: String,
    value: u32,
    has_failed: bool
}

pub enum EscapeCharSimpleCheckResult {
    Normal(char),
    Invalid(char),
    Unicode(EscapeCharParser),
}
pub enum EscapeCharParserInputResult {
    WantMore, 
    FailedAndWantMore,      // Unexpected char but keep until got 4 or 8 char
    FailedAndFinish,        // Unexpected char, enough, continue
    FailedAtLast,           // u32 value is not char
    Success(char),          // Succeed and result
}

use lexical::message::Message;
use lexical::message::MessageEmitter;

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
            has_failed: false 
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
            'u' => EscapeCharSimpleCheckResult::Unicode(EscapeCharParser::new(4)),
            'U' => EscapeCharSimpleCheckResult::Unicode(EscapeCharParser::new(8)),
            other => EscapeCharSimpleCheckResult::Invalid(other),
        }
    }

    // pos for message: (literal_start_pos, escape_start_pos)
    /// ATTENTION: if returned finished but continue input, may cause algorithm overflow panic
    pub fn input(&mut self, ch: char, pos_for_message: (Position, Position), messages: &mut MessageEmitter) -> EscapeCharParserInputResult {
        
        if self.has_failed {
            if self.temp.len() < self.expect_size {
                return EscapeCharParserInputResult::FailedAndWantMore;                      // C1
            } else {
                return EscapeCharParserInputResult::FailedAndFinish;                        // C2
            }
        }

        match ch.to_digit(16) {
            Some(digit) => {
                self.temp.push(ch);
                self.value += digit * FP1_POWERED[self.expect_size - self.temp.len()];
                
                if self.temp.len() == self.expect_size {
                    match char::from_u32(self.value) {
                        Some(ch) => EscapeCharParserInputResult::Success(ch),               // C3
                        None => {
                            // messages.push(Message::)
                            EscapeCharParserInputResult::FailedAtLast,                      // C4
                        }
                    }
                } else {
                    EscapeCharParserInputResult::WantMore                                   // C5
                }
            }
            None => {
                // messages.push(Message::)
                self.has_failed = true;
                if self.temp.len() < self.expect_size {
                    EscapeCharParserInputResult::FailedAndWantMore                          // C6
                } else {
                    EscapeCharParserInputResult::FailedAndFinish                            // C7
                }
            }
        }
    }
}

pub struct CharLiteralParser {

}

#[cfg(test)]
mod tests {

    #[test]
    fn escape_char_input() {

    }
}