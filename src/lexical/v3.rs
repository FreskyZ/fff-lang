
// Level3 parser, mainly a dispatcher
// input v2
// Hide spaces
// Output 
//      escaped string literal, \n\r\t\"\\, include unicode escape
//      evaluated numeric literal, rust style, _ is ignored, with type postfix, only i32
//      identifier or keyword
//      operators, +, -, *, /, %, +=, -=, *=, /=, %=, .
//      seperators, [, ], {, }, (, ), ;, ,
// May be final layer

use position::Position;
use position::StringPosition;
use lexical::string_literal::StringLiteral;
use lexical::keyword_kind::KeywordKind;
use lexical::seperator_kind::SeperatorKind;

#[derive(Clone)]
pub enum V3Token {
    StringLiteral { inner: StringLiteral },
    NumericLiteral { raw: String, value: i32, pos: StringPosition, has_failed: bool },
    Identifier { name: String, pos: StringPosition },
    Keyword { kind: KeywordKind, pos: StringPosition },
    Seperator { kind: SeperatorKind, pos: StringPosition },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::V3Token::*;
        match *self {
            StringLiteral{ ref inner } => {
                write!(f, "{:?}", inner)
            }
            NumericLiteral{ ref raw, ref value, ref pos, ref has_failed } => {
                write!(f, "Numeric literal {:?} at {:?} with value {}{}", raw, pos, value, if *has_failed { ", has failed" } else { "" })
            }
            Identifier { ref name, ref pos } => {
                write!(f, "Identifier {:?} at {:?}", name, pos)
            }
            Keyword { ref kind, ref pos } => {
                write!(f, "Keyword {:?} at pos {:?}", kind, pos)
            }
            Seperator { ref kind, ref pos } => {
                write!(f, "Seperator {:?} at pos {:?}", kind, pos)
            }
        }
    }
}

use lexical::v2::V2Lexer;
use lexical::v2::BufV2Lexer;
pub struct V3Lexer {
    v2: BufV2Lexer,
}

impl From<String> for V3Lexer {
    fn from(content: String) -> V3Lexer {
        V3Lexer { v2: BufV2Lexer::from(V2Lexer::from(content)) }
    }
}

use lexical::message::Message;
use lexical::message::MessageEmitter;

// ignore _ and ignore i32 postfix
fn numeric_literal_to_value(raw: &str, pos: StringPosition, messages: &mut MessageEmitter) -> (i32, bool) { // value, has_failed
    
    let no_postfix = if raw.len() > 3 && &raw[(raw.len() - 3)..] == "i32" {
        &raw[..(raw.len() - 3)]
    } else {
        raw
    };

    let mut digits = Vec::new();
    for ch in no_postfix.chars() {
        if ch == '_' {
            continue;
        } else if ch.is_digit(10) {
            digits.push(ch);
        } else {
            messages.push(Message::UnexpectedIdentifierCharInNumericLiteral { 
                literal_start: pos.start_pos,
                unexpected_char: ch
            });
            return (0, true);
        }
    }

    // digits will not be empty because first character must be [0-9]
    if digits.len() > 10 {
        messages.push(Message::NumericLiteralTooLong { literal_start: pos.start_pos });
        return (0, true);
    }
    
    const TENS: [i32; 10] = [
        1, 
        10, 
        100, 
        1000, 
        10000,
        100000, 
        1000000,
        10000000, 
        100000000, 
        1000000000
    ];

    let mut value = 0_i32;
    let length = digits.len();
    for i in 0..length {
        value = match (digits[i].to_digit(10).unwrap() as i32).checked_mul(TENS[length - i - 1]) {
            None => {
                messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                return (0, true);
            }
            Some(middle) => {
                match value.checked_add(middle) {
                    Some(value) => value, 
                    None => {
                        messages.push(Message::NumericLiteralTooLarge { literal_start: pos.start_pos });
                        return (0, true);
                    }
                }
            }
        };
    }

    (value, false)
}

#[cfg(test)]
pub fn pub_numeric_literal(raw: &str, messages: &mut MessageEmitter) -> (i32, bool) {
    numeric_literal_to_value(raw, StringPosition::new(), messages)
}

impl V3Lexer {
    pub fn position(&self) -> Position { self.v2.inner().position() }
}

use lexical::ILexer;
use lexical::v2::V2Token;
use lexical::v2::BufV2Token;
use common::TryFrom;
impl ILexer<V3Token> for V3Lexer {

    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {

        loop { // Loop for ignore char
            match self.v2.next(messages) {
                Some(BufV2Token{ token: V2Token::StringLiteral{ inner }, next: _1 }) => {
                    // Dispatch string literal to escape
                    return Some(V3Token::StringLiteral{ inner: inner });
                }
                Some(BufV2Token{ token: V2Token::NumericLiteral{ raw, pos }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    let result = numeric_literal_to_value(&raw, pos, messages);
                    return Some(V3Token::NumericLiteral{ raw: raw, value: result.0, pos: pos, has_failed: result.1 });
                }
                Some(BufV2Token{ token: V2Token::Identifier{ name, pos }, next: _1 }) => {
                    // Dispatch identifier to identifier or keyword
                    match KeywordKind::try_from(&name) {
                        Some(keyword) => return Some(V3Token::Keyword{ kind: keyword, pos: pos }),
                        None => return Some(V3Token::Identifier { name: name, pos: pos })
                    }
                }
                Some(BufV2Token{ token: V2Token::OtherChar{ ch, pos }, next: Some(V2Token::OtherChar{ ch: next_ch, pos: next_pos }) }) => {
                    // Dispatch otherchar to seperator
                    match SeperatorKind::try_from((ch ,next_ch)) {
                        Some(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) }),
                            2 => {
                                self.v2.skip1(messages);
                                return Some(V3Token::Seperator{ kind: sep, pos: StringPosition::from((pos, next_pos)) });
                            }
                            _ => unreachable!(),
                        },
                        None => continue,
                    }
                }
                Some(BufV2Token{ token: V2Token::OtherChar{ ch, pos }, next: _other }) => {
                    // Dispatch otherchar, seperator to seperator or operators
                    match SeperatorKind::try_from(ch) {
                        Some(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) }),
                            _ => unreachable!(),
                        },
                        None => continue,
                    }
                }
                None => {
                    return None;
                }
            }
        }
    }
}

use lexical::buf_lexer::BufToken;
use lexical::buf_lexer::BufLexer;
pub type BufV3Token = BufToken<V3Token>;
pub type BufV3Lexer = BufLexer<V3Lexer, V3Token>;

#[cfg(test)]
mod tests {
    use lexical::ILexer;

    #[test]
    fn v3_numeric_literal() {
        use super::pub_numeric_literal;
        use lexical::message::MessageEmitter;

        let messages = &mut MessageEmitter::new();
        perrorln!("{:?}", pub_numeric_literal("123", messages));
        perrorln!("{:?}", pub_numeric_literal("123_i32", messages));
        perrorln!("{:?}", pub_numeric_literal("1_2_3i32", messages));
        perrorln!("{:?}", pub_numeric_literal("123_456_789_012", messages));
        perrorln!("{:?}", pub_numeric_literal("999_999_999_9_i32", messages));

        perrorln!("Messages: {:?}", messages);
    }

    #[test]
    fn v3_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use lexical::message::MessageEmitter;

        let file_name = "tests/lexical/2.sm";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v3lexer = V3Lexer::from(content);

        let mut messages = MessageEmitter::new();
        loop {
            match v3lexer.next(&mut messages) {
                Some(v3) => perrorln!("{:?}", v3),
                None => break, 
            }
        }
        
        perrorln!("messages: \n{:?}", messages);
    }
}