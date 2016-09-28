
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
use lexical::types::Keyword;
use lexical::types::Operator;
use lexical::types::Seperator;

#[derive(Clone)]
pub enum V3Token {
    StringLiteral { value: String, pos: StringPosition, has_failed: bool },
    NumericLiteral { raw: String, value: i32, pos: StringPosition, has_failed: bool },
    Identifier { name: String, pos: StringPosition },
    Keyword { kind: Keyword, pos: StringPosition },
    Operator { kind: Operator, pos: StringPosition },
    Seperator { kind: Seperator, pos: StringPosition, }
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::V3Token::*;
        match *self {
            StringLiteral{ ref value, ref pos, ref has_failed } => {
                write!(f, "String literal {:?} at {:?}{}", value, pos, if *has_failed { ", has failed" } else { "" })
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
            Operator { ref kind, ref pos } => {
                write!(f, "Operator {:?} at pos {:?}", kind, pos)
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

// escape \u{ABCD}
fn escape_string_literal(raw: String, is_raw: bool, has_failed: bool, messages: &mut MessageEmitter) -> String {
    // Currently not implemented

    let _is_raw = is_raw;
    let _has_failed = has_failed;
    let _messages = messages;
    raw
}
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
pub fn pub_escape_string(raw: String, is_raw: bool, has_failed: bool, messages: &mut MessageEmitter) -> String {
    escape_string_literal(raw, is_raw, has_failed, messages)
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
impl ILexer<V3Token> for V3Lexer {

    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {

        loop { // Loop for ignore char
            match self.v2.next(messages) {
                Some(BufV2Token{ token: V2Token::StringLiteral{ value, pos, is_raw, has_failed }, next: _1 }) => {
                    // Dispatch string literal to escape
                    return Some(V3Token::StringLiteral{ value: escape_string_literal(value, is_raw, has_failed, messages), pos: pos, has_failed: has_failed });
                }
                Some(BufV2Token{ token: V2Token::NumericLiteral{ raw, pos }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    let result = numeric_literal_to_value(&raw, pos, messages);
                    return Some(V3Token::NumericLiteral{ raw: raw, value: result.0, pos: pos, has_failed: result.1 });
                }
                Some(BufV2Token{ token: V2Token::Identifier{ name, pos }, next: _1 }) => {
                    // Dispatch identifier to identifier or keyword
                    match Keyword::from(&name) {
                        Some(keyword) => return Some(V3Token::Keyword{ kind: keyword, pos: pos }),
                        None => return Some(V3Token::Identifier { name: name, pos: pos })
                    }
                }
                Some(BufV2Token{ token: V2Token::OtherChar{ ch, pos }, next: Some(V2Token::OtherChar{ ch: next_ch, pos: next_pos }) }) => {
                    // Dispatch otherchar, otherchar to seperator or operators
                    match Seperator::from(ch)
                        .map(|sep|{
                            V3Token::Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) }        // Got seperator
                        })
                        .or_else(||{
                            Operator::from2(ch, next_ch)
                                .map(|op|{
                                    self.v2.skip1(messages);
                                    V3Token::Operator{ kind: op, pos: StringPosition::from((pos, next_pos)) }      // Get operator, length 2, so skip1
                                })
                                .or_else(||{
                                    Operator::from1(ch)
                                        .map(|op|{
                                            V3Token::Operator{ kind: op, pos: StringPosition::from((pos, pos)) }  // Get operator, length 1
                                        })
                                })
                        }) {
                        Some(v3) => match v3 {
                            V3Token::Seperator { kind: Seperator::WhiteSpace, .. } => continue,
                            v3 @ _ => return Some(v3),
                        },
                        None => continue,
                    }
                }
                Some(BufV2Token{ token: V2Token::OtherChar{ ch, pos }, next: _other }) => {
                    // Dispatch otherchar, seperator to seperator or operators
                    match Seperator::from(ch)
                        .map(|sep|{
                            V3Token::Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) }        // Got seperator
                        })
                        .or_else(||{
                            Operator::from1(ch)
                                .map(|op| {
                                    V3Token::Operator{ kind: op, pos: StringPosition::from((pos, pos)) }
                                })
                        }) {
                        Some(v3) => match v3 {
                            V3Token::Seperator { kind: Seperator::WhiteSpace, .. } => continue,
                            v3 @ _ => return Some(v3),
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
impl fmt::Debug for BufV3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            BufV3Token { ref token, next: Some(ref next_token) } => {
                write!(f, "{:?}, next: {:?}", token, next_token)
            }
            BufV3Token { ref token, next: None } => {
                write!(f, "{:?}, next: None", token)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use lexical::ILexer;

    #[test]
    fn v3_string_literal() {
        let _ = 1;
    }   

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