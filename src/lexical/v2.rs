
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char
// block comments become simple space here

use position::Position;
#[derive(Debug, Eq, PartialEq)]
pub enum V2Token {
    StringLiteral { value: String, start_pos: Position, end_pos: Position },
    NumericLiteral { raw: String, start_pos: Position, end_pos: Position },
    Identifier { name: String, start_pos: Position, end_pos: Position },  // Any thing of [_a-zA-Z][_a-zA-Z0-9]*
    OtherChar { raw: char, pos: Position }, // space, parenthenes, comma, etc.
}

use lexical::v1::V1Lexer;
pub struct V2Lexer {
    v1: V1Lexer,
}
impl From<String> for V2Lexer {

    fn from(content: String) -> V2Lexer {
        V2Lexer { 
            v1: V1Lexer::from(content),
        }
    }
}

trait IdentifierChar {

    fn is_identifier_start(&self) -> bool;
    fn is_identifier(&self) -> bool;

    fn is_numeric_literal_start(&self) -> bool;
    fn is_numeric_literal(&self) -> bool;

    fn is_seperator(&self) -> bool;
}
impl IdentifierChar for char {

    // Include chinese alphabetical char
    fn is_identifier_start(&self) -> bool {
        *self == '_' || self.is_alphabetic()
    }
    // Include digit
    fn is_identifier(&self) -> bool {
        *self == '_' || self.is_alphabetic() || self.is_digit(10)  
    }

    // Only digit
    fn is_numeric_literal_start(&self) -> bool {
        self.is_digit(10)  
    }
    // Only digit or ASCII letters or underscore
    fn is_numeric_literal(&self) -> bool {
        *self == '_' || self.is_digit(36)
    }

    fn is_seperator(&self) -> bool {
        !self.is_identifier()
    }
}

use lexical::v0::V0Token;
use lexical::v1::V1Token;
use lexical::message::MessageEmitter;
impl V2Lexer { 
    
    pub fn position(&self) -> Position { self.v1.position() }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<V2Token> {

        enum State {
            Nothing,
            InIdentifier { value: String, start_pos: Position, end_pos: Position },
            InNumericLiteral { value: String, start_pos: Position, end_pos: Position },
        }

        // TODO: using preview char to fix the bug: seperator exactly after identifier is missing
        let mut state = State::Nothing;
        loop {
            // Pass string literal and None, make block comment to space and process with other char
            let vhalf = match self.v1.next(messages) {
                Some(V1Token::SkippedBlockComment { start_pos, next_ch, next_pos }) => 
                    V0Token { ch: ' ', pos: start_pos, next_ch: next_ch, next_pos: next_pos }, // Pretend to be a simple '\u{20}'
                Some(V1Token::OtherChar { raw, pos, next_ch, next_pos }) => 
                    V0Token { ch: raw, pos: pos, next_ch: next_ch, next_pos: next_pos }, 
                Some(V1Token::StringLiteral { value, start_pos, end_pos }) => 
                    return Some(V2Token::StringLiteral { value: value, start_pos: start_pos, end_pos: end_pos }),
                None => return None,
            };

            match state {
                State::Nothing => {
                    match (vhalf.ch.is_identifier_start(), vhalf.ch.is_numeric_literal_start(), vhalf.pos, vhalf.next_ch) {
                        (false, false, pos, _next_ch) => {
                            // Nothing happened
                            return Some(V2Token::OtherChar { raw: vhalf.ch, pos: pos });
                        }
                        (true, false, pos, next_ch) => {
                            // Identifier try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            match next_ch {
                                Some(ch) if ch.is_seperator() => { // Direct return identifier is next preview is a sperator
                                    return Some(V2Token::Identifier { name: value, start_pos: pos, end_pos: pos });
                                }
                                Some(_) => { // else normal goto InIdentifier state
                                    state = State::InIdentifier { value: value, start_pos: pos, end_pos: pos };
                                }
                                None => { // If next preview is none, just return here
                                    return Some(V2Token::Identifier { name: value, start_pos: pos, end_pos: pos });
                                }
                            }
                        }
                        (false, true, pos, next_ch) => {
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            match next_ch {
                                Some(ch) if ch.is_seperator() => { // Direct return numeric literal is next preview is a sperator
                                    return Some(V2Token::NumericLiteral { raw: value, start_pos: pos, end_pos: pos });
                                }
                                Some(_) => { // else normal goto InIdentifier state
                                    state = State::InNumericLiteral { value: value, start_pos: pos, end_pos: pos };
                                }
                                None => { // If next preview is none, just return here
                                    return Some(V2Token::NumericLiteral { raw: value, start_pos: pos, end_pos: pos });
                                }
                            }
                        }
                        _ => unreachable!()
                    }
                }
                State::InIdentifier { mut value, start_pos, end_pos: _1 } => {
                    if vhalf.ch.is_identifier() {
                        value.push(vhalf.ch);
                        if vhalf.next_ch.is_none() || vhalf.next_ch.unwrap().is_seperator() {
                            // To be finished, return here
                            return Some(V2Token::Identifier { name: value, start_pos: start_pos, end_pos: vhalf.pos });
                        } else {
                            state = State::InIdentifier { value: value, start_pos: start_pos, end_pos: vhalf.pos };
                        }
                    } else {
                        unreachable!()
                    }
                }
                State::InNumericLiteral { mut value, start_pos, end_pos: _1 } => {
                    if vhalf.ch.is_numeric_literal() {
                        value.push(vhalf.ch);                        
                        if vhalf.next_ch.is_none() || vhalf.next_ch.unwrap().is_seperator() {
                            // To be finished, return here
                            return Some(V2Token::NumericLiteral { raw: value, start_pos: start_pos, end_pos: vhalf.pos });
                        } else {
                            state = State::InNumericLiteral { value: value, start_pos: start_pos, end_pos: vhalf.pos };
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
        }
    }
}


#[cfg(test)]
mod tests {

    #[test]
    fn v2_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V2Lexer;
        use lexical::message::MessageEmitter;

        let file_name = "tests/lexical/2.sm";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v2lexer = V2Lexer::from(content);

        let mut messages = MessageEmitter::new();
        loop {
            match v2lexer.next(&mut messages) {
                Some(v2) => perrorln!("{:?}", v2),
                None => break, 
            }
        }
        
        perrorln!("messages: \n{:?}", messages);
    }
}