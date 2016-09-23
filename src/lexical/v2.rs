
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char
// block comments become simple space here

use position::Position;
#[derive(Debug, Eq, PartialEq)]
pub enum V2Token {
    StringLiteral { value: String, start_pos: Position, end_pos: Position },
    NumericLiteral { raw: String, start_pos: Position, end_pos: Position },
    Identifier { raw: String, start_pos: Position, end_pos: Position },  // Any thing of [_a-zA-Z][_a-zA-Z0-9]*
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

use lexical::v0::V0Token;
use lexical::v1::V1Token;
use lexical::message::Message;
use lexical::message::MessageEmitter;
impl V2Lexer { 
    
    pub fn position(&self) -> Position { self.v1.position() }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<V2Token> {

        // means v0.5, leave other char include block comment as space here, other just pass through
        macro_rules! get_vhalf {
            ($this: expr, $messages: expr) => (
                match $this.v1.next(messages) {
                    Some(V1Token::SkippedBlockComment { start_pos, next_ch, next_pos }) => 
                        V0Token { ch: ' ', pos: start_pos, next_ch: next_ch, next_pos: next_pos }, // Pretend to be a simple '\u{20}'
                    Some(V1Token::OtherChar { raw, pos, next_ch, next_pos }) => 
                        V0Token { ch: raw, pos: pos, next_ch: next_ch, next_pos: next_pos }, 
                    Some(V1Token::StringLiteral { value, start_pos, end_pos }) => 
                        return Some(V2Token::StringLiteral { value: value, start_pos: start_pos, end_pos: end_pos }),
                    None => return None,
                }
            )
        }

        // not identifier is all seperator
        macro_rules! is_identifier_start_char {
            ($ch: expr) => ($ch == '_' || ($ch >= 'a' && $ch <= 'z') || ($ch >= 'A' && $ch <= 'Z'))
        }
        macro_rules! is_identifier_char {
            ($ch: expr) => ($ch == '_' || ($ch >= 'a' && $ch <= 'z') || ($ch >= 'A' && $ch <= 'Z') || ($ch >= '0' && $ch <= '9'))
        }
        macro_rules! is_numeric_start_char {
            ($ch: expr) => ($ch >= '0' && $ch <= '9')
        }
        macro_rules! is_numeric_char {
            ($ch: expr) => ($ch >= '0' && $ch <= '9')
        }

        enum State {
            Nothing,
            InIdentifier { value: String, start_pos: Position, end_pos: Position },
            InNumericLiteral { value: String, start_pos: Position, end_pos: Position }, // TODO: has_failed: bool
        }

        // TODO: using preview char to fix the bug: seperator exactly after identifier is missing
        let mut state = State::Nothing;
        loop {
            let vhalf = get_vhalf!(self, messages);
            match state {
                State::Nothing => {
                    if !is_identifier_start_char!(vhalf.ch) && !is_numeric_start_char!(vhalf.ch) {
                        // Nothing happened
                        return Some(V2Token::OtherChar { raw: vhalf.ch, pos: vhalf.pos });
                    } else if is_identifier_start_char!(vhalf.ch) {
                        let mut value = String::new();
                        value.push(vhalf.ch);
                        state = State::InIdentifier { value: value, start_pos: vhalf.pos, end_pos: vhalf.pos };
                    } else if is_numeric_start_char!(vhalf.ch) {
                        let mut value = String::new();
                        value.push(vhalf.ch);
                        state = State::InNumericLiteral { value: value, start_pos: vhalf.pos, end_pos: vhalf.pos };
                    } else {
                        unreachable!()
                    }
                }
                State::InIdentifier { mut value, start_pos, end_pos } => {
                    if is_identifier_char!(vhalf.ch) {
                        value.push(vhalf.ch);
                        state = State::InIdentifier { value: value, start_pos: start_pos, end_pos: vhalf.pos };
                    } else {
                        return Some(V2Token::Identifier { raw: value, start_pos: start_pos, end_pos: end_pos });
                    }
                }
                State::InNumericLiteral { mut value, start_pos, end_pos } => {
                    if is_numeric_char!(vhalf.ch) {
                        value.push(vhalf.ch);
                        state = State::InNumericLiteral { value: value, start_pos: start_pos, end_pos: vhalf.pos };
                    } else if is_identifier_char!(vhalf.ch) {
                        messages.push(Message::UnexpectedIdentifierCharInNumericLiteral { 
                            literal_start: start_pos, unexpected_pos: vhalf.pos, unexpected_char: vhalf.ch });
                        value.push(vhalf.ch);  
                        state = State::InNumericLiteral { value: value, start_pos: start_pos, end_pos: vhalf.pos };
                    } else {
                        return Some(V2Token::NumericLiteral { raw: value, start_pos: start_pos, end_pos: end_pos });
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
        
        perrorln!("messages: {:?}", messages);
    }
}