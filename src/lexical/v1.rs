
// Level1 parser
// input v0
// remove comment
// Pass through pos
// output string literal, numeric literal, identifier and otherchar
// string literal is allowed to cross line, line end is regarded as \n

use position::Position;
#[derive(Debug, Eq, PartialEq)]
pub enum V1Token {
    SkippedBlockComment { start_pos: Position }, // For not concat identifier before and after, become space in v2
    StringLiteral { value: String, start_pos: Position, end_pos: Position },
    NumericLiteral { raw: String, start_pos: Position, end_pos: Position },
    Identifier { raw: String, start_pos: Position, end_pos: Position },  // Any thing of [_a-zA-Z][_a-zA-Z0-9]*
    OtherChar { raw: char, pos: Position }, // space, parenthenes, comma, etc.
}

use lexical::v0::V0Lexer;
use lexical::v0::V0Token;
pub struct V1Lexer {
    v0: V0Lexer,
}
impl From<String> for V1Lexer {

    fn from(content: String) -> V1Lexer {
        V1Lexer { 
            v0: V0Lexer::from(content),
        }
    }
}

mod interest {
    pub const QUOTE: char = '"';  
    pub const SLASH: char = '/';           // \n 
    pub const BACKSLASH: char = '\\';      // \\\n\r\t\"\\
    pub const STAR: char = '*';            /* */
    pub const LINEEND: char = '\n';

    pub const ESCAPE_TAB: char = 't';
    pub const ESCAPE_LINEFEED: char = 'n';
    pub const ESCAPE_CARRIAGE_RETURN: char = 'r';
    pub const ESCAPE_QUOTE: char = '"';
    pub const ESCAPE_BACKSLASH: char = '\\';
}

enum InternalV1Token {
    SkippedBlockComment { start_pos: Position, next_ch: Option<char>, next_pos: Position },
    StringLiteral { value: String, start_pos: Position, end_pos: Position, next_ch: Option<char>, next_pos: Position },
    OtherChar { raw: char, pos: Position, next_ch:  Option<char>, next_pos: Position },
}

use lexical::message::Message;
use lexical::message::MessageEmitter;
impl V1Lexer {

    pub fn position(&self) -> Position { self.v0.position() }

    // input v0, output stringliteral or otherchar without comment
    fn next_except_comment(&mut self, messages: &mut MessageEmitter) -> Option<InternalV1Token> {
        // First there is quote, and anything inside is regarded as string literal, include `\n` as real `\n`
        // and then outside of quote pair there is comments, anything inside comment, // and /n, or /* and */ is regarded as comment

        #[derive(Debug)]
        enum State {
            Nothing,
            InStringLiteral { raw: String, start_pos: Position, end_pos: Position, last_escape_quote_pos: Option<Position> },
            InLineComment,
            InBlockComment { start_pos: Position },
        }

        let mut state = State::Nothing;
        loop {
            let v0token = self.v0.next_char();
            match state {
                State::Nothing => {
                    match v0token {
                        Some(V0Token { ch: '/', pos: _1, next_ch: Some('/'), next_pos: _2 }) => {
                            state = State::InLineComment;
                        }
                        Some(V0Token { ch: '/', pos, next_ch: Some('*'), next_pos: _1 }) => {
                            state = State::InBlockComment { start_pos: pos };
                        }
                        Some(V0Token { ch: '"', pos, next_ch: _1, next_pos }) => {
                            state = State::InStringLiteral { raw: String::new(), start_pos: pos, end_pos: next_pos, last_escape_quote_pos: None };
                        }
                        Some(V0Token { ch, pos, next_ch, next_pos }) => {
                            return Some(InternalV1Token::OtherChar { raw: ch, pos: pos, next_ch: next_ch, next_pos: next_pos });
                        }
                        None => { return None; }
                    }
                }
                State::InBlockComment { start_pos } => {
                    match v0token {
                        Some(V0Token { ch: '*', pos: _1, next_ch: Some('/'), next_pos: _2 }) => {
                            let V0Token { ch: _3, pos: _4, next_ch, next_pos } = self.v0.next_char().unwrap(); // cannot be none
                            return Some(InternalV1Token::SkippedBlockComment { start_pos: start_pos, next_ch: next_ch, next_pos: next_pos });
                        }
                        Some(V0Token { .. }) => (),
                        None => {
                            messages.push(Message::UnexpectedEndofFileInBlockComment { block_start: start_pos, eof_pos: self.v0.position() });
                            return None;
                        }
                    }
                }
                State::InLineComment => {
                    match v0token {
                        Some(V0Token { ch: '\n', pos, next_ch, next_pos }) => {
                            // state = State::Nothing;
                            return Some(InternalV1Token::OtherChar { raw: '\n', pos: pos, next_ch: next_ch, next_pos: next_pos });
                        }
                        None => {
                            return None;
                        }
                        Some(V0Token { .. }) => (),
                    }
                }
                State::InStringLiteral { mut raw, start_pos, end_pos, last_escape_quote_pos } => {
                    match v0token {
                        Some(V0Token { ch: '\\', pos, next_ch: Some('"'), next_pos: _2 }) => {
                            // record escaped \" here to be error hint
                            raw.push('"');
                            state = State::InStringLiteral { raw: raw, start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: Some(pos) };
                            self.v0.skip1();
                        }
                        Some(V0Token { ch: '\\', pos, next_ch, next_pos: _2 }) => {
                            match next_ch {
                                Some('n') => {
                                    raw.push('\n');
                                    state = State::InStringLiteral { raw: raw, 
                                        start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                    self.v0.skip1();
                                }
                                Some('\\') => {
                                    raw.push('\\');
                                    state = State::InStringLiteral { raw: raw, 
                                        start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                    self.v0.skip1();
                                }
                                Some('t') => {
                                    raw.push('\t');
                                    state = State::InStringLiteral { raw: raw, 
                                        start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                    self.v0.skip1();
                                }
                                Some('r') => {
                                    raw.push('\r');
                                    state = State::InStringLiteral { raw: raw, 
                                        start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                    self.v0.skip1();
                                }
                                Some(other) => {
                                    messages.push(Message::UnrecogonizedEscapeCharInStringLiteral {
                                        literal_start: start_pos, unrecogonize_pos: pos, unrecogonize_escape: other });
                                    // here actually is meaning less
                                    state = State::InStringLiteral { raw: raw, start_pos: 
                                        start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                }
                                None => {
                                    state = State::InStringLiteral { raw: raw, start_pos: 
                                        start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                                }
                            }
                        }
                        Some(V0Token { ch: '"', pos, next_ch, next_pos }) => {
                            return Some(InternalV1Token::StringLiteral { value: raw, start_pos: start_pos, end_pos: pos, next_ch: next_ch, next_pos: next_pos });
                        }
                        Some(V0Token { ch, pos: _1, next_ch: _2, next_pos: _3 }) => {
                            raw.push(ch);
                            state = State::InStringLiteral { raw: raw, 
                                start_pos: start_pos, end_pos: end_pos, last_escape_quote_pos: last_escape_quote_pos };
                        }
                        None => {
                            messages.push(Message::UnexpectedEndofFileInStringLiteral { 
                                literal_start: start_pos, eof_pos: self.v0.position(), hint_escaped_quote_pos: last_escape_quote_pos });
                            return None;
                        }
                    }
                }
            }
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<V1Token> {

        // means v0.5, leave other char include block comment as space here, other just pass through
        macro_rules! get_vhalf {
            ($this: expr, $messages: expr) => (
                match $this.next_except_comment(messages) {
                    Some(InternalV1Token::SkippedBlockComment { start_pos, next_ch, next_pos }) => 
                        V0Token { ch: ' ', pos: start_pos, next_ch: next_ch, next_pos: next_pos }, // Pretend to be a simple '\u{20}'
                    Some(InternalV1Token::OtherChar { raw, pos, next_ch, next_pos }) => 
                        V0Token { ch: raw, pos: pos, next_ch: next_ch, next_pos: next_pos }, 
                    Some(InternalV1Token::StringLiteral { value, start_pos, end_pos, .. }) => 
                        return Some(V1Token::StringLiteral { value: value, start_pos: start_pos, end_pos: end_pos }),
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

        let mut state = State::Nothing;
        loop {
            let vhalf = get_vhalf!(self, messages);
            match state {
                State::Nothing => {
                    if !is_identifier_start_char!(vhalf.ch) && !is_numeric_start_char!(vhalf.ch) {
                        // Nothing happened
                        return Some(V1Token::OtherChar { raw: vhalf.ch, pos: vhalf.pos });
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
                        return Some(V1Token::Identifier { raw: value, start_pos: start_pos, end_pos: end_pos });
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
                        return Some(V1Token::NumericLiteral { raw: value, start_pos: start_pos, end_pos: end_pos });
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v1_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V1Lexer;
        use lexical::message::MessageEmitter;

        let file_name = "tests/lexical/2.sm";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v1lexer = V1Lexer::from(content);

        let mut messages = MessageEmitter::new();
        loop {
            match v1lexer.next(&mut messages) {
                Some(v1) => perrorln!("{:?}", v1),
                None => break, 
            }
        }
        
        perrorln!("messages: {:?}", messages);
    }
}