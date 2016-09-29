
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char

use position::Position;
use position::StringPosition;
use lexical::string_literal::StringLiteral;
#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub enum V2Token {
    StringLiteral { inner: StringLiteral },
    NumericLiteral { raw: String, pos: StringPosition },
    Identifier { name: String, pos: StringPosition },  // Any thing of [_a-zA-Z][_a-zA-Z0-9]*
    OtherChar { ch: char, pos: Position }, // space, parenthenes, comma, etc.
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum V2Token {
    StringLiteral { value: String, pos: StringPosition, is_raw: bool },
    NumericLiteral { raw: String, pos: StringPosition },
    Identifier { name: String, pos: StringPosition },
    OtherChar { ch: char, pos: Position },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V2Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::V2Token::*;
        match *self {
            StringLiteral { ref inner } => {
                write!(f, "{:?}", inner)
            }
            NumericLiteral { ref raw, ref pos } => {
                write!(f, "NumericLiteral {:?} at {:?}", raw, pos)
            }
            Identifier{ ref name, ref pos } => {
                write!(f, "Identifier {:?} at {:?}", name, pos)
            }
            OtherChar{ ch, pos } => {
                write!(f, "Char {:?} at {:?}", ch, pos)
            }
        }
    }
}

use lexical::v1::V1Lexer;
use lexical::v1::BufV1Lexer;
pub struct V2Lexer {
    v1: BufV1Lexer,
}
impl From<String> for V2Lexer {

    fn from(content: String) -> V2Lexer {
        V2Lexer { 
            v1: BufV1Lexer::from(V1Lexer::from(content)),
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

use lexical::ILexer;
use lexical::v1::V1Token;
use lexical::v1::BufV1Token;
use lexical::message::MessageEmitter;
impl V2Lexer {    
    pub fn position(&self) -> Position { self.v1.inner().position() }
}

impl ILexer<V2Token> for V2Lexer {

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V2Token> {
        
        struct VHalf{ ch: char, pos: Position, next_is_sep: bool }

        enum State {
            Nothing,
            InIdentifier { value: String, start_pos: Position },
            InNumericLiteral { value: String, start_pos: Position },
        }

        // TODO: using preview char to fix the bug: seperator exactly after identifier is missing
        let mut state = State::Nothing;
        loop {
            // Pass string literal and None and process with other char
            let vhalf = match self.v1.next(messages) {
                Some(BufV1Token{ token: V1Token::StringLiteral { inner }, next: _1 }) => { 
                    return Some(V2Token::StringLiteral { inner: inner });
                }
                None => {
                    return None;
                }
                Some(BufV1Token{ token: V1Token::OtherChar{ raw, pos }, next: Some(V1Token::StringLiteral{ .. }) }) 
                    | Some(BufV1Token{ token: V1Token::OtherChar{ raw, pos }, next: None }) => { 
                    VHalf{ ch: raw, pos: pos, next_is_sep: true } 
                }
                Some(BufV1Token{ token: V1Token::OtherChar{ raw, pos }, next: Some(V1Token::OtherChar{ raw: next_ch, pos: _1 }) }) => {
                    VHalf{ ch: raw, pos: pos, next_is_sep: next_ch.is_seperator() }
                }
            };

            match state {
                State::Nothing => {
                    match (vhalf.ch.is_identifier_start(), vhalf.ch.is_numeric_literal_start(), vhalf.pos, vhalf.next_is_sep) {
                        (false, false, pos, _next_is_sep) => {
                            // Nothing happened
                            return Some(V2Token::OtherChar { ch: vhalf.ch, pos: pos });
                        }
                        (true, false, pos, next_is_sep) => {
                            // Identifier try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {  // Direct return identifier is next preview is a sperator
                                return Some(V2Token::Identifier { name: value, pos: StringPosition { start_pos: pos, end_pos: pos } });
                            } else {          // Else normal goto InIdentifier state
                                state = State::InIdentifier { value: value, start_pos: pos };
                            }
                        }
                        (false, true, pos, next_is_sep) => {
                            // Numeric try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {    // Direct return numeric literal is next preview is a sperator
                                return Some(V2Token::NumericLiteral { raw: value, pos: StringPosition { start_pos: pos, end_pos: pos } });
                            } else {            // else normal goto InIdentifier state
                                state = State::InNumericLiteral { value: value, start_pos: pos };
                            }
                        }
                        _ => unreachable!()
                    }
                }
                State::InIdentifier { mut value, start_pos } => {
                    if vhalf.ch.is_identifier() {
                        value.push(vhalf.ch);
                        if vhalf.next_is_sep {
                            // To be finished, return here
                            return Some(V2Token::Identifier { name: value, pos: StringPosition{ start_pos: start_pos, end_pos: vhalf.pos } });
                        } else {
                            state = State::InIdentifier { value: value, start_pos: start_pos };
                        }
                    } else {
                        unreachable!()
                    }
                }
                State::InNumericLiteral { mut value, start_pos } => {
                    if vhalf.ch.is_numeric_literal() {
                        value.push(vhalf.ch);                        
                        if vhalf.next_is_sep {
                            // To be finished, return here
                            return Some(V2Token::NumericLiteral { raw: value, pos: StringPosition{ start_pos: start_pos, end_pos: vhalf.pos } });
                        } else {
                            state = State::InNumericLiteral { value: value, start_pos: start_pos };
                        }
                    } else {
                        unreachable!()
                    }
                }
            }
        }
    }
}

use lexical::buf_lexer::BufToken;
use lexical::buf_lexer::BufLexer;
pub type BufV2Token = BufToken<V2Token>;
pub type BufV2Lexer = BufLexer<V2Lexer, V2Token>;

#[cfg(test)]
mod tests {

    use super::V2Token;
    use super::V2Lexer;
    use lexical::ILexer;
    use position::Position;
    use position::StringPosition;
    use lexical::string_literal::StringLiteral;
    use lexical::message::MessageEmitter;
    
    macro_rules! test_case {
        ($program: expr, $($expect: expr, )*) => ({
            let mut v2lexer = V2Lexer::from($program.to_owned());
            let mut messages = MessageEmitter::new();
            let mut v2s = Vec::new();
            loop {
                match v2lexer.next(&mut messages) {
                    Some(v2) => v2s.push(v2),
                    None => break,
                }
            }

            assert_eq!(v2s, vec![$($expect, )*]);
            if !messages.is_empty() {
                perrorln!("Messages for {}:", stringify!($program));
                perror!("{:?}", messages);
            }
        })
    }

    macro_rules! tstring {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr, $has_fail: expr) => (
            V2Token::StringLiteral{ inner: StringLiteral {
                value: $val.to_owned(), 
                pos: StringPosition { start_pos: Position { row: $row1, col: $col1 }, end_pos: Position { row: $row2, col: $col2 } }, 
                is_raw: $is_raw,
                has_failed: $has_fail } }
        )
    }
    macro_rules! tnumber {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::NumericLiteral{ raw: $val.to_owned(), pos: StringPosition { start_pos: Position { row: $row1, col: $col1 }, end_pos: Position { row: $row2, col: $col2 } } }
        )
    }
    macro_rules! tident {
        ($name: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::Identifier{ name: $name.to_owned(), pos: StringPosition { start_pos: Position { row: $row1, col: $col1 }, end_pos: Position { row: $row2, col: $col2 } } }
        )
    }
    macro_rules! tchar {
        ($ch: expr, $row: expr, $col: expr) => (
            V2Token::OtherChar{ ch: $ch, pos: Position{ row: $row, col: $col } }
        )
    }

    const PROGRAM1: &'static str = "123 456";    // Space char as seperator 
    const PROGRAM2: &'static str = "abc/**/def\"\"ght"; // Comment and string literal as seperator 
    const PROGRAM3: &'static str = "123a/ qw1.ad -qw+\nR\"123+456\".to_owned()kekekee\n"; // Otherchar as seperator
    // const PROGRAM5: &'static str = r"123, abc，你好world_a，123世界";   // Chinese identifier
    // SOLVE IN FUTURE

    #[test]
    fn v2_anyother_is_seperator() {
        test_case!(PROGRAM1,
            tnumber!("123", 1, 1, 1, 3),
            tchar!(' ', 1, 4),
            tnumber!("456", 1, 5, 1, 7), 
        );
        test_case!(PROGRAM2,
            tident!("abc", 1, 1, 1, 3),
            tchar!(' ', 1, 4),
            tident!("def", 1, 8, 1, 10),
            tstring!("", 1, 11, 1, 12, false, false),
            tident!("ght", 1, 13, 1, 15),    
        );
        test_case!(PROGRAM3,
            tnumber!("123a", 1, 1, 1, 4),
            tchar!('/', 1, 5), 
            tchar!(' ', 1, 6),
            tident!("qw1", 1, 7, 1, 9),
            tchar!('.', 1, 10),
            tident!("ad", 1, 11, 1, 12),
            tchar!(' ', 1, 13),
            tchar!('-', 1, 14),
            tident!("qw", 1, 15, 1, 16),
            tchar!('+', 1, 17),
            tchar!('\n', 1, 18),
            tstring!("123+456", 2, 1, 2, 10, true, false),
            tchar!('.', 2, 11),
            tident!("to_owned", 2, 12, 2, 19),
            tchar!('(', 2, 20),
            tchar!(')', 2, 21),
            tident!("kekekee", 2, 22, 2, 28),
            tchar!('\n', 2, 29), 
        );
        // test_case!(PROGRAM5, 
        //     tnumber!("123", 1, 1, 1, 3),
        //     tchar!(',', 1, 4),
        //     tchar!(' ', 1, 5),
        //     tident!("abc", 1, 6, 1, 8),
        //     tchar!('，', 1, 9),
        //     tident!(r"你好world", 1, 10, 1, 16),
        //     tchar!(',', 1, 17),
        //     tchar!('\n', 1, 18),
        //     tident!("_a", 2, 1, 2, 2),
        //     tchar!('，', 2, 3),
        //     tnumber!(r"123世界", 2, 4, 2, 8),
        // );
    }

    #[test] 
    fn v2_buf() {
        use super::BufV2Lexer;

        macro_rules! test_case_buf {
            ($program: expr) => ({
                let mut bufv2 = BufV2Lexer::from(V2Lexer::from($program.to_owned()));
                let mut messages = MessageEmitter::new();
                loop {
                    match bufv2.next(&mut messages) {
                        Some(v2) => perrorln!("{:?}", v2),
                        None => break,
                    }
                }

                if !messages.is_empty() {
                    perrorln!("Messages for {}:", stringify!($program));
                    perror!("{:?}", messages);
                }
            })
        }

        test_case_buf!(PROGRAM1);
        test_case_buf!(PROGRAM2);
        test_case_buf!(PROGRAM3);
    }
}