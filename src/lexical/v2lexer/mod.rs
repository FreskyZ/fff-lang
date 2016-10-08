
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char

mod numeric_lit_parser;

use common::From2;
use common::Position;
use common::StringPosition;
use message::MessageEmitter;

use lexical::v1lexer::V1Token;
use lexical::v1lexer::BufV1Token;
use lexical::v1lexer::BufV1Lexer;

use lexical::buf_lexer::ILexer;
use lexical::buf_lexer::BufToken;
use lexical::buf_lexer::BufLexer;

use lexical::StringLiteral;
use lexical::NumericLiteral;
use lexical::CharLiteral;

use lexical::v2lexer::numeric_lit_parser::parse_numeric_literal;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub enum V2Token {
    StringLiteral { inner: StringLiteral },
    CharLiteral { inner: CharLiteral },
    NumericLiteral { inner: NumericLiteral },           // Anything of [0-9][._a-zA-Z0-9]*
    Identifier { name: String, pos: StringPosition },   // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Other { ch: char, pos: Position },
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum V2Token {
    StringLiteral { inner: StringLiteral },
    CharLiteral { inner: CharLiteral },
    NumericLiteral { inner: NumericLiteral },
    Identifier { name: String, pos: StringPosition },
    Other { ch: char, pos: Position },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V2Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V2Token::StringLiteral { ref inner } => {
                write!(f, "{:?}", inner)
            }
            V2Token::CharLiteral { ref inner } => {
                write!(f, "{:?}", inner)
            }
            V2Token::NumericLiteral { ref inner } => {
                write!(f, "{:?}", inner)
            }
            V2Token::Identifier{ ref name, ref pos } => {
                write!(f, "Identifier {:?} at {:?}", name, pos)
            }
            V2Token::Other{ ch, pos } => {
                write!(f, "Other {:?} at {:?}", ch, pos)
            }
        }
    }
}

pub struct V2Lexer {
    v1: BufV1Lexer,
}
impl From<String> for V2Lexer {

    fn from(content: String) -> V2Lexer {
        V2Lexer { 
            v1: BufV1Lexer::from(content),
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

    // Only digit, '.' start is not supported
    fn is_numeric_literal_start(&self) -> bool {
        self.is_digit(10)
    }
    // Only digit or ASCII letters or underscore
    fn is_numeric_literal(&self) -> bool {
        *self == '_' || self.is_digit(36) || *self == '.'
    }

    fn is_seperator(&self) -> bool {
        !self.is_identifier()
    }
}

impl V2Lexer {    
    pub fn position(&self) -> Position { self.v1.inner().position() }
}

impl ILexer<V2Token> for V2Lexer {

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V2Token> {
        
        struct VHalf{ ch: char, pos: Position, next_is_sep: bool, next_is_dot: bool }

        enum State {
            Nothing,
            InIdentifier { value: String, start_pos: Position },
            InNumericLiteral { value: String, start_pos: Position },
        }

        let mut state = State::Nothing;
        loop {
            // Pass string literal and None and process with other char
            let vhalf = match self.v1.next(messages) {
                Some(BufV1Token{ token: V1Token::StringLiteral { inner }, next: _1 }) => { 
                    return Some(V2Token::StringLiteral { inner: inner });
                }
                Some(BufV1Token{ token: V1Token::CharLiteral{ inner }, next: _1 }) => {
                    return Some(V2Token::CharLiteral { inner: inner });
                }
                None => {
                    return None;
                }
                Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::StringLiteral{ .. }) }) 
                    | Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::CharLiteral{ .. }) }) 
                    | Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: None }) => { 
                    VHalf{ ch: ch, pos: pos, next_is_sep: true, next_is_dot: false } 
                }
                Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::Other{ ch: next_ch, pos: _1 }) }) => {
                    VHalf{ ch: ch, pos: pos, next_is_sep: next_ch.is_seperator(), next_is_dot: next_ch == '.' }
                }
            };

            match state {
                State::Nothing => {
                    match (vhalf.ch.is_identifier_start(), vhalf.ch.is_numeric_literal_start(), vhalf.pos, vhalf.next_is_sep, vhalf.next_is_dot) {
                        (false, false, pos, _next_is_sep, _next_is_dot) => {  // Nothing 
                            return Some(V2Token::Other { ch: vhalf.ch, pos: pos });
                        }
                        (true, false, pos, next_is_sep, _next_is_dot) => { // Identifier try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {  // Direct return identifier is next preview is a sperator
                                return Some(V2Token::Identifier { name: value, pos: StringPosition::from2(pos, pos) });
                            } else {          // Else normal goto InIdentifier state
                                state = State::InIdentifier { value: value, start_pos: pos };
                            }
                        }
                        (false, true, pos, next_is_sep, _next_is_dot) => { // Numeric try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {    // Direct return numeric literal is next preview is a sperator
                                return Some(V2Token::NumericLiteral { inner: parse_numeric_literal(value, StringPosition::from2(pos, pos), messages) });
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
                        if vhalf.next_is_sep { // To be finished, return here
                            return Some(V2Token::Identifier { name: value, pos: StringPosition::from2(start_pos, vhalf.pos) });
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
                        if vhalf.next_is_sep && !vhalf.next_is_dot { // To be finished, return here
                            return Some(V2Token::NumericLiteral { inner: parse_numeric_literal(value, StringPosition::from((start_pos, vhalf.pos)), messages) } );
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

pub type BufV2Token = BufToken<V2Token>;
pub type BufV2Lexer = BufLexer<V2Lexer, V2Token>;

#[cfg(test)]
mod tests {
    use super::V2Token;
    use super::V2Lexer;
    use lexical::buf_lexer::ILexer;
    use common::Position;
    use common::StringPosition;
    use message::MessageEmitter;
    use lexical::symbol_type::StringLiteral;
    use lexical::symbol_type::NumericLiteral;
    use lexical::NumericLiteralValue;
    
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
            V2Token::StringLiteral{ inner: StringLiteral::new($val.to_owned(), StringPosition::from(($row1, $col1, $row2, $col2)), $is_raw) } 
        )
    }
    macro_rules! tnumber {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::NumericLiteral{ 
                inner: NumericLiteral{ 
                    value: Some(NumericLiteralValue::from($val)), 
                    pos: StringPosition::from(($row1, $col1, $row2, $col2))
                }
            }
        );        
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::NumericLiteral{ 
                inner: NumericLiteral{ 
                    value: None, 
                    pos: StringPosition::from(($row1, $col1, $row2, $col2))
                }
            }
        )
    }
    macro_rules! tident {
        ($name: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::Identifier{ name: $name.to_owned(), pos: StringPosition::from(($row1, $col1, $row2, $col2)) }
        )
    }
    macro_rules! tchar {
        ($ch: expr, $row: expr, $col: expr) => (
            V2Token::Other{ ch: $ch, pos: Position{ row: $row, col: $col } }
        )
    }

    const PROGRAM1: &'static str = "123 456.1";    // Space char as seperator 
    const PROGRAM2: &'static str = "abc/**/def\"\"ght"; // Comment and string literal as seperator 
    const PROGRAM3: &'static str = "123a/ qw1.ad -qw+\nR\"1.23+456\".to_owned()kekekee\n"; // Otherchar as seperator
    // const PROGRAM5: &'static str = r"123, abc，你好world_a，123世界";   // Chinese identifier

    #[test]
    fn v2_base() {
        test_case!(PROGRAM1,
            tnumber!(123, 1, 1, 1, 3),
            tchar!(' ', 1, 4),
            tnumber!(456.1, 1, 5, 1, 9), 
        );
        test_case!(PROGRAM2,
            tident!("abc", 1, 1, 1, 3),
            tchar!(' ', 1, 4),
            tident!("def", 1, 8, 1, 10),
            tstring!("", 1, 11, 1, 12, false, false),
            tident!("ght", 1, 13, 1, 15),    
        );
        test_case!(PROGRAM3,
            tnumber!(1, 1, 1, 4),
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
            tstring!("1.23+456", 2, 1, 2, 11, true, false),
            tchar!('.', 2, 12),
            tident!("to_owned", 2, 13, 2, 20),
            tchar!('(', 2, 21),
            tchar!(')', 2, 22),
            tident!("kekekee", 2, 23, 2, 29),
            tchar!('\n', 2, 30), 
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
                let mut bufv2 = BufV2Lexer::from($program.to_owned());
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