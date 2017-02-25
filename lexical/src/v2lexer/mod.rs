
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char

mod numeric_lit_parser;

use std::str::Chars;
use codepos::Position;
use codepos::StringPosition;
use message::MessageCollection;

use super::v1lexer::V1Token;
use super::v1lexer::BufV1Token;
use super::v1lexer::BufV1Lexer;

use super::buf_lexer::IDetailLexer;
use super::buf_lexer::BufToken;
use super::buf_lexer::LegacyBufLexer as BufLexer;

use super::symbol_type::string_literal::StringLiteral;
use super::symbol_type::numeric_literal::NumericLiteral;
use super::symbol_type::char_literal::CharLiteral;

use self::numeric_lit_parser::parse_numeric_literal;

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

pub struct V2Lexer<'chs> {
    v1: BufV1Lexer<'chs>,
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
        *self == '_' || *self == '\'' || self.is_alphabetic() || self.is_digit(36) || *self == '.'
    }

    fn is_seperator(&self) -> bool {
        !self.is_identifier()
    }
}

impl<'chs> IDetailLexer<'chs, V2Token> for V2Lexer<'chs> {

    fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> V2Lexer<'chs> {
        V2Lexer { 
            v1: BufV1Lexer::new(content_chars, messages),
        }
    }

    fn position(&self) -> Position { self.v1.inner().position() }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageCollection) -> Option<V2Token> {
        
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
                Some(BufV1Token{ token: V1Token::StringLiteral(value, pos), next: _1 }) => { 
                    return Some(V2Token::StringLiteral { inner: StringLiteral::new(value, pos, false) });
                }
                Some(BufV1Token{ token: V1Token::RawStringLiteral(value, pos), next: _1 }) => {
                    return Some(V2Token::StringLiteral{ inner: StringLiteral::new(value, pos, true) })
                }
                Some(BufV1Token{ token: V1Token::CharLiteral(value, pos), next: _1 }) => {
                    return Some(V2Token::CharLiteral { inner: CharLiteral{ value: value, pos: pos } });
                }
                None => {
                    return None;
                }
                Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::StringLiteral(_, _)) }) 
                    | Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::RawStringLiteral(_, _)) }) 
                    | Some(BufV1Token{ token: V1Token::Other{ ch, pos }, next: Some(V1Token::CharLiteral(_, _)) }) 
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
                            return Some(V2Token::NumericLiteral { inner: parse_numeric_literal(value, StringPosition::from2(start_pos, vhalf.pos), messages) } );
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

#[allow(dead_code)] // don't know what rustc is thinking
pub type BufV2Token = BufToken<V2Token>;
pub type BufV2Lexer<'chs> = BufLexer<V2Lexer<'chs>, V2Token>;

#[cfg(test)]
mod tests {
    use super::V2Token;
    use super::V2Lexer;
    use super::super::buf_lexer::IDetailLexer;
    use codepos::Position;
    use codepos::StringPosition;
    use message::MessageCollection;
    use super::super::symbol_type::string_literal::StringLiteral;
    use super::super::symbol_type::numeric_literal::NumericLiteral;
    use super::super::NumLitValue;
    
    macro_rules! test_case {
        ($program: expr, $($expect: expr, )*) => ({
            let mut messages = MessageCollection::new();
            let mut v2lexer = V2Lexer::new($program.chars(), &mut messages);
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

    macro_rules! string {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr, $is_raw: expr, $has_fail: expr) => (
            V2Token::StringLiteral{ inner: StringLiteral::new($val.to_owned(), StringPosition::from4($row1, $col1, $row2, $col2), $is_raw) } 
        )
    }
    macro_rules! number {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::NumericLiteral{ 
                inner: NumericLiteral{ 
                    value: Some(NumLitValue::from($val)), 
                    pos: StringPosition::from4($row1, $col1, $row2, $col2)
                }
            }
        );        
        ($row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::NumericLiteral{ 
                inner: NumericLiteral{ 
                    value: None, 
                    pos: StringPosition::from4($row1, $col1, $row2, $col2)
                }
            }
        )
    }
    macro_rules! ident {
        ($name: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            V2Token::Identifier{ name: $name.to_owned(), pos: StringPosition::from4($row1, $col1, $row2, $col2) }
        )
    }
    macro_rules! ch {
        ($ch: expr, $row: expr, $col: expr) => (
            V2Token::Other{ ch: $ch, pos: make_pos!($row, $col) }
        )
    }

    const PROGRAM1: &'static str = "123 456.1";    // Space char as seperator 
    const PROGRAM2: &'static str = "abc/**/def\"\"ght"; // Comment and string literal as seperator 
    const PROGRAM3: &'static str = "123a/ qw1.ad -qw+\nR\"1.23+456\".to_owned()kekekee\n"; // Otherchar as seperator
    #[allow(dead_code)] // temp
    const PROGRAM5: &'static str = "123, abc, hello世界, 你好world_a，123世界";   // Chinese identifier

    #[test]
    fn v2_base() {
        test_case!(PROGRAM1,
            number!(123, 1, 1, 1, 3),
            ch!(' ', 1, 4),
            number!(456.1, 1, 5, 1, 9), 
        );
        test_case!(PROGRAM2,
            ident!("abc", 1, 1, 1, 3),
            ch!(' ', 1, 4),
            ident!("def", 1, 8, 1, 10),
            string!("", 1, 11, 1, 12, false, false),
            ident!("ght", 1, 13, 1, 15),    
        );
        test_case!(PROGRAM3,
            number!(1, 1, 1, 4),
            ch!('/', 1, 5), 
            ch!(' ', 1, 6),
            ident!("qw1", 1, 7, 1, 9),
            ch!('.', 1, 10),
            ident!("ad", 1, 11, 1, 12),
            ch!(' ', 1, 13),
            ch!('-', 1, 14),
            ident!("qw", 1, 15, 1, 16),
            ch!('+', 1, 17),
            ch!('\n', 1, 18),
            string!("1.23+456", 2, 1, 2, 11, true, false),
            ch!('.', 2, 12),
            ident!("to_owned", 2, 13, 2, 20),
            ch!('(', 2, 21),
            ch!(')', 2, 22),
            ident!("kekekee", 2, 23, 2, 29),
            ch!('\n', 2, 30), 
        );
        test_case!("123, abc。hello世界，你好world_a,\n123世界", 
            number!(123, 1, 1, 1, 3),
            ch!(',', 1, 4),
            ch!(' ', 1, 5),
            ident!("abc", 1, 6, 1, 8),
            ch!('。', 1, 9),
            ident!("hello世界", 1, 10, 1, 16),
            ch!('，', 1, 17),
            ident!("你好world_a", 1, 18, 1, 26),
            ch!(',', 1, 27),
            ch!('\n', 1, 28),
            number!(2, 1, 2, 5),
        );
    }

    #[test] 
    fn v2_buf() {
        use super::BufV2Lexer;

        macro_rules! test_case_buf {
            ($program: expr) => ({
                let mut messages = MessageCollection::new();
                let mut bufv2 = BufV2Lexer::new($program.chars(), &mut messages);
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