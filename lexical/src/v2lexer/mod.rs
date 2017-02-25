
// Level2 parser
// input v1
// output string or numeric literal, identifier or other char

mod numeric_lit_parser;

use std::str::Chars;
use codepos::Position;
use codepos::StringPosition;
use message::MessageCollection;

use super::v1lexer::V1Token;
use super::v1lexer::V1Lexer;

use super::buf_lexer::ILexer;
use super::buf_lexer::BufLexer;

use super::LitValue;
use self::numeric_lit_parser::parse_numeric_literal;

#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub enum V2Token {
    Literal(LitValue),
    Identifier(String),   // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Other(char),
    EOF,
}
#[cfg(not(test))]
#[derive(Clone)]
pub enum V2Token {
    Literal(LitValue),
    Identifier(String),   // Anything of [_a-zA-Z][_a-zA-Z0-9]*
    Other(char),
    EOF,
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V2Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V2Token::Literal(ref value) => write!(f, "{:?}", value),
            V2Token::Identifier(ref value) => write!(f, "Identifier {:?}", value),
            V2Token::Other(ref value) => write!(f, "Other {:?}", value),
            V2Token::EOF => write!(f, "EOF"),
        }
    }
}

pub struct V2Lexer<'chs> {
    v1: BufLexer<V1Lexer<'chs>, V1Token>,
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

impl<'chs> ILexer<'chs, V2Token> for V2Lexer<'chs> {

    fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> V2Lexer<'chs> {
        V2Lexer { 
            v1: BufLexer::new(content_chars, messages),
        }
    }

    // input stringliteral or otherchar without comment, output identifier and numeric literal
    fn next(&mut self, messages: &mut MessageCollection) -> (V2Token, StringPosition) {
        
        struct VHalf{ ch: char, pos: Position, next_is_sep: bool, next_is_dot: bool }

        enum State {
            Nothing,
            InIdentifier { value: String, start_pos: Position },
            InNumericLiteral { value: String, start_pos: Position },
        }

        let mut state = State::Nothing;
        loop {
            self.v1.move_next(messages);
            let vhalf = match self.v1.current_with_preview() {
                (&V1Token::StringLiteral(ref value), pos, _2, _3) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::RawStringLiteral(ref value), pos, _2, _3) => {
                    return (V2Token::Literal(LitValue::Str(value.clone())), pos);
                }
                (&V1Token::CharLiteral(ref value), pos, _2, _3) => {
                    return (V2Token::Literal(LitValue::Char(value.clone())), pos);
                }
                (&V1Token::EOF, eof_pos, _2, _3) => {
                    return (V2Token::EOF, eof_pos);
                }
                (&V1Token::Other(ch), pos, &V1Token::Other(next_ch), _3) => {
                    VHalf{ ch: ch, pos: pos.start_pos(), next_is_sep: next_ch.is_seperator(), next_is_dot: next_ch == '.' }
                }
                (&V1Token::Other(ch), pos, _2, _3) => { 
                    VHalf{ ch: ch, pos: pos.start_pos(), next_is_sep: true, next_is_dot: false } 
                }
            };

            match state {
                State::Nothing => {
                    match (vhalf.ch.is_identifier_start(), vhalf.ch.is_numeric_literal_start(), vhalf.pos, vhalf.next_is_sep, vhalf.next_is_dot) {
                        (false, false, pos, _next_is_sep, _next_is_dot) => {  // Nothing 
                            return (V2Token::Other(vhalf.ch), StringPosition::double(pos));
                        }
                        (true, false, pos, next_is_sep, _next_is_dot) => { // Identifier try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {  // Direct return identifier is next preview is a sperator
                                return (V2Token::Identifier(value), StringPosition::from2(pos, pos));
                            } else {          // Else normal goto InIdentifier state
                                state = State::InIdentifier { value: value, start_pos: pos };
                            }
                        }
                        (false, true, pos, next_is_sep, _next_is_dot) => { // Numeric try start
                            let mut value = String::new();
                            value.push(vhalf.ch);
                            if next_is_sep {    // Direct return numeric literal is next preview is a sperator
                                let (num_lit_val, pos) = parse_numeric_literal(value, StringPosition::double(pos), messages);
                                return (V2Token::Literal(LitValue::Num(num_lit_val)), pos);
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
                            return (V2Token::Identifier(value), StringPosition::from2(start_pos, vhalf.pos));
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
                            let (num_lit_val, pos) = parse_numeric_literal(value, StringPosition::from2(start_pos, vhalf.pos), messages);
                            return (V2Token::Literal(LitValue::Num(num_lit_val)), pos);
                        } else {
                            state = State::InNumericLiteral{ value: value, start_pos: start_pos };
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
#[test]
fn v2_base() {    

    macro_rules! test_case {
        ($program: expr, $($expect: expr, )*) => ({
            let mut messages = MessageCollection::new();
            let mut v2lexer = V2Lexer::new($program.chars(), &mut messages);
            let mut v2s = Vec::new();
            loop {
                match v2lexer.next(&mut messages) {
                    (V2Token::EOF, _) => break,
                    v2 => v2s.push(v2),
                }
            }

            assert_eq!(v2s, vec![$($expect, )*]);
            if !messages.is_empty() {
                perrorln!("Messages for {}:", stringify!($program));
                perror!("{:?}", messages);
            }
        })
    }

    macro_rules! lit {
        ($val: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Literal(LitValue::from($val)), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! ident {
        ($name: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Identifier($name.to_owned()), StringPosition::from4($row1, $col1, $row2, $col2))
        )
    }
    macro_rules! ch {
        ($ch: expr, $row: expr, $col: expr) => (
            (V2Token::Other($ch), make_str_pos!($row, $col, $row, $col))
        );
        ($ch: expr, $row1: expr, $col1: expr, $row2: expr, $col2: expr) => (
            (V2Token::Other($ch), make_str_pos!($row1, $col1, $row2, $col2))
        )
    }

    const PROGRAM1: &'static str = "123 456.1";    // Space char as seperator 
    const PROGRAM2: &'static str = "abc/**/def\"\"ght"; // Comment and string literal as seperator 
    const PROGRAM3: &'static str = "123a/ qw1.ad -qw+\nR\"1.23+456\".to_owned()kekekee\n"; // Otherchar as seperator
    #[allow(dead_code)] // temp
    const PROGRAM5: &'static str = "123, abc, hello世界, 你好world_a，123世界";   // Chinese identifier

    test_case!(PROGRAM1,
        lit!(123, 1, 1, 1, 3),
        ch!(' ', 1, 4),
        lit!(456.1, 1, 5, 1, 9), 
    );
    test_case!(PROGRAM2,
        ident!("abc", 1, 1, 1, 3),
        ch!(' ', 1, 4),
        ident!("def", 1, 8, 1, 10),
        lit!("", 1, 11, 1, 12),
        ident!("ght", 1, 13, 1, 15),    
    );
    test_case!(PROGRAM3,
        lit!(123, 1, 1, 1, 4),
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
        lit!("1.23+456", 2, 1, 2, 11),
        ch!('.', 2, 12),
        ident!("to_owned", 2, 13, 2, 20),
        ch!('(', 2, 21),
        ch!(')', 2, 22),
        ident!("kekekee", 2, 23, 2, 29),
        ch!('\n', 2, 30), 
    );
    test_case!("123, abc。hello世界，你好world_a,\n123世界", 
        lit!(123, 1, 1, 1, 3),
        ch!(',', 1, 4),
        ch!(' ', 1, 5),
        ident!("abc", 1, 6, 1, 8),
        ch!('。', 1, 9),
        ident!("hello世界", 1, 10, 1, 16),
        ch!('，', 1, 17),
        ident!("你好world_a", 1, 18, 1, 26),
        ch!(',', 1, 27),
        ch!('\n', 1, 28),
        lit!(123, 2, 1, 2, 5),
    );
}

#[test] 
fn v2_buf() {
    // use super::BufV2Lexer;

    // macro_rules! test_case_buf {
    //     ($program: expr) => ({
    //         let mut messages = MessageCollection::new();
    //         let mut bufv2 = BufV2Lexer::new($program.chars(), &mut messages);
    //         loop {
    //             match bufv2.next(&mut messages) {
    //                 Some(v2) => perrorln!("{:?}", v2),
    //                 None => break,
    //             }
    //         }

    //         if !messages.is_empty() {
    //             perrorln!("Messages for {}:", stringify!($program));
    //             perror!("{:?}", messages);
    //         }
    //     })
    // }

    // test_case_buf!(PROGRAM1);
    // test_case_buf!(PROGRAM2);
    // test_case_buf!(PROGRAM3);
}