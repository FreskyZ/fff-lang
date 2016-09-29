
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

use common::Position;
use common::StringPosition;
use lexical::symbol_type::string_literal::StringLiteral;
use lexical::symbol_type::numeric_literal::NumericLiteral;
use lexical::symbol_type::char_literal::CharLiteral;
use lexical::symbol_type::keyword_kind::KeywordKind;
use lexical::symbol_type::seperator_kind::SeperatorKind;

#[derive(Clone)]
pub enum V3Token {
    StringLiteral { inner: StringLiteral },
    NumericLiteral { inner: NumericLiteral },
    CharLiteral { inner: CharLiteral },
    Identifier { name: String, pos: StringPosition },
    Keyword { kind: KeywordKind, pos: StringPosition },
    BooleanLiteral { value: bool },
    Seperator { kind: SeperatorKind, pos: StringPosition },
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V3Token::StringLiteral{ ref inner } => {
                write!(f, "{:?}", inner)
            }
            V3Token::NumericLiteral{ ref inner } => {
                write!(f, "{:?}", inner)
            }
            V3Token::CharLiteral { ref inner } => {
                write!(f, "{:?}", inner)
            }
            V3Token::BooleanLiteral{ ref value } => {
                write!(f, "Boolean literal: {:?}", value)
            }
            V3Token::Identifier { ref name, ref pos } => {
                write!(f, "Identifier {:?} at {:?}", name, pos)
            }
            V3Token::Keyword { ref kind, ref pos } => {
                write!(f, "Keyword {:?} at pos {:?}", kind, pos)
            }
            V3Token::Seperator { ref kind, ref pos } => {
                write!(f, "Seperator {:?} at pos {:?}", kind, pos)
            }
        }
    }
}

use lexical::lexer::v2::V2Lexer;
use lexical::lexer::v2::BufV2Lexer;
pub struct V3Lexer {
    v2: BufV2Lexer,
}

impl From<String> for V3Lexer {
    fn from(content: String) -> V3Lexer {
        V3Lexer { v2: BufV2Lexer::from(V2Lexer::from(content)) }
    }
}


impl V3Lexer {
    pub fn position(&self) -> Position { self.v2.inner().position() }
}

use common::TryFrom;
use lexical::ILexer;
use lexical::lexer::v2::V2Token;
use lexical::lexer::v2::BufV2Token;
use lexical::message::MessageEmitter;
impl ILexer<V3Token> for V3Lexer {

    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {

        loop { // Loop for ignore char
            match self.v2.next(messages) {
                Some(BufV2Token{ token: V2Token::StringLiteral{ inner }, next: _1 }) => {
                    // Dispatch string literal to escape
                    return Some(V3Token::StringLiteral{ inner: inner });
                }
                Some(BufV2Token{ token: V2Token::NumericLiteral{ inner }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    return Some(V3Token::NumericLiteral{ inner: inner });
                }
                Some(BufV2Token{ token: V2Token::CharLiteral{ inner }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    return Some(V3Token::CharLiteral{ inner: inner });
                }
                Some(BufV2Token{ token: V2Token::Identifier{ name, pos }, next: _1 }) => {
                    // Dispatch identifier to identifier or keyword
                    // TODO!TODO! `true`, `false` become bool literal here
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

use lexical::lexer::buf_lexer::BufToken;
use lexical::lexer::buf_lexer::BufLexer;
pub type BufV3Token = BufToken<V3Token>;
pub type BufV3Lexer = BufLexer<V3Lexer, V3Token>;

#[cfg(test)]
mod tests {
    use lexical::ILexer;

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