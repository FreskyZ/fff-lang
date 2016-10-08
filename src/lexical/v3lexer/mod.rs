
// Level3 parser, mainly a dispatcher
// input v2
// Hide spaces
// Output 
//      escaped string literal, \n\r\t\"\\, include unicode escape
//      evaluated numeric literal, rust style, _ is ignored, with type postfix, only i32
//      identifier or keyword
//      operators, +, -, *, /, %, +=, -=, *=, /=, %=, .
//      seperators, [, ], {, }, (, ), ;, ,
// May be final layer, --- not, 17/10/8

use common::From2;
use common::TryFrom;
use common::Position;
use common::StringPosition;
use message::MessageEmitter;

use lexical::v2lexer::V2Token;
use lexical::v2lexer::BufV2Token;
use lexical::v2lexer::BufV2Lexer;

use lexical::buf_lexer::ILexer;

use lexical::symbol_type::StringLiteral;
use lexical::symbol_type::NumericLiteral;
use lexical::symbol_type::CharLiteral;
use lexical::symbol_type::KeywordKind;
use lexical::symbol_type::SeperatorKind;

test_only_attr!{
    test: [derive(Clone, Eq, PartialEq)]
    not_test: [derive(Clone)]
    pub enum V3Token {
        StringLiteral(StringLiteral),
        NumericLiteral(NumericLiteral),
        CharLiteral(CharLiteral),
        Identifier(String, StringPosition),
        Keyword(KeywordKind, StringPosition),
        BooleanLiteral(bool, StringPosition),
        Seperator(SeperatorKind, StringPosition),
    }
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V3Token::StringLiteral(ref literal) => write!(f, "{:?}", literal),
            V3Token::NumericLiteral(ref literal) => write!(f, "{:?}", literal),
            V3Token::CharLiteral(ref literal) => write!(f, "{:?}", literal),
            V3Token::BooleanLiteral(ref literal, ref pos) => write!(f, "Boolean literal {} at {:?}", literal, pos),
            V3Token::Identifier(ref identifier, ref pos) => write!(f, "Identifier `{}` at {:?}", identifier, pos),
            V3Token::Keyword(ref keyword, ref pos) => write!(f, "Keyword {:?} at {:?}", keyword, pos),
            V3Token::Seperator(ref seperator, ref pos) => write!(f, "Seperator {:?} at {:?}", seperator, pos),
        }
    }
}

pub struct V3Lexer {
    v2: BufV2Lexer,
}

impl From<String> for V3Lexer {
    fn from(content: String) -> V3Lexer {
        V3Lexer { v2: BufV2Lexer::from(content) }
    }
}

impl V3Lexer {
    pub fn position(&self) -> Position { self.v2.inner().position() }
}

impl ILexer<V3Token> for V3Lexer {

    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {

        loop { // Loop for ignore char
            match self.v2.next(messages) {
                Some(BufV2Token{ token: V2Token::StringLiteral{ inner }, next: _1 }) => {
                    // Dispatch string literal to escape
                    return Some(V3Token::StringLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::NumericLiteral{ inner }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    return Some(V3Token::NumericLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::CharLiteral{ inner }, next: _1 }) => {
                    // Dispatch numeric literal to get value
                    return Some(V3Token::CharLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::Identifier{ name, pos }, next: _1 }) => {
                    // Dispatch identifier to identifier or keyword
                    match KeywordKind::try_from(&name) {
                        Some(keyword) => return Some(V3Token::Keyword(keyword, pos)),
                        None => {
                            match &*name {
                                "true" => return Some(V3Token::BooleanLiteral(true, pos)),
                                "false" => return Some(V3Token::BooleanLiteral(false, pos)),
                                _ => return Some(V3Token::Identifier(name, pos)),
                            }
                        }
                    }
                }
                Some(BufV2Token{ token: V2Token::Other{ ch, pos }, next: Some(V2Token::Other{ ch: next_ch, pos: next_pos }) }) => {
                    // Dispatch otherchar to seperator
                    match SeperatorKind::try_from((ch ,next_ch)) {
                        Some(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator(sep, StringPosition::from2(pos, pos))),
                            2 => {
                                self.v2.skip1(messages);
                                return Some(V3Token::Seperator(sep, StringPosition::from2(pos, next_pos)));
                            }
                            _ => unreachable!(),
                        },
                        None => continue,
                    }
                }
                Some(BufV2Token{ token: V2Token::Other{ ch, pos }, next: _other }) => {
                    // Dispatch otherchar, seperator to seperator or operators
                    match SeperatorKind::try_from(ch) {
                        Some(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator(sep, StringPosition::from2(pos, pos))),
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

#[cfg(test)]
mod tests {
    use lexical::buf_lexer::ILexer;

    // TODO: ~Extend: Unicode seperator error recover

    #[test]
    fn v3_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use message::MessageEmitter;

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

    #[test]
    fn v3_on_lexical_v2_num_lit_parser() {
         use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use message::MessageEmitter;

        let file_name = "src/lexical/v2lexer/numeric_lit_parser.rs";
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