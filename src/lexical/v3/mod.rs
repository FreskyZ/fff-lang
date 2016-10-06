
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

use common::TryFrom;
use common::Position;
use common::StringPosition;
use message::MessageEmitter;
use lexical::v2::V2Token;
use lexical::v2::BufV2Token;
use lexical::v2::BufV2Lexer;
use lexical::buf_lexer::ILexer;
use lexical::StringLiteral;
use lexical::NumericLiteral;
use lexical::CharLiteral;
use lexical::KeywordKind;
use lexical::SeperatorKind;
use lexical::Seperator;
use lexical::Keyword;
use lexical::Identifier;
use lexical::BooleanLiteral;

test_only_attr!{
    test: [derive(Clone, Eq, PartialEq)]
    not_test: [derive(Clone)]
    pub enum V3Token {
        StringLiteral(StringLiteral),
        NumericLiteral(NumericLiteral),
        CharLiteral(CharLiteral),
        Identifier(Identifier),
        Keyword(Keyword),
        BooleanLiteral(BooleanLiteral),
        Seperator(Seperator),
    }
}

impl V3Token {

    pub fn position(&self) -> StringPosition {
        match *self {
            V3Token::StringLiteral(ref literal) => literal.pos,
            V3Token::NumericLiteral(ref literal) => literal.pos,
            V3Token::CharLiteral(ref literal) => literal.pos,
            V3Token::Identifier(ref ident) => ident.pos,
            V3Token::Keyword(ref keyword) => keyword.pos,
            V3Token::BooleanLiteral(ref literal) => literal.pos,
            V3Token::Seperator(ref seperator) => seperator.pos,
        }
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
            V3Token::BooleanLiteral(ref literal) => write!(f, "{:?}", literal),
            V3Token::Identifier(ref identifier) => write!(f, "{:?}", identifier),
            V3Token::Keyword(ref keyword) => write!(f, "{:?}", keyword),
            V3Token::Seperator(ref seperator) => write!(f, "{:?}", seperator),
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
                        Some(keyword) => return Some(V3Token::Keyword( Keyword{ kind: keyword, pos: pos })),
                        None => {
                            match &*name {
                                "true" => return Some(V3Token::BooleanLiteral( BooleanLiteral{ value: true, pos: pos })),
                                "false" => return Some(V3Token::BooleanLiteral( BooleanLiteral{ value: false, pos: pos })),
                                _ => return Some(V3Token::Identifier( Identifier{ name: name, pos: pos })),
                            }
                        }
                    }
                }
                Some(BufV2Token{ token: V2Token::Other{ ch, pos }, next: Some(V2Token::Other{ ch: next_ch, pos: next_pos }) }) => {
                    // Dispatch otherchar to seperator
                    match SeperatorKind::try_from((ch ,next_ch)) {
                        Some(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator( Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) })),
                            2 => {
                                self.v2.skip1(messages);
                                return Some(V3Token::Seperator( Seperator{ kind: sep, pos: StringPosition::from((pos, next_pos)) }));
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
                            1 => return Some(V3Token::Seperator( Seperator{ kind: sep, pos: StringPosition::from((pos, pos)) })),
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
    fn v3_on_lexical_symbol_type_numeic_literal() {
         use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use message::MessageEmitter;

        let file_name = "src/lexical/symbol_type/numeric_literal.rs";
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