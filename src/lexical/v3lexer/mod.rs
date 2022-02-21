
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

use std::str::Chars;
use crate::common::From2;
use crate::common::Position;
use crate::common::StringPosition;
use crate::message::LexicalMessage;
use crate::message::MessageEmitter;

use crate::lexical::v2lexer::V2Token;
use crate::lexical::v2lexer::BufV2Token;
use crate::lexical::v2lexer::BufV2Lexer;

use crate::lexical::buf_lexer::IDetailLexer;

use crate::lexical::symbol_type::string_literal::StringLiteral;
use crate::lexical::symbol_type::numeric_literal::NumericLiteral;
use crate::lexical::symbol_type::char_literal::CharLiteral;
use crate::lexical::KeywordKind;
use crate::lexical::SeperatorKind;

mod unicode_char;

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

pub struct V3Lexer<'chs> {
    v2: BufV2Lexer<'chs>,
}

impl<'chs> From<Chars<'chs>> for V3Lexer<'chs> {
    fn from(content_chars: Chars) -> V3Lexer {
        V3Lexer { v2: BufV2Lexer::from(content_chars) }
    }
}

// Process every may be seperator char, if it is special unicode char, emit message and return the ascii version
fn pass_unicode_char(ch: char, pos: Position) -> (char, Option<LexicalMessage>) {
    use self::unicode_char::check_unicode_char;

    match check_unicode_char(ch) {
        Some((unicode_ch, unicode_name, ascii_ch, ascii_name)) => {
            (ascii_ch, Some(LexicalMessage::UnexpectedUnicodeChar{ 
                ch: unicode_ch, pos: pos, unicode_name: unicode_name.to_owned(), ascii_ch: ascii_ch, ascii_name: ascii_name.to_owned()
            }))
        }
        None => (ch, None),
    }
}

impl<'chs> IDetailLexer<'chs, V3Token> for V3Lexer<'chs> {

    fn position(&self) -> Position { self.v2.inner().position() }

    fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {

        loop { // Loop for ignore char
            match self.v2.next(messages) {
                Some(BufV2Token{ token: V2Token::StringLiteral{ inner }, next: _1 }) => {
                    return Some(V3Token::StringLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::NumericLiteral{ inner }, next: _1 }) => {
                    return Some(V3Token::NumericLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::CharLiteral{ inner }, next: _1 }) => {
                    return Some(V3Token::CharLiteral(inner));
                }
                Some(BufV2Token{ token: V2Token::Identifier{ name, pos }, next: _1 }) => {
                    match KeywordKind::try_from(&name) {
                        Ok(keyword) => return Some(V3Token::Keyword(keyword, pos)),
                        Err(_) => {
                            match &*name {
                                "true" => return Some(V3Token::BooleanLiteral(true, pos)),
                                "false" => return Some(V3Token::BooleanLiteral(false, pos)),
                                _ => return Some(V3Token::Identifier(name, pos)),
                            }
                        }
                    }
                }
                Some(BufV2Token{ token: V2Token::Other{ ch, pos }, next: Some(V2Token::Other{ ch: next_ch, pos: next_pos }) }) => {
                    let (ch, msg) = pass_unicode_char(ch, pos);
                    let (next_ch, next_msg) = pass_unicode_char(next_ch, next_pos);
                    match msg { Some(msg) => messages.push(msg), None => () }
                    
                    match SeperatorKind::try_from((ch ,next_ch)) {
                        Ok(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator(sep, StringPosition::from2(pos, pos))),
                            2 => {
                                self.v2.skip1(messages);
                                // Lazy push next_msg because if is not some of 2 word seperator, it will be pushed again in next loop
                                match next_msg { Some(next_msg) => messages.push(next_msg), None => () }
                                return Some(V3Token::Seperator(sep, StringPosition::from2(pos, next_pos)));
                            }
                            _ => unreachable!(),
                        },
                        Err(_) => continue,
                    }
                }
                Some(BufV2Token{ token: V2Token::Other{ ch, pos }, next: _other }) => {
                    let (ch, msg) = pass_unicode_char(ch, pos);
                    match msg { Some(msg) => messages.push(msg), None => () }

                    match SeperatorKind::try_from(ch) {
                        Ok(sep) => match sep.len() { 
                            1 => return Some(V3Token::Seperator(sep, StringPosition::from2(pos, pos))),
                            _ => unreachable!(),
                        },
                        Err(_) => continue,
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
    use crate::lexical::buf_lexer::IDetailLexer;

    #[test]
    fn v3_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use crate::message::MessageEmitter;

        let file_name = "tests/lexical/2.sm";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v3lexer = V3Lexer::from(content.chars());

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
    #[ignore]
    fn v3_on_lexical_v2_num_lit_parser() {
         use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use crate::message::MessageEmitter;

        let file_name = "src/lexical/v2lexer/numeric_lit_parser.rs";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v3lexer = V3Lexer::from(content.chars());

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