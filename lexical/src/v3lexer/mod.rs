
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
use util::TryFrom;
use codepos::Position;
use codepos::StringPosition;
use message::LexicalMessage;
use message::MessageCollection;

use super::v2lexer::V2Token;
use super::v2lexer::V2Lexer;

use super::buf_lexer::ILexer;
use super::buf_lexer::BufLexer;

use super::LitValue;
use super::KeywordKind;
use super::SeperatorKind;

mod unicode_char;

test_only_attr!{
    test: [derive(Clone, Eq, PartialEq)]
    not_test: [derive(Clone)]
    pub enum V3Token {
        Literal(LitValue),
        Identifier(String),
        Keyword(KeywordKind),
        Seperator(SeperatorKind),
        EOF,
    }
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V3Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            V3Token::Literal(ref literal) => write!(f, "{:?}", literal),
            V3Token::Identifier(ref identifier) => write!(f, "Identifier `{}`", identifier),
            V3Token::Keyword(ref keyword) => write!(f, "Keyword {:?}", keyword),
            V3Token::Seperator(ref seperator) => write!(f, "Seperator {:?}", seperator),
            V3Token::EOF => write!(f, "EOF"),
        }
    }
}

pub struct V3Lexer<'chs> {
    v2: BufLexer<V2Lexer<'chs>, V2Token>,
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

impl<'chs> ILexer<'chs, V3Token> for V3Lexer<'chs> {

    fn new(content_chars: Chars<'chs>, messages: &mut MessageCollection) -> V3Lexer<'chs> {
        V3Lexer { v2: BufLexer::new(content_chars, messages) }
    }

    fn next(&mut self, messages: &mut MessageCollection) -> (V3Token, StringPosition) {

        loop { // Loop for ignore char
            self.v2.move_next(messages);
            match self.v2.current_with_preview2() {
                (&V2Token::Literal(ref lit_value), lit_pos, _2, _3, _4, _5) => {
                    return (V3Token::Literal(lit_value.clone()), lit_pos);
                }
                (&V2Token::Identifier(ref name), ident_pos, _2, _3, _4, _5) => {
                    return (V3Token::Identifier(name.clone()), ident_pos);
                }
                (&V2Token::Keyword(ref kind), keyword_pos, _2, _3, _4, _5) => {
                    return (V3Token::Keyword(kind.clone()), keyword_pos);
                }
                (&V2Token::Other(ref ch), pos, &V2Token::Other(ref next_ch), next_pos, _4, _5) => {
                    let (ch, msg) = pass_unicode_char(*ch, pos.start_pos());
                    let (next_ch, next_msg) = pass_unicode_char(*next_ch, next_pos.start_pos());
                    match msg { Some(msg) => messages.push(msg), None => () }
                    
                    match SeperatorKind::try_from((ch ,next_ch)) {
                        Some(sep) => match sep.len() { 
                            1 => return (V3Token::Seperator(sep), pos),
                            2 => {
                                self.v2.prepare_skip1();
                                // Lazy push next_msg because if is not some of 2 word seperator, it will be pushed again in next loop
                                match next_msg { Some(next_msg) => messages.push(next_msg), None => () }
                                return (V3Token::Seperator(sep), StringPosition::merge(pos, next_pos));
                            }
                            _ => unreachable!(),
                        },
                        None => (),
                    }
                }
                (&V2Token::Other(ch), pos, _2, _3, _4, _5) => {
                    let (ch, msg) = pass_unicode_char(ch, pos.start_pos());
                    match msg { Some(msg) => messages.push(msg), None => () }

                    match SeperatorKind::try_from(ch) {
                        Some(sep) => match sep.len() { 
                            1 => return (V3Token::Seperator(sep), pos),
                            _ => unreachable!(),
                        },
                        None => (),
                    }
                }
                (&V2Token::EOF, eof_pos, _2, _3, _4, _5) => return (V3Token::EOF, eof_pos),
            }
        }
    }
}

#[cfg(test)]
#[test]
fn v3_test1() {
    use std::fs::File;
    use std::io::Read;

    let file_name = "../tests/lexical/2.sm";
    let mut file: File = File::open(file_name).expect("Open file failed");

    let mut content = String::new();
    let _read_size = file.read_to_string(&mut content).expect("Read file failed");
    let mut messages = MessageCollection::new();
    let mut v3lexer = V3Lexer::new(content.chars(), &mut messages);

    loop {
        match v3lexer.next(&mut messages) {
            (V3Token::EOF, _) => break,
            v3 => perrorln!("{:?}", v3),
        }
    }
    
    perrorln!("messages: \n{:?}", messages);
}
