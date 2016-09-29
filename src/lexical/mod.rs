
mod message;
mod symbol_type;
mod lexer;

pub use lexical::message::Message;
pub use lexical::message::MessageEmitter;
pub use lexical::symbol_type::keyword_kind::KeywordKind;
pub use lexical::symbol_type::seperator_kind::SeperatorKind;

pub trait ILexer<TToken> {
    fn next(&mut self, emitter: &mut MessageEmitter) -> Option<TToken>;
}

use self::lexer::v3::V3Lexer;
use self::lexer::v3::BufV3Lexer;
pub struct Lexer {
    v3: BufV3Lexer,
}

pub type Token = self::lexer::v3::V3Token;
pub type BufToken = self::lexer::v3::BufV3Token;
impl Lexer {
    
    pub fn from(file_name: &str, messages: &mut MessageEmitter) -> Option<Lexer> {
        use std::fs::File;
        use std::io::Read;

        let mut file = match File::open(file_name) {
            Ok(file) => file,
            Err(e) => {
                messages.push(Message::CannotOpenFile { file_name: file_name.to_owned(), e: e });
                return None;
            } 
        };

        let mut content = String::new();
        match file.read_to_string(&mut content) {
            Ok(_) => (),
            Err(e) => {
                messages.push(Message::CannotReadFile { file_name: file_name.to_owned(), e: e });
                return None;
            }
        }

        Some(Lexer {
            v3: BufV3Lexer::from(V3Lexer::from(content)),
        })
    }

    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<BufToken> {
        self.v3.next(messages)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn lexer_new() {
        use super::Lexer;
        use super::MessageEmitter;

        let messages = &mut MessageEmitter::new();
        let lexer = Lexer::from("tests\\lexical\\3.sm", messages);
        if lexer.is_none() {
            perrorln!("Messages: {:?}", messages);
            return;
        }

        let mut lexer = lexer.unwrap();
        
        loop {
            match lexer.next(messages) {
                Some(bufv) => perrorln!("{:?}", bufv),
                None => break,
            }
        }
        perrorln!("Messages: {:?}", messages);
    }
}