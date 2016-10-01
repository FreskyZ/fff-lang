
mod symbol_type;
mod lexer;

use message::MessageEmitter;
pub use lexical::symbol_type::keyword_kind::KeywordKind;
pub use lexical::symbol_type::seperator_kind::SeperatorKind;
use lexical::lexer::v3::V3Lexer;
use lexical::lexer::v3::BufV3Lexer;
pub type Token = self::lexer::v3::V3Token;
pub type BufToken = self::lexer::v3::BufV3Token;

pub trait ILexer<TToken> {
    fn next(&mut self, emitter: &mut MessageEmitter) -> Option<TToken>;
}

pub struct Lexer {
    v3: BufV3Lexer,
}

impl Lexer {
    
    pub fn from(content: String) -> Lexer {
        Lexer { v3: BufV3Lexer::from(V3Lexer::from(content)) }
    }

    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<BufToken> {
        self.v3.next(messages)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn lexer_new() {
        // use super::Lexer;
        // use message::MessageEmitter;

        // let messages = &mut MessageEmitter::new();
        // let lexer = Lexer::from("tests\\lexical\\3.sm", messages);
        // if lexer.is_none() {
        //     perrorln!("Messages: {:?}", messages);
        //     return;
        // }

        // let mut lexer = lexer.unwrap();
        
        // loop {
        //     match lexer.next(messages) {
        //         Some(bufv) => perrorln!("{:?}", bufv),
        //         None => break,
        //     }
        // }
        // perrorln!("Messages: {:?}", messages);
    }
}

// TODOs
// string return something even if meet unexpected EOF
// v1 new test
// numeric literal parser
// try pass these source files