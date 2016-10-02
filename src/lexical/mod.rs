
mod symbol_type;
mod buf_lexer;
mod v0;
mod v1;
mod v2;
mod v3;

use message::MessageEmitter;
use lexical::v3::V3Lexer;
use lexical::v3::BufV3Lexer;
pub type Token = self::v3::V3Token;
pub type BufToken = self::v3::BufV3Token;

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

// TODOs
// numeric literal parser
// char parser optimize
// v0 and its raw accessor to lexical::v0
// v1 and its 3 parsers to lexical::v1
// v2 and its numeric parser to lexical::v2
// v3 and its seperator and keyword parser to lexical::v3