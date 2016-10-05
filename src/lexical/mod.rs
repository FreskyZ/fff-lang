
mod symbol_type;
mod buf_lexer;
mod v0;
mod v1;
mod v2;
mod v3;

use self::v3::V3Lexer;
pub use self::v3::V3Token as Token;
pub type BufToken = self::buf_lexer::BufToken<Token>;
pub type BufLexer = self::buf_lexer::BufLexer<V3Lexer, Token>;

pub use self::symbol_type::SeperatorKind;
pub use self::symbol_type::KeywordKind;
pub use self::symbol_type::CharLiteral;
pub use self::symbol_type::StringLiteral;
pub use self::symbol_type::NumericLiteral;
pub use self::symbol_type::NumericLiteralValue;

use common::From2;
use common::StringPosition;
use message::MessageEmitter;

// Full buf v3 lexer
pub struct Lexer {
    v3: V3Lexer,
    buf: Vec<Token>,
    buf_index: usize,
}

pub struct Snapshot {
    // currently only index
    buf_index: usize,
}

impl Lexer {

    pub fn from(content: String, messages: &mut MessageEmitter) -> Lexer {
        use self::buf_lexer::ILexer;

        let mut v3lexer = V3Lexer::from(content);
        let mut buf = Vec::new();
        loop {
            match v3lexer.next(messages) {
                Some(token) => buf.push(token),
                None => break,
            }
        }

        Lexer{ v3: v3lexer, buf: buf, buf_index: 0 }
    }

    pub fn position(&self) -> StringPosition { 
        match self.nth(0) {
            Some(token) => token.position(),
            None => StringPosition::from2(self.v3.position(), self.v3.position()),
        }
    }

    // move forward n steps
    pub fn forward(&mut self, steps: usize) {
        self.buf_index = if self.buf_index + steps >= self.buf.len() { self.buf.len() } else { self.buf_index + steps }; 
    }

    // 0 for current, 1, 2... for preview
    // exceed high bound return none 
    pub fn nth(&self, idx: usize) -> Option<&Token> {
        if self.buf_index + idx >= self.buf.len() { 
            None 
        } else { 
            Some(&self.buf[self.buf_index + idx]) 
        }
    }

    // snapshot
    pub fn take_snapshot(&self) -> Snapshot {
        Snapshot{ buf_index: self.buf_index }
    }
    pub fn recover_snapshot(&mut self, snapshot: Snapshot) {
        self.buf_index = snapshot.buf_index;
    }

    #[cfg(test)]
    pub fn from_test(content: &str, messages: &mut MessageEmitter) -> Lexer {
        Lexer::from(content.to_owned(), messages)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn lexer_index() {
        use common::StringPosition;
        use message::MessageEmitter;
        use super::Lexer;
        use super::Token;
        use super::NumericLiteral;
        use super::NumericLiteralValue;
        use super::SeperatorKind;
        use super::CharLiteral;

        // numeric, 123, 1:1-1:3
        // identifier, abc, 1:5-1:7
        // char, 'd', 1:9-1:11
        // seperator, comma, 1:12-1:12
        // seperator, leftbracket, 1:14-1:14
        // numeric, 1, 1:15-1:15
        // seperator, rightbracket, 1:16-1:16
        let messages = &mut MessageEmitter::new();
        let mut lexer = Lexer::from_test("123 abc 'd', [1]", messages);

        assert_eq!(lexer.nth(0), Some(&Token::NumericLiteral{ inner: NumericLiteral{ value: Some(NumericLiteralValue::I32(0)), pos: StringPosition::from((1, 1, 1, 3)) } }));
        assert_eq!(lexer.nth(2), Some(&Token::CharLiteral{ inner: CharLiteral{ value: Some('d'), pos: StringPosition::from((1, 9, 1, 11)) } }));
        assert_eq!(lexer.nth(6), Some(&Token::Seperator{ kind: SeperatorKind::RightBracket, pos: StringPosition::from((1, 16, 1, 16)) }));
        assert_eq!(lexer.nth(7), None);
        let snapshot = lexer.take_snapshot();
        lexer.forward(3);
        assert_eq!(lexer.nth(1), Some(&Token::Seperator{ kind: SeperatorKind::LeftBracket, pos: StringPosition::from((1, 14, 1, 14)) }));
        assert_eq!(lexer.nth(0), Some(&Token::Seperator{ kind: SeperatorKind::Comma, pos: StringPosition::from((1, 12, 1, 12)) }));
        assert_eq!(lexer.nth(3), Some(&Token::Seperator{ kind: SeperatorKind::RightBracket, pos: StringPosition::from((1, 16, 1, 16)) }));
        assert_eq!(lexer.nth(4), None);
        lexer.recover_snapshot(snapshot);
        assert_eq!(lexer.nth(0), Some(&Token::NumericLiteral{ inner: NumericLiteral{ value: Some(NumericLiteralValue::I32(0)), pos: StringPosition::from((1, 1, 1, 3)) } }));
        assert_eq!(lexer.nth(2), Some(&Token::CharLiteral{ inner: CharLiteral{ value: Some('d'), pos: StringPosition::from((1, 9, 1, 11)) } }));
        assert_eq!(lexer.nth(6), Some(&Token::Seperator{ kind: SeperatorKind::RightBracket, pos: StringPosition::from((1, 16, 1, 16)) }));
        assert_eq!(lexer.nth(7), None);
        lexer.forward(7);
        assert_eq!(lexer.nth(0), None);
        assert_eq!(lexer.nth(1), None);
        assert_eq!(lexer.nth(100), None);
    }
}

// TODOs
// numeric literal parser