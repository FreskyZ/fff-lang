
// Lexer public interface
mod symbol_type;
mod buf_lexer;
mod v0lexer;
mod v1lexer;
mod v2lexer;
mod v3lexer;
mod v4lexer;

use self::v3lexer::V3Lexer;
pub use self::v3lexer::V3Token as Token;

pub use self::symbol_type::Seperator;
pub use self::symbol_type::SeperatorKind;
pub use self::symbol_type::Keyword;
pub use self::symbol_type::KeywordKind;
pub use self::symbol_type::CharLiteral;
pub use self::symbol_type::StringLiteral;
pub use self::symbol_type::NumericLiteral;
pub use self::symbol_type::NumericLiteralValue;
pub use self::symbol_type::BooleanLiteral;
pub use self::symbol_type::Identifier;

use common::From2;
use common::StringPosition;
use message::Message;
use message::MessageEmitter;

pub trait IToken {

    fn is_keyword(&self, kind: KeywordKind) -> bool;
    fn is_seperator(&self, kind: SeperatorKind) -> bool;
    fn is_identifier(&self, name: &str) -> bool;
    fn is_str_lit(&self) -> bool;
    fn is_num_lit(&self) -> bool;
    fn is_char_lit(&self) -> bool;
    fn is_bool_lit(&self) -> bool;

    // clone them if need
    fn get_keyword(&self) -> Option<&KeywordKind>;
    fn get_seperator(&self) -> Option<&SeperatorKind>;
    fn get_identifier(&self) -> Option<&String>;
}

impl IToken for Option<Token> {

    fn is_keyword(&self, kind: KeywordKind) -> bool {
        match *self {
            Some(ref token) => token.is_keyword(kind),
            None => false,
        }
    }
    fn is_seperator(&self, kind: SeperatorKind) -> bool {
        match *self {
            Some(ref token) => token.is_seperator(kind),
            None => false,
        }
    }
    fn is_identifier(&self, name: &str) -> bool {
        match *self {
            Some(ref token) => token.is_identifier(name),
            None => false,
        }
    }

    fn is_str_lit(&self) -> bool {
        match *self {
            Some(ref token) => token.is_str_lit(),
            None => false,
        }
    }
    fn is_num_lit(&self) -> bool {
        match *self {
            Some(ref token) => token.is_num_lit(),
            None => false,
        }
    }
    fn is_char_lit(&self) -> bool {
        match *self {
            Some(ref token) => token.is_char_lit(),
            None => false,
        }
    }
    fn is_bool_lit(&self) -> bool {
        match *self {
            Some(ref token) => token.is_bool_lit(),
            None => false,
        }
    }

    fn get_keyword(&self) -> Option<&KeywordKind> {
        match *self {
            Some(Token::Keyword(Keyword{ ref kind, pos: ref _1 })) => Some(kind),
            _ => None,
        }
    }
    fn get_seperator(&self) -> Option<&SeperatorKind> {
        match *self {
            Some(Token::Seperator(Seperator{ ref kind, pos: ref _1 })) => Some(kind),
            _ => None,
        }
    }
    fn get_identifier(&self) -> Option<&String> {
        match *self {
            Some(Token::Identifier(Identifier{ ref name, pos: ref _1 })) => Some(name),
            _ => None,
        }
    }
}

// Full buf v3 lexer
pub struct Lexer {
    buf: Vec<Token>,
    eof_pos: StringPosition,
    messages: MessageEmitter,
    push_enable: bool,
    push_enable_stack: Vec<bool>
}

impl Lexer {

    pub fn from(content: String, mut messages: MessageEmitter) -> Lexer {
        use self::buf_lexer::ILexer;

        let mut v3lexer = V3Lexer::from(content);
        let mut buf = Vec::new();
        loop {
            match v3lexer.next(&mut messages) {
                Some(token) => buf.push(token),
                None => break,
            }
        }

        Lexer{ 
            buf: buf, 
            messages: messages,
            eof_pos: StringPosition::from2(v3lexer.position(), v3lexer.position()),  
            push_enable: true, 
            push_enable_stack: Vec::new(),
        }
    }

    pub fn sym_pos(&self, idx: usize) -> StringPosition { 
        if idx >= self.buf.len() { 
            self.eof_pos
        } else {
            self.buf[idx].position()
        }
    }

    pub fn nth(&self, idx: usize) -> Option<Token> {
        if idx >= self.buf.len() { 
            None 
        } else { 
            Some(self.buf[idx].clone()) 
        }
    }

    // MessageEmitter
    pub fn push(&mut self, message: Message) {
        self.messages.push(message);
    }
    pub fn push_ret_none<T>(&mut self, message: Message) -> Option<T> {
        self.messages.push(message);
        return None;
    }
    pub fn push_expect_symbol<T>(&mut self, desc: &str, sym_pos_index: usize) -> Option<T> {
        self.messages.push(Message::ExpectSymbol{ 
            desc: desc.to_owned(), 
            pos: if sym_pos_index >= self.buf.len() { self.eof_pos.start_pos } else { self.buf[sym_pos_index].position().start_pos },
        });
        return None;
    }
    pub fn emitter(&self) -> &MessageEmitter {
        &self.messages
    }

    /// ATTENTION!!! push enable mechanism is not working currently, judge whether use it in future
    /// only IASTItem::parse 's caller knows whether this is try, so that is, 
    /// push_temp_message, pop_temp_message, apply_temp_message(to current level, maybe higher level's temp messages)

    // MessageEmitter enable
    pub fn push_push_enable(&mut self, new_enable: bool) {
        self.push_enable_stack.push(self.push_enable);
        self.push_enable = new_enable;
    }
    /// Attention: will cover over popped panic
    pub fn pop_push_enable(&mut self) {
        self.push_enable = self.push_enable_stack.pop().unwrap_or(true);
    }
    
    #[cfg(test)]
    pub fn from_test(content: &str, messages: MessageEmitter) -> Lexer {
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
        use super::Seperator;

        // numeric, 123, 1:1-1:3
        // identifier, abc, 1:5-1:7
        // char, 'd', 1:9-1:11
        // seperator, comma, 1:12-1:12
        // seperator, leftbracket, 1:14-1:14
        // numeric, 1, 1:15-1:15
        // seperator, rightbracket, 1:16-1:16
        let messages = MessageEmitter::new();
        let lexer = Lexer::from_test("123 abc 'd', [1]", messages);

        assert_eq!(lexer.nth(0), Some(Token::NumericLiteral(NumericLiteral{ value: Some(NumericLiteralValue::I32(123)), pos: StringPosition::from((1, 1, 1, 3)) } )));
        assert_eq!(lexer.nth(2), Some(Token::CharLiteral(CharLiteral{ value: Some('d'), pos: StringPosition::from((1, 9, 1, 11)) })));
        assert_eq!(lexer.nth(6), Some(Token::Seperator(Seperator{ kind: SeperatorKind::RightBracket, pos: StringPosition::from((1, 16, 1, 16)) })));
        assert_eq!(lexer.nth(7), None);
        assert_eq!(lexer.nth(8), None);

        assert_eq!(lexer.sym_pos(0), StringPosition::from((1, 1, 1, 3)));
        assert_eq!(lexer.sym_pos(1), StringPosition::from((1, 5, 1, 7)));
        assert_eq!(lexer.sym_pos(2), StringPosition::from((1, 9, 1, 11)));
        assert_eq!(lexer.sym_pos(3), StringPosition::from((1, 12, 1, 12)));
        assert_eq!(lexer.sym_pos(4), StringPosition::from((1, 14, 1, 14)));
        assert_eq!(lexer.sym_pos(5), StringPosition::from((1, 15, 1, 15)));
        assert_eq!(lexer.sym_pos(6), StringPosition::from((1, 16, 1, 16)));
        assert_eq!(lexer.sym_pos(7), StringPosition::from((1, 17, 1, 17)));
        assert_eq!(lexer.sym_pos(8), StringPosition::from((1, 17, 1, 17)));
    }
}