///! fff-lang
///!
///! syntax/parse_sess, to be ISyntaxItem::parse's parameter

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;
use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;

pub struct ParseSession<'tokens, 'msgs> {
    tokens: &'tokens TokenStream,
    messages: &'msgs mut MessageCollection,
    current_index: usize,
}

impl<'a, 'b> ParseSession<'a, 'b> {

    pub fn new(tokens: &'a TokenStream, messages: &'b mut MessageCollection) -> ParseSession<'a, 'b> {
        ParseSession{ tokens, messages, current_index: 0 }
    }

    // pub fn current_token_is_seperator2(&self, sep1: SeperatorKind, sep2: SeperatorKind) -> bool {
    //     self.tokens.nth(self.current_index).is_seperator(sep1) && self.tokens.nth(self.current_index + 1).is_seperator(sep2)
    // }

    // pub fn get_current_token(&self) -> &Token { self.tokens.nth(self.current_index) }
    // pub fn get_current_strpos(&self) -> StringPosition { self.tokens.pos(self.current_index) }

    // pub fn consume_current_token(&mut self) -> &Token {
    //     let retval = self.tokens.nth(self.current_index);
    //     self.current_index += 1;
    //     return retval;
    // }
    // pub fn consume_current_strpos(&mut self) -> StringPosition { 
    //     let retval = self.tokens.pos(self.current_index);
    //     self.current_index += 1;
    //     return retval;
    // }

    // pub fn skip_current_token(&mut self) { self.current_index += 1; }

    // pub fn push_message(&mut self, message: Message) { self.messages.push(message); }
    // pub fn push_unexpect<T>(&mut self, expect_desc: &str) -> Option<T> {

    //     self.messages.push(Message::with_help("Unexpect symbol".to_owned(), 
    //         vec![(self.tokens.pos(self.current_index), format!("Meet {:?}", self.tokens.nth(self.current_index)))],
    //         vec![format!("Expect {}", expect_desc)]
    //     ));

    //     return None;
    // }
}
