///! fff-lang
///!
///! syntax/parse_sess, for ISyntaxItem::parse's parameter

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;
use lexical::IToken;
use lexical::TokenStream;

pub struct ParseSession<'tokens, 'msgs> {
    m_tokens: &'tokens TokenStream,
    m_messages: &'msgs mut MessageCollection,
    m_current_index: usize,
    m_current_length: usize,
}

impl<'a, 'b> ParseSession<'a, 'b> {

    pub fn set_current_index(&self);
    pub fn add_current_length(&self);

    pub fn nth_token(&self) -> &IToken;
    pub fn nth_strpos(&self) -> StringPosition;

    pub fn preview_token(&self, offset: usize) -> &IToken;
    pub fn preview_strpos(&self, offset: usize) -> StringPosition;

    pub fn push_message(&self, message: Message);
}

// TODO
// ISyntaxItem::parse(sess: &PraseSession) -> Option<Self>;