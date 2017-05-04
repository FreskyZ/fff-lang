///! fff-lang
///!
///! syntax/label
///! Label = fLabel fColon
///! it's here because for, while, loop and block all needs it

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;

use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LabelDef {
    m_name: String,
    m_strpos: StringPosition,
}
impl ISyntaxItemFormat for LabelDef {
    fn format(&self, indent: u32) -> String {
        format!("{}Label {} <{:?}>", LabelDef::indent_str(indent), self.m_name, self.m_strpos)
    }
}
impl fmt::Debug for LabelDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}
impl LabelDef {
    
    pub fn new(name: String, strpos: StringPosition) -> LabelDef { LabelDef{ m_name: name, m_strpos: strpos } }

    pub fn get_all_strpos(&self) -> StringPosition { self.m_strpos }
    pub fn get_name(&self) -> &String { &self.m_name }

    pub fn into(self) -> (String, StringPosition) { (self.m_name, self.m_strpos) }
}
impl ISyntaxItemGrammar for LabelDef {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { if let &Token::Label(_) = tokens.nth(index) { true } else { false } }
}
impl ISyntaxItemParse for LabelDef {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<LabelDef>, usize) {

        match (tokens.nth(index), tokens.nth(index + 1)) {
            (&Token::Label(ref label_name), &Token::Sep(SeperatorKind::Colon)) => 
                (Some(LabelDef::new(label_name.clone(), StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2),
            (&Token::Label(_), _) =>
                push_unexpect!(tokens, messages, "colon", index + 1, 1),
            _ => 
                push_unexpect!(tokens, messages, "label", index, 0),
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!(LabelDef::with_test_str("@1:"), LabelDef::new("1".to_owned(), make_strpos!(1, 1, 1, 3)));
}