///! fff-lang
///!
///! syntax/label
///! Label = fLabel fColon
///! it's here because for, while, loop and block all needs it

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;
use lexical::TokenStream;
use lexical::SeperatorKind;

use super::ISyntaxItem;
use super::ISyntaxItemFormat;

#[derive(Eq, PartialEq)]
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
impl ISyntaxItem for LabelDef {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() }
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { tokens.nth(index).is_label() }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<LabelDef>, usize) {

        match tokens.nth(index).get_label() {
            None => return push_unexpect!(tokens, messages, "label", index, 0),
            Some(label_name) => {
                if tokens.nth(index + 1).is_seperator(SeperatorKind::Colon) {
                    return (Some(LabelDef::new(label_name.clone(), StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2);
                } else {
                    return push_unexpect!(tokens, messages, "colon", index + 1, 1);
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::ISyntaxItemWithStr;

    assert_eq!(LabelDef::with_test_str("@1:"), LabelDef::new("1".to_owned(), make_strpos!(1, 1, 1, 3)));
}