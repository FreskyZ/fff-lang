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

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
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
        format!("{}Label '@{}' <{:?}>", LabelDef::indent_str(indent), self.m_name, self.m_strpos)
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
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for LabelDef {
    fn is_first_finalx(sess: &ParseSession) -> bool { if let &Token::Label(_) = sess.tk { true } else { false } }
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
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for LabelDef {

    fn parsex(sess: &mut ParseSession) -> ParseResult<LabelDef> {

        match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
            (&Token::Label(ref label_name), ref label_name_strpos,
                &Token::Sep(SeperatorKind::Colon), ref colon_strpos) => {
                sess.move_next2();
                Ok(LabelDef::new(label_name.clone(), StringPosition::merge(*label_name_strpos, *colon_strpos)))
            }
            (&Token::Label(_), _, _, _) => sess.push_unexpect("colon"),
            _ => sess.push_unexpect("label"),
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!(LabelDef::with_test_str("@1:"), LabelDef::new("1".to_owned(), make_strpos!(1, 1, 1, 3)));
}