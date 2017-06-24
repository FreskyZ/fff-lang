///! fff-lang
///!
///! syntax/label
///! Label = fLabel fColon
///! it's here because for, while, loop and block all needs it

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use lexical::Token;
use lexical::Seperator;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LabelDef {
    pub name: SymbolID,
    pub all_span: Span,
}
impl ISyntaxItemFormat for LabelDef {
    fn format(&self, indent: u32) -> String {
        format!("{}Label {:?} <{:?}>", LabelDef::indent_str(indent), self.name, self.all_span)
    }
}
impl fmt::Debug for LabelDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl LabelDef {
    
    pub fn new(name: SymbolID, all_span: Span) -> LabelDef { LabelDef{ name, all_span } }
}
impl ISyntaxItemGrammar for LabelDef {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Label(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for LabelDef {
    type Target = LabelDef;

    fn parse(sess: &mut ParseSession) -> ParseResult<LabelDef> {

        match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
            (&Token::Label(ref label_name), ref label_name_strpos,
                &Token::Sep(Seperator::Colon), ref colon_strpos) => {
                sess.move_next2();
                Ok(LabelDef::new(label_name.clone(), label_name_strpos.merge(colon_strpos)))
            }
            (&Token::Label(_), _, _, _) => sess.push_unexpect("colon"),
            _ => sess.push_unexpect("label"),
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!(LabelDef::with_test_str("@1:"), LabelDef::new(make_id!(1), make_span!(0, 2)));
}