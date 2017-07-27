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

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct LabelDef {
    pub name: SymbolID,
    pub all_span: Span,
}
impl ISyntaxFormat for LabelDef {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("label").space().sym(self.name).space().span(self.all_span).finish()
    }
}
impl fmt::Debug for LabelDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
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

        if let Some((label_id, label_span)) = sess.try_expect_label() {
            let colon_span = sess.expect_sep(Seperator::Colon)?;
            Ok(LabelDef::new(label_id, label_span.merge(&colon_span)))
        } else {
            sess.push_unexpect("label")
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::super::WithTestInput;

    assert_eq!(LabelDef::with_test_str("@1:"), LabelDef::new(make_id!(1), make_span!(0, 2)));
}