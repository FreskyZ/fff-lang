///! fff-lang
///!
///! syntax/label
///! label-def = label ':'
///! it's here because for, while, loop and block all needs it

use crate::syntax::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
pub struct LabelDef {
    pub name: IsId,
    pub all_span: Span,
}
impl ISyntaxFormat for LabelDef {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("label").space().isid(self.name).space().span(self.all_span).finish()
    }
}
impl fmt::Debug for LabelDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(Formatter::empty())) }
}
impl LabelDef {
    
    pub fn new(name: impl Into<IsId>, all_span: Span) -> LabelDef { LabelDef{ name: name.into(), all_span } }
}
impl ISyntaxGrammar for LabelDef {
    fn matches_first(tokens: [&Token; 3]) -> bool { if let &Token::Label(_) = tokens[0] { true } else { false } }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for LabelDef where F: FileSystem {
    type Output = LabelDef;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<LabelDef> {

        if let Some((label_id, label_span)) = sess.try_expect_label() {
            let colon_span = sess.expect_sep(Separator::Colon)?;
            Ok(LabelDef::new(label_id, label_span + colon_span))
        } else {
            sess.push_unexpect("label")
        }
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    use super::super::make_node;

    assert_eq!(make_node!("@1:" as LabelDef), LabelDef::new(1, Span::new(0, 2)));
}
