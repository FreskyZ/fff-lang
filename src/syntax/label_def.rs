///! fff-lang
///!
///! syntax/label
///! label-def = label ':'
///! it's here because for, while, loop and block all needs it

use super::prelude::*;

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
impl Node for LabelDef {
    type ParseOutput = LabelDef;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Label(_))
    }

    fn parse<F>(sess: &mut ParseSession<F>) -> ParseResult<LabelDef> where F: FileSystem {

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
    use super::make_node;

    assert_eq!(make_node!("@1:" as LabelDef), LabelDef::new(1, Span::new(0, 2)));
}