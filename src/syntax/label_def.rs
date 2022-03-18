///! fff-lang
///!
///! syntax/label
///! label-def = label ':'
///! it's here because for, while, loop and block all needs it

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct LabelDef {
    pub name: IsId,
    pub all_span: Span,
}
impl LabelDef {
    pub fn new(name: impl Into<IsId>, all_span: Span) -> LabelDef { 
        LabelDef{ name: name.into(), all_span } 
    }
}

impl Parser for LabelDef {
    type Output = LabelDef;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Label(_))
    }

    fn parse(cx: &mut ParseContext) -> Result<LabelDef, Unexpected> {

        if let Some((label_id, label_span)) = cx.try_expect_label() {
            let colon_span = cx.expect_sep(Separator::Colon)?;
            Ok(LabelDef::new(label_id, label_span + colon_span))
        } else {
            cx.push_unexpect("label")
        }
    }
}

impl Node for LabelDef {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_label_def(self)
    }
}

#[cfg(test)] #[test]
fn label_def_parse() {
    case!{ "@1:" as LabelDef, LabelDef::new(2, Span::new(0, 2)) }
}
