///! fff-lang
///!
///! syntax/label
///! label-def = label ':'
///! it's here because for, while, loop and block all needs it

use super::prelude::*;

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

#[cfg(test)] #[test]
fn label_def_parse() {
    case!{ "@1:" as LabelDef, LabelDef::new(2, Span::new(0, 2)) }
}
