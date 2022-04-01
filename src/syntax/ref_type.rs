///! syntax::ref_type:
///! ref_type = '&' type_ref

use super::prelude::*;

impl Parser for RefType {
    type Output = RefType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::And | Separator::AndAnd))
    }

    fn parse(cx: &mut ParseContext) -> Result<RefType, Unexpected> {
        
        let and_span = cx.expect_sep(Separator::And)?;
        let base = cx.expect::<TypeRef>()?;
        Ok(RefType{ span: and_span + base.get_all_span(), base: Box::new(base) })
    }
}
