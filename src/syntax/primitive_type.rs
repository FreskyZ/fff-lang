///! syntax::primitive_type:
///! primitive_type = primitive_keyword

use super::prelude::*;

impl Parser for PrimitiveType {
    type Output = PrimitiveType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(kw) if kw.kind(KeywordKind::Primitive))
    }

    fn parse(cx: &mut ParseContext) -> Result<PrimitiveType, Unexpected> {
        let (name, span) = cx.expect_keyword_kind(KeywordKind::Primitive)?;
        Ok(PrimitiveType{ name, span })
    }
}
