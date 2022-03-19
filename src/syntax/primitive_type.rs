///! syntax::primitive_type:
///! primitive_type = primitive_keyword

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct PrimitiveType {
    pub name: Keyword,
    pub span: Span,
}

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

impl Node for PrimitiveType {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_primitive_type(self)
    }
}
