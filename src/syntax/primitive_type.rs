///! syntax::primitive_type:
///! primitive_type = primitive_keyword

use super::prelude::*;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct PrimitiveType {
    pub name: Keyword,
    pub span: Span,
}

impl Node for PrimitiveType {
    type ParseOutput = PrimitiveType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(kw) if kw.kind(KeywordKind::Primitive))
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<PrimitiveType> {
        let (name, span) = cx.expect_keyword_kind(KeywordKind::Primitive)?;
        Ok(PrimitiveType{ name, span })
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_primitive_type(self)
    }
}

#[cfg(test)]
#[test]
fn primitive_type_parse() {

    case!{ "u8" as PrimitiveType, PrimitiveType{ name: Keyword::U8, span: Span::new(0, 1) } }
    case!{ "i32" as PrimitiveType, PrimitiveType{ name: Keyword::I32, span: Span::new(0, 2) } }
}
