///! syntax::ref_type:
///! ref_type = '&' type_ref

use super::prelude::*;
use super::{TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct RefType {
    pub base: Box<TypeRef>,
    pub span: Span, // all span
}

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

impl Node for RefType {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_ref_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(&self.base)
    }
}
