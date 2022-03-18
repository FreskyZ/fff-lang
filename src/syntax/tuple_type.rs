///! syntax::tuple_type:
///! tuple_type = '(' type_ref { ',' type_ref } [ ',' ] ')'
///!
///! empty for unit type, one element tuple require ending comma
///! type template name will be `tuple` when analysis, so user type `tuple` should be rejected by analysis

use super::prelude::*;
use super::{TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct TupleType {
    pub items: Vec<TypeRef>,
    pub span: Span,
}

impl Parser for TupleType {
    type Output = TupleType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftParen))
    }

    fn parse(cx: &mut ParseContext) -> Result<TupleType, Unexpected> {
        
        let left_paren_span = cx.expect_sep(Separator::LeftParen)?;
        if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
            return Ok(Self{ items: Vec::new(), span: left_paren_span + right_paren_span });
        }
        
        let mut items = vec![cx.expect::<TypeRef>()?];
        let span = left_paren_span + loop {
            if let Some((_, right_paren_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                break right_paren_span;
            } else if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
                if items.len() == 1 {
                    cx.emit(strings::SingleItemTupleType)
                        .detail(right_paren_span, strings::TupleTypeExpectCommaMeetRightParen);
                }
                break right_paren_span
            } else {
                items.push(cx.expect::<TypeRef>()?);
            }
        };

        Ok(TupleType{ items, span })
    }
}

impl Node for TupleType {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_tuple_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for item in &self.items {
            v.visit_type_ref(item)?;
        }
        Ok(Default::default())
    }
}
