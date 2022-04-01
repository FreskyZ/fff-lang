///! syntax::tuple_type:
///! tuple_type = '(' type_ref { ',' type_ref } [ ',' ] ')'
///!
///! empty for unit type, one element tuple require ending comma
///! type template name will be `tuple` when analysis, so user type `tuple` should be rejected by analysis

use super::prelude::*;

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
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                if !skipped_comma && items.len() == 1 {
                    cx.emit(strings::SingleItemTupleType)
                        .detail(right_paren_span, strings::TupleTypeExpectCommaMeetRightParen);
                }
                break right_paren_span;
            } else {
                cx.expect_sep(Separator::Comma)?;
                items.push(cx.expect::<TypeRef>()?);
            }
        };

        Ok(TupleType{ items, span })
    }
}

