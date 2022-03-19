///! syntax::fn_type:
///! fn_type = 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
///!
///! parameter name is optional and does not affect type identity
///! return type in fn type and fn def is not colon but arrow: https://mail.mozilla.org/pipermail/rust-dev/2013-July/005042.html

use super::prelude::*;
use super::{TypeRef};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnTypeParam {
    pub name: Option<(IsId, Span)>,
    pub r#type: TypeRef,
    pub all_span: Span,
}

impl Node for FnTypeParam {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_type_param(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(&self.r#type)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnType {
    pub paren_span: Span,
    pub parameters: Vec<FnTypeParam>,
    pub ret_type: Option<Box<TypeRef>>,
    pub all_span: Span,
}

impl Parser for FnType {
    type Output = FnType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::Fn))
    }

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {
        
        let fn_span = cx.expect_keyword(Keyword::Fn)?;
        let left_paren_span = cx.expect_sep(Separator::LeftParen)?;

        let mut parameters = Vec::new();
        let right_paren_span = if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
            right_paren_span
        } else {
            loop {
                let name = cx.try_expect_ident_or_keywords(&[Keyword::This, Keyword::Self_, Keyword::Underscore]);
                if name.is_some() {
                    cx.expect_sep(Separator::Colon)?;
                }
                let r#type = cx.expect::<TypeRef>()?;
                let span = name.map(|(_, s)| s).unwrap_or(r#type.get_all_span()) + r#type.get_all_span();
                parameters.push(FnTypeParam{ name, r#type, all_span: span });

                if let Some((_, right_paren_span)) = cx.try_expect_2_sep(Separator::Comma, Separator::RightParen) {
                    break right_paren_span;
                } else if let Some(right_paren_span) = cx.try_expect_sep(Separator::RightParen) {
                    break right_paren_span;
                } else {
                    cx.expect_sep(Separator::Comma)?;
                }
            }
        };

        let ret_type = cx.try_expect_seps(&[Separator::Arrow, Separator::Colon]).map(|(sep, span)| {
            if sep == Separator::Colon {
                cx.emit(strings::FunctionReturnTypeShouldUseArrow).detail(span, strings::FunctionReturnTypeExpectArrowMeetColon);
            }
            cx.try_expect::<TypeRef>()
        }).transpose()?.flatten();
        
        let all_span = fn_span + ret_type.as_ref().map(|t| t.get_all_span()).unwrap_or(right_paren_span);
        Ok(FnType{ paren_span: left_paren_span + right_paren_span, parameters, ret_type: ret_type.map(Box::new), all_span })
    }
}

impl Node for FnType {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        for parameter in &self.parameters {
            v.visit_fn_type_param(parameter)?;
        }
        if let Some(ret_type) = &self.ret_type {
            v.visit_type_ref(ret_type.as_ref())?;
        }
        Ok(Default::default())
    }
}
