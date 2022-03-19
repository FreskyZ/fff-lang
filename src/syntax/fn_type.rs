///! syntax::fn_type:
///! fn_type = 'fn' '(' [ ident ':' ] type_ref { ',' [ ident ':' ] type_ref } [ ',' ] ')' [ '->' type_ref ]
///!
///! - return type in fn type and fn def is not colon but arrow: https://mail.mozilla.org/pipermail/rust-dev/2013-July/005042.html
///! - parameter name is optional and does not affect type identity
///!   type ref may start with ident, actually most common type refs start with ident, 
///!   so it is ambiguous and that may be the reason rust does not support that, but I always want to add parameter name to make function type more clear,
///!   so they are distinguished by always parseing type ref and very simple result (only one identifier) followed with colon is regarded as parameter name

use super::prelude::*;
use super::{TypeRef, PlainType};

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
        let right_paren_span = loop {
            if let Some((right_paren_span, skipped_comma)) = cx.try_expect_closing_bracket(Separator::RightParen) {
                if skipped_comma && parameters.is_empty() {
                    // TODO: need comma span
                    cx.emit("unexpected token").detail(right_paren_span, "expected ident, type or right paren, meet comma");
                }
                break right_paren_span;
            } else if !parameters.is_empty() {
                cx.expect_sep(Separator::Comma)?;
            }
            // these can-regard-as-variable keywords are not expected by type ref, they are definitely parameter name
            let name = cx.try_expect_keywords(&[Keyword::This, Keyword::Self_, Keyword::Underscore]);
            if name.is_some() {
                cx.expect_sep(Separator::Colon)?;
            }
            let r#type = cx.expect::<TypeRef>()?;
            let (name, r#type) = if let TypeRef::Plain(PlainType{ type_as_segment: None, global: false, segments, .. }) = &r#type {
                if name.is_none() // this one should be before previous let r#type but that will make it 3 ifs are too more (None, r#type)s
                    && segments.len() == 1 && segments[0].parameters.is_empty() && cx.try_expect_sep(Separator::Colon).is_some() {
                    (Some((segments[0].ident, segments[0].ident_span)), cx.expect::<TypeRef>()?)
                } else {
                    (name.map(|(kw, span)| (cx.intern(kw.display()), span)), r#type)
                }
            } else {
                (name.map(|(kw, span)| (cx.intern(kw.display()), span)), r#type)
            };
            parameters.push(FnTypeParam{ name, all_span: name.map(|(_, name_span)| name_span).unwrap_or_else(|| r#type.get_all_span()) + r#type.get_all_span(), r#type });
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
