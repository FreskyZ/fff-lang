///! syntax::enum:
///! enum_def = 'enum' ident [ ':' primitive_type ] '{' { ident [ '=' expr ] ',' } '}'

use super::prelude::*;
use super::{PrimitiveType, Expr};

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumVariant {
    pub name: IsId,
    pub name_span: Span,
    pub value: Option<Expr>,
    pub all_span: Span,
}

impl Node for EnumVariant {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_enum_variant(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(value) = &self.value {
            v.visit_expr(value)?;
        }
        Ok(Default::default())
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct EnumDef {
    pub name: IsId,
    pub name_span: Span,
    pub base_type: Option<PrimitiveType>,
    pub quote_span: Span,
    pub variants: Vec<EnumVariant>,
    pub all_span: Span,
}

impl Node for EnumDef {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_enum_def(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(base_type) = &self.base_type {
            v.visit_primitive_type(base_type)?;
        }
        for variant in &self.variants {
            v.visit_enum_variant(variant)?;
        }
        Ok(Default::default())
    }
}

impl Parser for EnumDef {
    type Output = Self;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Keyword(Keyword::Enum))
    }

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {

        let enum_span = cx.expect_keyword(Keyword::Enum)?;
        let (enum_name, enum_name_span) = cx.expect_ident()?;
        let base_type = cx.try_expect_sep(Separator::Colon).map(|_| cx.expect::<PrimitiveType>()).transpose()?;
        let left_brace_span = cx.expect_sep(Separator::LeftBrace)?;

        let mut variants = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else {
            loop {
                let (variant_name, variant_name_span) = cx.expect_ident()?;
                let init_value = cx.try_expect_sep(Separator::Eq).map(|_| cx.expect::<Expr>()).transpose()?;
                let variant_all_span = variant_name_span + init_value.as_ref().map(|e| e.get_all_span()).unwrap_or(variant_name_span);
                variants.push(EnumVariant{ name: variant_name, name_span: variant_name_span, value: init_value, all_span: variant_all_span });

                if let Some((right_brace_span, _)) = cx.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    cx.expect_sep(Separator::Comma)?;
                }
            }
        };

        let quote_span = left_brace_span + right_brace_span;
        let all_span = enum_span + right_brace_span;
        Ok(EnumDef{ name: enum_name, name_span: enum_name_span, base_type, quote_span, variants, all_span })
    }
}

#[cfg(test)]
#[test]
fn enum_def_parse() {

    //      012345678901234567890
    case!{ "enum E { M1, M2 = 1,}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(7, 20), all_span: Span::new(0, 20), base_type: None, variants: vec![
            EnumVariant{ name: IsId::new(3), name_span: Span::new(9, 10), value: None, all_span: Span::new(9, 10) },
            EnumVariant{ name: IsId::new(4), name_span: Span::new(13, 14), value: Some(make_expr!(i32 1 18:18)), all_span: Span::new(13, 18) }]}
    }

    //      0123456789012
    case!{ "enum E: u8 {}" as EnumDef, 
        EnumDef{ name: IsId::new(2), name_span: Span::new(5, 5), quote_span: Span::new(11, 12), all_span: Span::new(0, 12), variants: Vec::new(),
            base_type: Some(PrimitiveType{ name: Keyword::U8, span: Span::new(8, 9) }), }
    }
}
