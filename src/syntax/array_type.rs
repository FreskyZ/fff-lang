///! syntax::array_type:
///! array_type = '[' type_ref ';' expr ']'
///!
///! type template name will be `array` when analysis, so user type `array` should be rejected by analysis

use super::prelude::*;
use super::{TypeRef, Expr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ArrayType {
    pub base: Box<TypeRef>,
    pub size: Expr,
    pub span: Span, // all span
}

impl Parser for ArrayType {
    type Output = ArrayType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftBracket))
    }

    fn parse(cx: &mut ParseContext) -> Result<ArrayType, Unexpected> {

        let left_bracket_span = cx.expect_sep(Separator::LeftBracket)?;
        let base = Box::new(cx.expect::<TypeRef>()?);

        if let Some(right_bracket_span) = cx.try_expect_sep(Separator::RightBracket) {
            cx.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected semicolon, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Default::default(), span: left_bracket_span + right_bracket_span });
        }

        let _semicolon_span = cx.expect_sep(Separator::SemiColon)?;

        if let Some(right_bracket_span) = cx.try_expect_sep(Separator::RightBracket) {
            cx.emit(strings::InvalidArrayType)
                .detail(right_bracket_span, "expected expr, meet right bracket")
                .help(strings::ArrayTypeSyntaxHelp);
            return Ok(ArrayType{ base, size: Default::default(), span: left_bracket_span + right_bracket_span });
        }

        let size = cx.expect::<Expr>()?;
        let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
        Ok(ArrayType{ base, size, span: left_bracket_span + right_bracket_span })
    }
}

impl Node for ArrayType {

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_array_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(&self.base)?;
        v.visit_expr(&self.size)
    }
}