///! syntax::array_type:
///! array_type = '[' type_ref ';' expr ']'

use super::prelude::*;
use super::{TypeRef, Expr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ArrayType {
    pub base: TypeRef,
    pub size: Expr,
    pub span: Span, // all span
}

impl Node for ArrayType {
    type ParseOutput = ArrayType;

    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftBracket))
    }

    fn parse(cx: &mut ParseContext) -> ParseResult<ArrayType> {

        let left_bracket_span = cx.expect_sep(Separator::LeftBracket)?;
        let base = cx.expect_node::<TypeRef>()?;

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

        let size = cx.expect_node::<Expr>()?;
        let right_bracket_span = cx.expect_sep(Separator::RightBracket)?;
        Ok(ArrayType{ base, size, span: left_bracket_span + right_bracket_span })
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_array_type(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_type_ref(&self.base)?;
        v.visit_expr(&self.size)
    }
}

#[cfg(test)]
#[test]
fn array_type_parse() {

    case!{ "[i32; 5]" as ArrayType, ArrayType{
        base: TypeRef::new_simple(2, Span::new(1, 3)),
        size: make_lit!(5, 6, 6).into(),
        span: Span::new(0, 7),
    }}

    // case!{ "[[a;1]; 1 + 1 * 1 - 1 / 1]" as ArrayType, ArrayType{
    //     span: Span::new()
    // }}
}
