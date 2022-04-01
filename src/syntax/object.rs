///! syntax::object:
///! object_literal = name '{' { ident ':' expr ',' } '}'
///!
///! last comma may omit

use super::prelude::*;

impl Node for ObjectLiteralField {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_object_literal_field(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.value)
    }
}

impl Parser for ObjectLiteral {
    type Output = Self;
    
    fn matches(current: &Token) -> bool {
        matches!(current, Token::Sep(Separator::LeftBrace))
    }

    fn parse(cx: &mut ParseContext) -> Result<Self, Unexpected> {
        
        let left_brace_span = cx.expect_sep(Separator::LeftBrace)?;
        let mut fields = Vec::new();
        let right_brace_span = if let Some(right_brace_span) = cx.try_expect_sep(Separator::RightBrace) {
            right_brace_span
        } else { loop {
                let (field_name, field_name_span) = cx.expect_ident()?;
                let colon_span = cx.expect_sep(Separator::Colon)?;
                let value = cx.expect::<Expr>()?;
                fields.push(ObjectLiteralField{ all_span: field_name_span + value.get_all_span(), name: field_name, name_span: field_name_span, colon_span, value });

                if let Some((right_brace_span, _)) = cx.try_expect_closing_bracket(Separator::RightBrace) {
                    break right_brace_span;
                } else {
                    cx.expect_sep(Separator::Comma)?;
                }
            }
        };

        Ok(ObjectLiteral{ base: Box::new(Expr::default()), quote_span: left_brace_span + right_brace_span, fields, all_span: Span::new(0, 0) })
    }
}
