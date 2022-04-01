///! syntax::fn_call_expr:
///! fn_call_expr = expr '(' [ expr_list ] ')'

use super::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct FnCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub paren_span: Span,
    pub all_span: Span,
}

impl Parser for FnCallExpr {
    type Output = FnCallExpr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftParen)) 
    }

    fn parse(cx: &mut ParseContext) -> Result<FnCallExpr, Unexpected> {

        // this parse method return partial and priority proxy will fill base
        macro_rules! make_partial { ($span:expr, $params:expr) => (
            Ok(FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: $span, params: $params })) }

        match cx.expect::<ExprList>()? {
            | ExprListParseResult::Normal(span, expr_list) 
            | ExprListParseResult::EndWithComma(span, expr_list) => make_partial!(span, expr_list),
            ExprListParseResult::Empty(span) => make_partial!(span, ExprList{ items: Vec::new() }),
            ExprListParseResult::SingleComma(span) => {
                cx.emit(strings::UnexpectedSingleComma).detail(span, strings::FnCallHere);
                make_partial!(span, ExprList{ items: Vec::new() })
            }
        }
    }
}

impl Node for FnCallExpr {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_call_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.base)?;
        v.visit_expr_list(&self.params)
    }
}

#[cfg(test)]
#[test]
fn fn_call_parse() {

    case!{ "()" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 1), params: ExprList{ items: Vec::new() } }
    }

    case!{ "(\"hello\")" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 8), params: ExprList{ items: vec![
            make_expr!(str #2 1:7),
        ] } }
    }

    case!{ "(,)" as FnCallExpr,
        FnCallExpr{ all_span: Span::new(0, 0), base: Box::new(Expr::default()), paren_span: Span::new(0, 2), params: ExprList{ items: Vec::new() } },
        errors make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::FnCallHere))
    }
}
