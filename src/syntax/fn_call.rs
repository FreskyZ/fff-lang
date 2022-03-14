///! fff-lang
///!
///! syntax/fn_call_expr
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
impl From<FnCallExpr> for Expr {
    fn from(fn_call_expr: FnCallExpr) -> Expr { Expr::FnCall(fn_call_expr) }
}
impl FnCallExpr {

    pub fn new<T: Into<Expr>>(base: T, paren_span: Span, params: ExprList) -> FnCallExpr {
        let base = base.into();
        FnCallExpr{
            all_span: base.get_all_span() + paren_span,
            base: Box::new(base),
            params,
            paren_span,
        }
    }

    fn new_with_parse_result(paren_span: Span, params: ExprList) -> FnCallExpr {
        FnCallExpr{
            all_span: Span::new(0, 0), 
            base: Box::new(Expr::default()),
            paren_span, params
        }
    }
}
impl Node for FnCallExpr {
    type ParseOutput = FnCallExpr;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Sep(Separator::LeftParen)) 
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<FnCallExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => 
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new()))),
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(FnCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::SingleComma(span) => {
                sess.emit(strings::UnexpectedSingleComma).detail(span, strings::FnCallHere);
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new())));
            }
        }
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_fn_call_expr(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.base)?;
        v.visit_expr_list(&self.params)
    }
}

#[cfg(test)] #[test]
fn fn_call_parse() {
    use super::{make_node, make_exprs, make_lit};

    assert_eq!{ make_node!("()" as FnCallExpr),
        FnCallExpr::new_with_parse_result(Span::new(0, 1), make_exprs![])
    }

    assert_eq!{ make_node!("(\"hello\")" as FnCallExpr),
        FnCallExpr::new_with_parse_result(Span::new(0, 8), make_exprs![
            make_lit!(2: str, 1, 7),
        ])
    }
}

#[cfg(test)] #[test]
fn fn_call_errors() {
    use crate::diagnostics::make_errors;
    use super::{make_node};

    assert_eq!{ make_node!("(,)" as FnCallExpr, and messages), (
        FnCallExpr::new_with_parse_result(Span::new(0, 2), ExprList::new(Vec::new())),
        make_errors!(e: e.emit(strings::UnexpectedSingleComma).detail(Span::new(0, 2), strings::FnCallHere)),
    )}
}
