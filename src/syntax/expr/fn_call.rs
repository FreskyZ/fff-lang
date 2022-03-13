///! fff-lang
///!
///! syntax/fn_call_expr
///! fn_call_expr = expr '(' [ expr_list ] ')'

use crate::syntax::prelude::*;
use super::{Expr, ExprList, ExprListParseResult};

#[cfg_attr(test, derive(PartialEq))]
pub struct FnCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub paren_span: Span,
    pub all_span: Span,
}
impl ISyntaxFormat for FnCallExpr {
    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("fn-call").space().span(self.all_span).endl()
            .set_prefix_text("base-is").apply1(self.base.as_ref()).unset_prefix_text().endl()
            .indent1().lit("parenthenes").space().span(self.paren_span).endl();
        (if self.params.items.len() == 0 { f.indent1().lit("no-argument") } else { f.apply1(&self.params) }).finish()
    }
}
impl fmt::Debug for FnCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
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
impl ISyntaxGrammar for FnCallExpr {
    fn matches_first(tokens: [&Token; 3]) -> bool { matches!(tokens[0], &Token::Sep(Separator::LeftParen)) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for FnCallExpr where F: FileSystem {
    type Output = FnCallExpr;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<FnCallExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => 
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new()))),
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(FnCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str(strings::UnexpectedSingleComma, vec![(span, strings::FnCallHere)]));
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new())));
            }
        }
    }
}

#[cfg(test)] #[test]
fn fn_call_parse() {
    use super::super::{make_node};
    use super::{LitExpr};

    assert_eq!{ make_node!("()" as FnCallExpr),
        FnCallExpr::new_with_parse_result(Span::new(0, 1), ExprList::new(vec![]))
    }

    assert_eq!{ make_node!("(\"hello\")" as FnCallExpr),
        FnCallExpr::new_with_parse_result(Span::new(0, 8), 
            ExprList::new(vec![Expr::Lit(LitExpr::new(2u32, Span::new(1, 7)))])
        )
    }
}

#[cfg(test)] #[test]
fn fn_call_errors() {
    use super::super::{make_node};

    assert_eq!{ make_node!("(,)" as FnCallExpr, and messages),
        (FnCallExpr::new_with_parse_result(Span::new(0, 2), ExprList::new(Vec::new())), make_messages![
            Message::new_by_str(strings::UnexpectedSingleComma, vec![(Span::new(0, 2), strings::FnCallHere)])
        ])
    }
}
