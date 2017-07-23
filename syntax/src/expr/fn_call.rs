///! fff-lang
///!
///! syntax/fn_call_expr
///! fn_call_expr = expr '(' [ expr_list ] ')'

use std::fmt;

use codemap::Span;
use message::Message;
use lexical::Token;
use lexical::Seperator;

use super::Expr;
use super::ExprList;
use super::ExprListParseResult;

use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::error_strings;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct FnCallExpr {
    pub base: Box<Expr>,
    pub params: ExprList,
    pub paren_span: Span,
    pub all_span: Span,
}
impl ISyntaxItemFormat for FnCallExpr {
    fn format(&self, f: Formatter) -> String {
        format!("{}FnCall <{}>\n{}\n{}paren <{}>\n{}", 
            f.indent(), f.span(self.all_span),
            f.apply1(self.base.as_ref()),
            f.indent1(), f.span(self.paren_span),
            if self.params.items.len() == 0 { format!("{}(empty)", f.indent1()) } else { f.apply1(&self.params) })
    }
}
impl fmt::Debug for FnCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::default())) }
}
impl From<FnCallExpr> for Expr {
    fn from(fn_call_expr: FnCallExpr) -> Expr { Expr::FnCall(fn_call_expr) }
}
impl FnCallExpr {

    pub fn new<T: Into<Expr>>(base: T, paren_span: Span, params: ExprList) -> FnCallExpr {
        let base = base.into();
        FnCallExpr{
            all_span: base.get_all_span().merge(&paren_span),
            base: Box::new(base),
            params,
            paren_span,
        }
    }

    fn new_with_parse_result(paren_span: Span, params: ExprList) -> FnCallExpr {
        FnCallExpr{
            all_span: Span::default(), 
            base: Box::new(Expr::default()),
            paren_span, params
        }
    }
}
impl ISyntaxItemGrammar for FnCallExpr {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(Seperator::LeftParenthenes) }
}
impl ISyntaxItemParse for FnCallExpr {
    type Target = FnCallExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<FnCallExpr> {

        match ExprList::parse(sess)? {
            ExprListParseResult::Empty(span) => 
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new()))),
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(FnCallExpr::new_with_parse_result(span, expr_list)),
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(span, error_strings::FnCallHere)]));
                return Ok(FnCallExpr::new_with_parse_result(span, ExprList::new(Vec::new())));
            }
        }
    }
}

#[cfg(test)] #[test]
fn fn_call_parse() {
    use lexical::LitValue;
    use super::super::WithTestInput;
    use super::LitExpr;

    assert_eq!{ FnCallExpr::with_test_str("()"),
        FnCallExpr::new_with_parse_result(make_span!(0, 1), ExprList::new(vec![]))
    }

    assert_eq!{ FnCallExpr::with_test_str("(\"hello\")"),
        FnCallExpr::new_with_parse_result(make_span!(0, 8), 
            ExprList::new(vec![Expr::Lit(LitExpr::new(LitValue::new_str_lit(make_id!(1)), make_span!(1, 7)))])
        )
    }
}

#[cfg(test)] #[test]
fn fn_call_errors() {
    use message::MessageCollection;
    use super::super::TestInput;

    TestInput::new("(,)")
        .apply::<FnCallExpr, _>()
        .expect_result(FnCallExpr::new_with_parse_result(make_span!(0, 2), ExprList::new(vec![])))
        .expect_messages(make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(0, 2), error_strings::FnCallHere)])
        ])
    .finish();
}