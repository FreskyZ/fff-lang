///! fff-lang
///!
///! syntax/fn_call_expr
///! fn_call_expr = expr '(' [ expr_list ] ')'

use std::fmt;

use codemap::Span;
use message::Message;
use lexical::Token;
use lexical::SeperatorKind;

use super::Expr;
use super::ExprList;
use super::ExprListParseResult;

use super::super::error_strings;
use super::super::ParseSession;
use super::super::ParseResult;
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
    fn format(&self, indent: u32) -> String {
        format!("{}FnCall <{:?}>\n{}\n{}paren <{:?}>\n{}", 
            FnCallExpr::indent_str(indent), self.all_span,
            self.base.format(indent + 1),
            FnCallExpr::indent_str(indent + 1), self.paren_span, 
            if self.params.items.len() == 0 { format!("{}(empty)", FnCallExpr::indent_str(indent + 1)) } else { self.params.format(indent + 1) })
    }
}
impl fmt::Debug for FnCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl From<FnCallExpr> for Expr {
    fn from(fn_call_expr: FnCallExpr) -> Expr { Expr::FnCall(fn_call_expr) }
}
impl FnCallExpr {

    pub fn new<T1: Into<Expr>, T2: Into<ExprList>>(base: T1, paren_span: Span, params: T2) -> FnCallExpr {
        let base = base.into();
        FnCallExpr{
            all_span: base.get_all_span().merge(&paren_span),
            base: Box::new(base),
            params: params.into(),
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
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::LeftParenthenes) }
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
    use super::super::ISyntaxItemWithStr;
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
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ FnCallExpr::with_test_str_ret_messages("(,)"), (
        Some(FnCallExpr::new_with_parse_result(make_span!(0, 2), ExprList::new(vec![]))),
        make_messages![
            Message::new_by_str(error_strings::UnexpectedSingleComma, vec![(make_span!(0, 2), error_strings::FnCallHere)])
        ]
    )}
}