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
        format!("{}FnCall <{:?}>\n{}\n{}Paren <{:?}>\n{}", 
            FnCallExpr::indent_str(indent), self.all_span,
            self.base.format(indent + 1),
            FnCallExpr::indent_str(indent + 1), self.paren_span, 
            self.params.format(indent + 1))
    }
}
impl fmt::Debug for FnCallExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl FnCallExpr {

    pub fn new<T: Into<Expr>>(base: T, paren_span: Span, params: ExprList) -> FnCallExpr {
        let base = base.into();
        FnCallExpr{
            all_span: base.get_all_span().merge(&paren_span),
            base: Box::new(base),
            paren_span, params
        }
    }

    fn new_with_parse_result(params: ExprList, paren_span: Span) -> FnCallExpr {
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
                return Ok(FnCallExpr::new_with_parse_result(ExprList::new(Vec::new()), span)),
            ExprListParseResult::Normal(span, expr_list) | ExprListParseResult::EndWithComma(span, expr_list) => 
                return Ok(FnCallExpr::new_with_parse_result(expr_list, span)),
            ExprListParseResult::SingleComma(span) => {
                sess.push_message(Message::new_by_str("single comma in function call", vec![(span, "function call here")]));
                return Ok(FnCallExpr::new_with_parse_result(ExprList::new(Vec::new()), span));
            }
        }
    }
}