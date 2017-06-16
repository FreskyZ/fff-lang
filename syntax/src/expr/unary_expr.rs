///! fff-lang
///!
///! syntax/unary_expr
///! unary_expr = { unary_operator } postfix_expr

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::PostfixExpr;
use super::Expr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct UnaryExpr {
    pub base: Box<Expr>, 
    pub operator: SeperatorKind, 
    pub operator_span: Span,
    pub all_span: Span,
}
impl ISyntaxItemFormat for UnaryExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}UnaryExpr <{:?}>\n{}{} <{:?}>\n{}", 
            UnaryExpr::indent_str(indent), self.all_span,
            UnaryExpr::indent_str(indent + 1), self.operator, self.operator_span,
            self.base.format(indent + 1),
        )
    }
}
impl fmt::Debug for UnaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl UnaryExpr {

    pub fn new(operator: SeperatorKind, operator_span: Span, base: Expr) -> UnaryExpr {
        UnaryExpr{
            all_span: operator_span.merge(&base.get_all_span()),
            base: Box::new(base),
            operator, operator_span,
        }
    }
}
impl ISyntaxItemGrammar for UnaryExpr {
    fn is_first_final(sess: &ParseSession) -> bool { 
        match sess.tk {
            &Token::Sep(ref sep) if sep.is_category(SeperatorCategory::Unary) => true,
            _ => false,
        }
    }
}
impl ISyntaxItemParse for UnaryExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        
        let mut op_spans = Vec::new();
        loop {
            match (sess.tk, sess.pos) {
                (&Token::Sep(operator), operator_strpos) if operator.is_category(SeperatorCategory::Unary) => {
                    sess.move_next();
                    op_spans.push((operator, operator_strpos));
                }
                _ => {
                    let base = Expr::Postfix(PostfixExpr::parse(sess)?);
                    return Ok(op_spans.into_iter().rev().fold(base, |base, (op, span)| { Expr::Unary(UnaryExpr::new(op, span, base)) }));
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn unary_expr_parse() {
    use lexical::LitValue;
    use super::LitExpr;
    use super::PrimaryExpr;
    use super::super::ISyntaxItemWithStr;
    
    assert_eq!{ UnaryExpr::with_test_str("1"), 
        Expr::new_primary(PrimaryExpr::Lit(LitExpr::new(LitValue::from(1), make_span!(0, 0)))) 
    }

    assert_eq!{ UnaryExpr::with_test_str("!~!1"),
        Expr::Unary(UnaryExpr::new(
            SeperatorKind::LogicalNot, make_span!(0, 0),
            Expr::Unary(UnaryExpr::new(
                SeperatorKind::BitNot, make_span!(1, 1),            
                Expr::Unary(UnaryExpr::new(
                    SeperatorKind::LogicalNot, make_span!(2, 2),
                    Expr::new_primary(PrimaryExpr::Lit(LitExpr::new(LitValue::from(1), make_span!(3, 3)))),
                ))
            ))
        ))
    }
}