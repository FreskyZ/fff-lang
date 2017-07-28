///! fff-lang
///! 
///! syntax/expr_stmt
///! expr_stmt = expr { assign_ops expr } ';'

use std::fmt;

use codemap::Span;
use lexical::Seperator;
use lexical::SeperatorCategory;

use super::Statement;
use super::super::Expr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SimpleExprStatement {
    pub expr: Expr, 
    pub all_span: Span,  // this span = expr.all_span.merge(&semicolon_span)
}
impl ISyntaxFormat for SimpleExprStatement {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("expr-stmt simple").space().span(self.all_span).endl()
            .apply1(&self.expr)
            .finish()
    }
}
impl fmt::Debug for SimpleExprStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl SimpleExprStatement {
    pub fn new<T: Into<Expr>>(all_span: Span, expr: T) -> SimpleExprStatement { 
        SimpleExprStatement{ all_span, expr: expr.into() } 
    }
}
// dispatch them to convenience statement define macro
impl ISyntaxItemGrammar for SimpleExprStatement {
    fn is_first_final(sess: &ParseSession) -> bool { AssignExprStatement::is_first_final(sess) }
}
impl ISyntaxItemParse for SimpleExprStatement {
    type Target = <AssignExprStatement as ISyntaxItemParse>::Target;
    fn parse(sess: &mut ParseSession) -> ParseResult<Self::Target> { AssignExprStatement::parse(sess) }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Seperator,
    pub assign_op_span: Span,
    pub all_span: Span,
}
impl ISyntaxFormat for AssignExprStatement {
    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("expr-stmt assign").space().span(self.all_span).endl()
            .set_prefix_text("left-is").apply1(&self.left_expr).unset_prefix_text().endl()
            .indent1().lit("\"").debug(&self.assign_op).lit("\"").space().span(self.assign_op_span).endl()
            .set_prefix_text("right-is").apply1(&self.right_expr)
            .finish()
    }
}
impl fmt::Debug for AssignExprStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl AssignExprStatement {
    
    pub fn new<T1: Into<Expr>, T2: Into<Expr>>(all_span: Span, 
        assign_op: Seperator, assign_op_span: Span, left_expr: T1, right_expr: T2) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: left_expr.into(),
            right_expr: right_expr.into(),
            all_span,
            assign_op, assign_op_span,
        }
    }
}

impl ISyntaxItemGrammar for AssignExprStatement {
    fn is_first_final(sess: &ParseSession) -> bool { Expr::is_first_final(sess) }
}
impl ISyntaxItemParse for AssignExprStatement {
    type Target = Statement;

    fn parse(sess: &mut ParseSession) -> ParseResult<Statement> {

        let left_expr = Expr::parse(sess)?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = sess.try_expect_sep(Seperator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span.merge(&semicolon_span), left_expr)))
        } else if let Some((assign_op, assign_op_span)) = sess.try_expect_sep_cat(SeperatorCategory::Assign) {
            let right_expr = Expr::parse(sess)?;
            let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span.merge(&semicolon_span), assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            sess.push_unexpect("assign operators, semicolon")
        }
    }
}

#[cfg(test)] #[test]
fn expr_stmt_parse() {
    use codemap::SymbolCollection;
    use lexical::LitValue;
    use super::super::LitExpr;
    use super::super::SimpleName;
    use super::super::BinaryExpr;
    use super::super::FnCallExpr;
    use super::super::ExprList;
    use super::super::TestInput;
    use super::super::WithTestInput;

    //                                                0         1          2
    //                                                012345678 90123456789 012
    TestInput::new("writeln(\"helloworld\");")
        .set_syms(make_symbols!["writeln", "helloworld"]) 
        .apply::<AssignExprStatement, _>()
        .expect_no_message()
        .expect_result(Statement::SimpleExpr(SimpleExprStatement::new(make_span!(0, 21),
            FnCallExpr::new(
                SimpleName::new(make_id!(1), make_span!(0, 6)),
                make_span!(7, 20), make_exprs![
                    LitExpr::new(LitValue::new_str_lit(make_id!(2)), make_span!(8, 19))
                ]
            )
        )))
    .finish();

    //                                              012345678901
    assert_eq!{ AssignExprStatement::with_test_str("1 + 1 <<= 2;"),  // to show I have 3 char seperator available
        Statement::AssignExpr(AssignExprStatement::new(make_span!(0, 11),
            Seperator::ShiftLeftAssign, make_span!(6, 8),
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1), make_span!(0, 0)),
                Seperator::Add, make_span!(2, 2),
                LitExpr::new(LitValue::from(1), make_span!(4, 4)),
            ),
            LitExpr::new(LitValue::from(2), make_span!(10, 10))
        ))
    }
}