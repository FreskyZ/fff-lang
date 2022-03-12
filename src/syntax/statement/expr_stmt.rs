///! fff-lang
///! 
///! syntax/expr_stmt
///! expr_stmt = expr { assign_ops expr } ';'

use crate::syntax::prelude::*;
use super::{Statement, super::Expr};

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct SimpleExprStatement {
    pub expr: Expr, 
    pub all_span: Span,  // this span = expr.all_span + semicolon_span
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
impl ISyntaxGrammar for SimpleExprStatement {
    fn matches_first(tokens: [&Token; 3]) -> bool { AssignExprStatement::matches_first(tokens) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for SimpleExprStatement where F: FileSystem {
    type Output = <AssignExprStatement as ISyntaxParse<'ecx, 'scx, F>>::Output;
    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Self::Output> { AssignExprStatement::parse(sess) }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Separator,
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
        assign_op: Separator, assign_op_span: Span, left_expr: T1, right_expr: T2) -> AssignExprStatement {
        AssignExprStatement{
            left_expr: left_expr.into(),
            right_expr: right_expr.into(),
            all_span,
            assign_op, assign_op_span,
        }
    }
}

impl ISyntaxGrammar for AssignExprStatement {
    fn matches_first(tokens: [&Token; 3]) -> bool { Expr::matches_first(tokens) }
}
impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for AssignExprStatement where F: FileSystem {
    type Output = Statement;

    fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<Statement> {

        let left_expr = Expr::parse(sess)?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = sess.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span + semicolon_span, left_expr)))
        } else if let Some((assign_op, assign_op_span)) = sess.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = Expr::parse(sess)?;
            let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span + semicolon_span, assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            sess.push_unexpect("assign operators, semicolon")
        }
    }
}

#[cfg(test)] #[test]
fn expr_stmt_parse() {
    use crate::source::SymbolCollection;
    use crate::lexical::LitValue;
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
        .expect_result(Statement::SimpleExpr(SimpleExprStatement::new(Span::new(0, 21),
            FnCallExpr::new(
                SimpleName::new(1, Span::new(0, 6)),
                Span::new(7, 20), make_exprs![
                    LitExpr::new(make_lit!(str, 2), Span::new(8, 19))
                ]
            )
        )))
    .finish();

    //                                              012345678901
    assert_eq!{ make_node!("1 + 1 <<= 2;"),  // to show I have 3 char Separator available
        Statement::AssignExpr(AssignExprStatement::new(Span::new(0, 11),
            Separator::LtLtAssign, Span::new(6, 8),
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1i32), Span::new(0, 0)),
                Separator::Add, Span::new(2, 2),
                LitExpr::new(LitValue::from(1i32), Span::new(4, 4)),
            ),
            LitExpr::new(LitValue::from(2i32), Span::new(10, 10))
        ))
    }
}