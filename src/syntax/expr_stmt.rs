///! fff-lang
///! 
///! syntax/expr_stmt
///! expr_stmt = expr { assign_ops expr } ';'

use super::prelude::*;
use super::{Statement, Expr};

#[cfg_attr(test, derive(PartialEq))]
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
impl Node for SimpleExprStatement {
    type ParseOutput = <AssignExprStatement as Node>::ParseOutput;

    fn matches(current: &Token) -> bool { 
        AssignExprStatement::matches(current)
    }
    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
        AssignExprStatement::matches3(current, peek, peek2)
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Self::ParseOutput> { 
        AssignExprStatement::parse(sess) 
    }
}

#[cfg_attr(test, derive(PartialEq))]
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

impl Node for AssignExprStatement {
    type ParseOutput = Statement;

    fn matches(current: &Token) -> bool { 
        Expr::matches(current)
    }
    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
        Expr::matches3(current, peek, peek2)
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<Statement> {

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
    use super::{make_node, make_exprs, LitExpr, LitValue, SimpleName, BinaryExpr, FnCallExpr};

    //                      0          1          2
    //                      012345678 90123456789 012
    assert_eq!{ make_node!("writeln(\"helloworld\");" as AssignExprStatement, [Span::new(0, 6)], ["helloworld"]),
        Statement::SimpleExpr(SimpleExprStatement::new(Span::new(0, 21),
            FnCallExpr::new(
                SimpleName::new(1, Span::new(0, 6)),
                Span::new(7, 20), make_exprs![
                    LitExpr::new(3u32, Span::new(8, 19))
                ]
            )
        ))
    }

    //                      012345678901
    assert_eq!{ make_node!("1 + 1 <<= 2;" as AssignExprStatement),  // to show I have 3 char Separator available
        Statement::AssignExpr(AssignExprStatement::new(Span::new(0, 11),
            Separator::LtLtEq, Span::new(6, 8),
            BinaryExpr::new(
                LitExpr::new(LitValue::from(1i32), Span::new(0, 0)),
                Separator::Add, Span::new(2, 2),
                LitExpr::new(LitValue::from(1i32), Span::new(4, 4)),
            ),
            LitExpr::new(LitValue::from(2i32), Span::new(10, 10))
        ))
    }
}