///! fff-lang
///! 
///! syntax/expr_stmt
///! expr_stmt = expr { assign_ops expr } ';'

use super::prelude::*;
use super::{Statement, Expr};

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct SimpleExprStatement {
    pub expr: Expr, 
    pub all_span: Span,  // this span = expr.all_span + semicolon_span
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

    fn parse(cx: &mut ParseContext) -> ParseResult<Self::ParseOutput> { 
        cx.expect_node::<AssignExprStatement>() 
    }
    
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_expr_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.expr)
    }
}

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct AssignExprStatement {
    pub left_expr: Expr,
    pub right_expr: Expr,
    pub assign_op: Separator,
    pub assign_op_span: Span,
    pub all_span: Span,
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

    fn parse(cx: &mut ParseContext) -> ParseResult<Statement> {

        let left_expr = cx.expect_node::<Expr>()?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span + semicolon_span, left_expr)))
        } else if let Some((assign_op, assign_op_span)) = cx.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = cx.expect_node::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span + semicolon_span, assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            cx.push_unexpect("assign operators, semicolon")
        }
    }
    
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_assign_expr_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.left_expr)?;
        v.visit_expr(&self.right_expr)
    }
}

#[cfg(test)] #[test]
fn expr_stmt_parse() {
    use super::{SimpleName, BinaryExpr, FnCallExpr};

    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ "writeln(\"helloworld\");" as AssignExprStatement,
        Statement::SimpleExpr(SimpleExprStatement::new(Span::new(0, 21),
            FnCallExpr::new(
                SimpleName::new(2, Span::new(0, 6)),
                Span::new(7, 20), make_exprs![
                    make_lit!(3: str, 8, 19)
                ]
            )
        ))
    }

    //                      012345678901
    case!{ "1 + 1 <<= 2;" as AssignExprStatement,  // to show I have 3 char Separator available
        Statement::AssignExpr(AssignExprStatement::new(Span::new(0, 11),
            Separator::LtLtEq, Span::new(6, 8),
            BinaryExpr::new(
                make_lit!(1, 0, 0),
                Separator::Add, Span::new(2, 2),
                make_lit!(1, 4, 4),
            ),
            make_lit!(2, 10, 10)
        ))
    }
}