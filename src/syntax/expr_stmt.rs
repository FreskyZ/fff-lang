///! fff-lang
///! 
///! syntax/expr_stmt
///! expr_stmt = expr { assign_ops expr } ';'

use super::prelude::*;

// dispatch them to convenience statement define macro
impl Parser for SimpleExprStatement {
    type Output = <AssignExprStatement as Parser>::Output;

    fn matches(current: &Token) -> bool { 
        AssignExprStatement::matches(current)
    }
    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
        AssignExprStatement::matches3(current, peek, peek2)
    }

    fn parse(cx: &mut ParseContext) -> Result<Self::Output, Unexpected> { 
        cx.expect::<AssignExprStatement>() 
    }
}

impl Node for SimpleExprStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_simple_expr_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_expr(&self.expr)
    }
}


impl Parser for AssignExprStatement {
    type Output = Statement;

    fn matches(current: &Token) -> bool { 
        Expr::matches(current)
    }
    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
        Expr::matches3(current, peek, peek2)
    }

    fn parse(cx: &mut ParseContext) -> Result<Statement, Unexpected> {

        let left_expr = cx.expect::<Expr>()?;
        let starting_span = left_expr.get_all_span();

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            Ok(Statement::SimpleExpr(SimpleExprStatement::new(starting_span + semicolon_span, left_expr)))
        } else if let Some((assign_op, assign_op_span)) = cx.try_expect_sep_kind(SeparatorKind::Assign) {
            let right_expr = cx.expect::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(Statement::AssignExpr(
                AssignExprStatement::new(starting_span + semicolon_span, assign_op, assign_op_span, left_expr, right_expr)))
        } else {
            cx.push_unexpect("assign operators, semicolon")
        }
    }
}

impl Node for AssignExprStatement {
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
    //                      0          1          2
    //                      012345678 90123456789 012
    case!{ "writeln(\"helloworld\");" as AssignExprStatement,
        Statement::SimpleExpr(SimpleExprStatement::new(Span::new(0, 21),
            make_expr!(fn 0:20 paren 7:20
                make_name!(simple 0:6 #2),
                make_expr!(str #3 8:19))
        ))
    }

    //                      012345678901
    case!{ "1 + 1 <<= 2;" as AssignExprStatement,  // to show I have 3 char Separator available
        Statement::AssignExpr(AssignExprStatement::new(Span::new(0, 11),
            Separator::LtLtEq, Span::new(6, 8),
            make_expr!(binary 0:4 Add 2:2
                make_expr!(i32 1 0:0),
                make_expr!(i32 1 4:4)),
            make_expr!(i32 2 10:10)
        ))
    }
}