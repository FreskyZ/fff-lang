///! fff-lang
///!
///! syntax/ret_stmt
///! ret_stmt = 'return' [ expr ] ';'

use super::prelude::*;
use super::Expr;

#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
    pub all_span: Span,
}
impl ISyntaxFormat for ReturnStatement {
    fn format(&self, f: Formatter) -> String {
        match self.expr {
            Some(ref expr) => 
                f.indent().header_text_or("return-stmt").space().span(self.all_span).endl()
                    .set_prefix_text("ret-val-is").apply1(expr).unset_prefix_text().finish(),
            None => f.indent().header_text_or("return-stmt").space().span(self.all_span).finish(),
        }
    }
}
impl ReturnStatement {

    pub fn new_unit(all_span: Span) -> ReturnStatement {
        ReturnStatement{ all_span, expr: None }
    }
    pub fn new_expr(all_span: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ all_span, expr: Some(expr) }
    }
}
impl Node for ReturnStatement {
    type ParseOutput = ReturnStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Return))
    }

    fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<ReturnStatement> {

        let starting_span = sess.expect_keyword(Keyword::Return)?;

        if let Some(semicolon_span) = sess.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_sess and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span + semicolon_span))
        } else {
            let expr = Expr::parse(sess)?;
            let semicolon_span = sess.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span + semicolon_span, expr))
        }
    }

    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_ret_stmt(self)
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use super::{make_node, make_lit, BinaryExpr};

    assert_eq!{ make_node!("return;" as ReturnStatement), ReturnStatement::new_unit(Span::new(0, 6)) }
    assert_eq!{ make_node!("return 1 + 1;" as ReturnStatement), 
        ReturnStatement::new_expr(
            Span::new(0, 12), 
            Expr::Binary(BinaryExpr::new(
                Expr::Lit(make_lit!(1, 7, 7)),
                Separator::Add, Span::new(9, 9),
                Expr::Lit(make_lit!(1, 11, 11)),
            ))
        )
    }
}