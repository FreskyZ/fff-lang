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
impl ReturnStatement {

    pub fn new_unit(all_span: Span) -> ReturnStatement {
        ReturnStatement{ all_span, expr: None }
    }
    pub fn new_expr(all_span: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ all_span, expr: Some(expr) }
    }
}
impl Parser for ReturnStatement {
    type Output = ReturnStatement;

    fn matches(current: &Token) -> bool { 
        matches!(current, Token::Keyword(Keyword::Return))
    }

    fn parse(cx: &mut ParseContext) -> Result<ReturnStatement, Unexpected> {

        let starting_span = cx.expect_keyword(Keyword::Return)?;

        if let Some(semicolon_span) = cx.try_expect_sep(Separator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_cx and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span + semicolon_span))
        } else {
            let expr = cx.expect::<Expr>()?;
            let semicolon_span = cx.expect_sep(Separator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span + semicolon_span, expr))
        }
    }
}

impl Node for ReturnStatement {
    fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        v.visit_ret_stmt(self)
    }
    fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
        if let Some(expr) = &self.expr {
            v.visit_expr(expr)?;
        }
        Ok(Default::default())
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    case!{ "return;" as ReturnStatement, ReturnStatement::new_unit(Span::new(0, 6)) }
    case!{ "return 1 + 1;" as ReturnStatement, 
        ReturnStatement::new_expr(
            Span::new(0, 12),
            make_expr!(binary 7:11 Add 9:9
                make_expr!(i32 1 7:7),
                make_expr!(i32 1 11:11))
        )
    }
}
