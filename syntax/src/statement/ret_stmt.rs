///! fff-lang
///!
///! syntax/ret_stmt
///! ret_stmt = 'return' [ expr ] ';'

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::Seperator;
use lexical::Keyword;

use super::super::Expr;
use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ReturnStatement {
    pub expr: Option<Expr>,
    pub all_span: Span,
}
impl ISyntaxItemFormat for ReturnStatement {
    fn format(&self, indent: u32) -> String {
        match self.expr {
            Some(ref expr) => format!("{}ReturnStmt some <{:?}>\n{}", ReturnStatement::indent_str(indent), self.all_span, expr.format(indent + 1)),
            None => format!("{}ReturnStmt unit <{:?}>", ReturnStatement::indent_str(indent), self.all_span),
        }
    }
}
impl fmt::Debug for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl ReturnStatement {

    pub fn new_unit(all_span: Span) -> ReturnStatement {
        ReturnStatement{ all_span, expr: None }
    }
    pub fn new_expr(all_span: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ all_span, expr: Some(expr) }
    }
}
impl ISyntaxItemGrammar for ReturnStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(Keyword::Return) }
}
impl ISyntaxItemParse for ReturnStatement {
    type Target = ReturnStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ReturnStatement> {

        let return_strpos = sess.expect_keyword(Keyword::Return)?;

        if sess.tk == &Token::Sep(Seperator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            let ending_span = sess.pos;
            sess.move_next();
            return Ok(ReturnStatement::new_unit(return_strpos.merge(&ending_span)));
        }

        let expr = Expr::parse(sess)?;
        let ending_strpos = sess.expect_sep(Seperator::SemiColon)?;

        return Ok(ReturnStatement::new_expr(return_strpos.merge(&ending_strpos), expr));
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use lexical::LitValue;
    use lexical::Seperator;
    use super::super::LitExpr;
    use super::super::BinaryExpr;
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ ReturnStatement::with_test_str("return;"), ReturnStatement::new_unit(make_span!(0, 6)) }
    assert_eq!{ ReturnStatement::with_test_str("return 1 + 1;"), 
        ReturnStatement::new_expr(
            make_span!(0, 12), 
            Expr::Binary(BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(7, 7))),
                Seperator::Add, make_span!(9, 9),
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(11, 11))),
            ))
        )
    }
}