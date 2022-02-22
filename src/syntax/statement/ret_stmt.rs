///! fff-lang
///!
///! syntax/ret_stmt
///! ret_stmt = 'return' [ expr ] ';'

use std::fmt;
use crate::codemap::Span;
use crate::lexical::Token;
use crate::lexical::Seperator;
use crate::lexical::Keyword;
use super::super::Expr;
use super::super::Formatter;
use super::super::ParseResult;
use super::super::ParseSession;
use super::super::ISyntaxParse;
use super::super::ISyntaxFormat;
use super::super::ISyntaxGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
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
impl fmt::Debug for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
}
impl ReturnStatement {

    pub fn new_unit(all_span: Span) -> ReturnStatement {
        ReturnStatement{ all_span, expr: None }
    }
    pub fn new_expr(all_span: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ all_span, expr: Some(expr) }
    }
}
impl ISyntaxGrammar for ReturnStatement {
    fn matches_first(tokens: &[&Token]) -> bool { tokens[0] == &Token::Keyword(Keyword::Return) }
}
impl ISyntaxParse for ReturnStatement {
    type Output = ReturnStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ReturnStatement> {

        let starting_span = sess.expect_keyword(Keyword::Return)?;

        if let Some(semicolon_span) = sess.try_expect_sep(Seperator::SemiColon) {
            // 17/6/17: you forgot move_next here!
            // but I have never write some test cases like following something after ret stmt
            // so the bug is not propagated to be discovered
            // 17/7/28: now new features added to parse_sess and move_next is to be removed, no current position management bug any more!
            Ok(ReturnStatement::new_unit(starting_span.merge(&semicolon_span)))
        } else {
            let expr = Expr::parse(sess)?;
            let semicolon_span = sess.expect_sep(Seperator::SemiColon)?;
            Ok(ReturnStatement::new_expr(starting_span.merge(&semicolon_span), expr))
        }
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use crate::lexical::LitValue;
    use crate::lexical::Seperator;
    use super::super::LitExpr;
    use super::super::BinaryExpr;
    use super::super::WithTestInput;

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