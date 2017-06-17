///! fff-lang
///!
///! syntax/ret_stmt for ReturnStatement

use std::fmt;

use codemap::Span;
use lexical::Token;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::Expr;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ReturnStatement {
    m_expr: Option<Expr>,
    m_all_strpos: Span,
}
impl ISyntaxItemFormat for ReturnStatement {
    fn format(&self, indent: u32) -> String {
        format!("{}ReturnStmt <{:?}>\n{:?}", 
            ReturnStatement::indent_str(indent), self.m_all_strpos, self.m_expr)
    }
}
impl fmt::Debug for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}
impl ReturnStatement {

    pub fn new_unit(all_strpos: Span) -> ReturnStatement {
        ReturnStatement{ m_expr: None, m_all_strpos: all_strpos }
    }
    pub fn new_expr(all_strpos: Span, expr: Expr) -> ReturnStatement {
        ReturnStatement{ m_expr: Some(expr), m_all_strpos: all_strpos }
    }
}
impl ReturnStatement {

    pub fn get_expr(&self) -> Option<&Expr> { match self.m_expr { Some(ref expr) => Some(expr), None => None } }
    pub fn get_all_strpos(&self) -> Span { self.m_all_strpos }

    // TODO: maybe should remove this temp for make codegen compile
    pub fn into_expr(self) -> Option<Expr> { self.m_expr }
}
impl ISyntaxItemGrammar for ReturnStatement {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Return) }
}
impl ISyntaxItemParse for ReturnStatement {
    type Target = ReturnStatement;

    fn parse(sess: &mut ParseSession) -> ParseResult<ReturnStatement> {

        let return_strpos = sess.expect_keyword(KeywordKind::Return)?;

        if sess.tk == &Token::Sep(SeperatorKind::SemiColon) {
            return Ok(ReturnStatement::new_unit(return_strpos.merge(&sess.pos)));
        }

        let expr = Expr::parse(sess)?;
        let ending_strpos = sess.expect_sep(SeperatorKind::SemiColon)?;

        return Ok(ReturnStatement::new_expr(return_strpos.merge(&ending_strpos), expr));
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use lexical::LitValue;
    use lexical::SeperatorKind;
    use super::super::LitExpr;
    use super::super::BinaryExpr;
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ ReturnStatement::with_test_str("return;"), ReturnStatement::new_unit(make_span!(0, 6)) }
    assert_eq!{ ReturnStatement::with_test_str("return 1 + 1;"), 
        ReturnStatement::new_expr(
            make_span!(0, 12), 
            Expr::Binary(BinaryExpr::new(
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(7, 7))),
                SeperatorKind::Add, make_span!(9, 9),
                Expr::Lit(LitExpr::new(LitValue::from(1), make_span!(11, 11))),
            ))
        )
    }
}