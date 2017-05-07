///! fff-lang
///!
///! syntax/ret_stmt for ReturnStatement

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::BinaryExpr;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct ReturnStatement {
    m_expr: Option<BinaryExpr>,
    m_all_strpos: StringPosition,
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

    pub fn new_unit(all_strpos: StringPosition) -> ReturnStatement {
        ReturnStatement{ m_expr: None, m_all_strpos: all_strpos }
    }
    pub fn new_expr(all_strpos: StringPosition, expr: BinaryExpr) -> ReturnStatement {
        ReturnStatement{ m_expr: Some(expr), m_all_strpos: all_strpos }
    }
}
impl ReturnStatement {

    pub fn get_expr(&self) -> Option<&BinaryExpr> { match self.m_expr { Some(ref expr) => Some(expr), None => None } }
    pub fn get_all_strpos(&self) -> StringPosition { self.m_all_strpos }

    // TODO: maybe should remove this temp for make codegen compile
    pub fn into_expr(self) -> Option<BinaryExpr> { self.m_expr }
}
impl ISyntaxItemGrammar for ReturnStatement {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool { tokens.nth(index) == &Token::Keyword(KeywordKind::Return) }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for ReturnStatement {
    fn is_first_finalx(sess: &ParseSession) -> bool { sess.tk == &Token::Keyword(KeywordKind::Return) }
}
impl ISyntaxItemParse for ReturnStatement {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ReturnStatement>, usize) {
        assert!(tokens.nth(index) == &Token::Keyword(KeywordKind::Return));

        if tokens.nth(index + 1) == &Token::Sep(SeperatorKind::SemiColon) {
            return (Some(ReturnStatement::new_unit(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2);
        }

        match BinaryExpr::parse(tokens, messages, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if tokens.nth(index + 1 + expr_len) == &Token::Sep(SeperatorKind::SemiColon) {
                    return (Some(ReturnStatement::new_expr(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1 + expr_len)), expr)), 2 + expr_len);
                } else {
                    return push_unexpect!(tokens, messages, "semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for ReturnStatement {
    
    fn parsex(sess: &mut ParseSession) -> ParseResult<ReturnStatement> {

        let return_strpos = sess.expect_keyword(KeywordKind::Return)?;

        if sess.tk == &Token::Sep(SeperatorKind::SemiColon) {
            return Ok(ReturnStatement::new_unit(StringPosition::merge(return_strpos, sess.pos)));
        }

        let expr = BinaryExpr::parsex(sess)?;
        let ending_strpos = sess.expect_sep(SeperatorKind::SemiColon)?;

        return Ok(ReturnStatement::new_expr(StringPosition::merge(return_strpos, ending_strpos), expr));
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use super::super::ISyntaxItemWithStr;
    use lexical::LitValue;
    use lexical::SeperatorKind;

    assert_eq!{ ReturnStatement::with_test_str("return;"), ReturnStatement::new_unit(make_strpos!(1, 1, 1, 7)) }
    assert_eq!{ ReturnStatement::with_test_str("return 1 + 1;"), 
        ReturnStatement::new_expr(
            make_strpos!(1, 1, 1, 13), 
            BinaryExpr::new_binary(
                BinaryExpr::new_lit(LitValue::from(1), make_strpos!(1, 8, 1, 8)),
                SeperatorKind::Add, make_strpos!(1, 10, 1, 10),
                BinaryExpr::new_lit(LitValue::from(1), make_strpos!(1, 12, 1, 12)),
            )
        )
    }
}