///! fff-lang
///!
///! syntax/ret_stmt for ReturnStatement

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::Expression;

#[derive(Eq, PartialEq)]
pub struct ReturnStatement {
    m_expr: Option<Expression>,
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
    pub fn new_expr(all_strpos: StringPosition, expr: Expression) -> ReturnStatement {
        ReturnStatement{ m_expr: Some(expr), m_all_strpos: all_strpos }
    }
}
impl ReturnStatement {

    pub fn get_expr(&self) -> Option<&Expression> { match self.m_expr { Some(ref expr) => Some(expr), None => None } }
    pub fn get_all_strpos(&self) -> StringPosition { self.m_all_strpos }

    // TODO: maybe should remove this temp for make codegen compile
    pub fn into_expr(self) -> Option<Expression> { self.m_expr }
}
impl ISyntaxItem for ReturnStatement {

    fn pos_all(&self) -> StringPosition { self.get_all_strpos() } 

    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_keyword(KeywordKind::Return)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<ReturnStatement>, usize) {

        if !tokens.nth(index).is_keyword(KeywordKind::Return) {
            unreachable!()
        }

        if tokens.nth(index + 1).is_seperator(SeperatorKind::SemiColon) {
            return (Some(ReturnStatement::new_unit(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2);
        }

        match Expression::parse(tokens, messages, index + 1) {
            (None, length) => return (None, length),
            (Some(expr), expr_len) => {
                if tokens.nth(index + 1 + expr_len).is_seperator(SeperatorKind::SemiColon) {
                    return (Some(ReturnStatement::new_expr(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1 + expr_len)), expr)), 2 + expr_len);
                } else {
                    return push_unexpect!(tokens, messages, "semicolon", index + expr_len + 1, expr_len + 1);
                }
            } 
        }
    }
}

#[cfg(test)] #[test]
fn ret_stmt_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ ReturnStatement::with_test_str("return;"), ReturnStatement::new_unit(make_strpos!(1, 1, 1, 7)) }
    assert_eq!{ ReturnStatement::with_test_str("return 1 + 1;"), ReturnStatement::new_unit(make_strpos!(1, 1, 1, 13)) }
    // return xxx yyy
}