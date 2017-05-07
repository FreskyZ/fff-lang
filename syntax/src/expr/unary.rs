///! fff-lang
///!
///! syntax/unary_expr
///! UnaryExpr = PostfixExpr | UnaryOperator UnaryExpr

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::Token;
use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

#[cfg(feature = "parse_sess")] use super::super::ParseSession;
#[cfg(feature = "parse_sess")] use super::super::ParseResult;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use super::super::ISyntaxItemGrammarX;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::postfix::PostfixExpr;
use super::primary::PrimaryExpr;

#[cfg_attr(test, derive(Eq, PartialEq))]
struct ActualUnaryExpr {
    right: UnaryExpr, 
    operator: SeperatorKind, 
    operator_strpos: StringPosition,
    all_strpos: StringPosition,
}
#[cfg_attr(test, derive(Eq, PartialEq))]
enum UnaryExprImpl {
    Postfix(PostfixExpr),
    Unary(ActualUnaryExpr),
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct UnaryExpr(Box<UnaryExprImpl>);

impl ISyntaxItemFormat for UnaryExpr {
    fn format(&self, indent: u32) -> String {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(ref postfix_expr) => postfix_expr.format(indent),
            &UnaryExprImpl::Unary(ActualUnaryExpr{ ref right, ref operator, ref operator_strpos, ref all_strpos }) => {
                format!("{}UnaryExpr <{:?}>\n{}{} <{:?}>\n{}", 
                    UnaryExpr::indent_str(indent), all_strpos,
                    UnaryExpr::indent_str(indent + 1), operator, operator_strpos,
                    right.format(indent + 1),
                )
            }
        }
    }
}
impl fmt::Debug for UnaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl UnaryExpr { // New

    pub fn new_unary(operator: SeperatorKind, operator_strpos: StringPosition, right: UnaryExpr) -> UnaryExpr {
        
        let all_strpos =  StringPosition::merge(operator_strpos, right.get_all_strpos());
        UnaryExpr(Box::new(UnaryExprImpl::Unary(ActualUnaryExpr{
            right: right,
            operator: operator,
            operator_strpos: operator_strpos,
            all_strpos: all_strpos
        })))
    }
    pub fn new_postfix(postfix_expr: PostfixExpr) -> UnaryExpr {
        UnaryExpr(Box::new(UnaryExprImpl::Postfix(postfix_expr)))
    }
    pub fn new_primary(primary_expr: PrimaryExpr) -> UnaryExpr {
        UnaryExpr(Box::new(UnaryExprImpl::Postfix(PostfixExpr::new_primary(primary_expr))))
    }
}
impl UnaryExpr { // Get

    pub fn is_postfix(&self) -> bool {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(_) => true,
            &UnaryExprImpl::Unary(_) => false,
        }
    }
    pub fn is_unary(&self) -> bool {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(_) => false,
            &UnaryExprImpl::Unary(_) => true,
        }
    }

    pub fn get_postfix(&self) -> Option<&PostfixExpr> {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(ref postfix_expr) => Some(postfix_expr),
            &UnaryExprImpl::Unary(_) => None,
        }
    }
    pub fn get_right(&self) -> Option<&UnaryExpr> {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(_) => None,
            &UnaryExprImpl::Unary(ActualUnaryExpr{ ref right, operator: ref _1, operator_strpos: ref _2, all_strpos: ref _3 }) => Some(right),
        }
    }
    pub fn get_operator(&self) -> Option<&SeperatorKind> {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(_) => None,
            &UnaryExprImpl::Unary(ActualUnaryExpr{ ref operator, right: ref _1, operator_strpos: ref _2, all_strpos: ref _3 }) => Some(operator),
        }
    }
    pub fn get_operator_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(_) => StringPosition::new(),
            &UnaryExprImpl::Unary(ActualUnaryExpr{ ref operator_strpos, operator: ref _1, right: ref _2, all_strpos: ref _3 }) => *operator_strpos,
        }
    }
    pub fn get_all_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &UnaryExprImpl::Postfix(ref postfix_expr) => postfix_expr.get_all_strpos(),
            &UnaryExprImpl::Unary(ActualUnaryExpr{ ref all_strpos, operator: ref _1, operator_strpos: ref _2, right: ref _3 }) => *all_strpos,
        }
    }
}
impl ISyntaxItemGrammar for UnaryExpr {
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        PostfixExpr::is_first_final(tokens, index) || match tokens.nth(index) {
            &Token::Sep(ref sep) if sep.is_category(SeperatorCategory::Unary) => true,
            _ => false
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemGrammarX for UnaryExpr {
    fn is_first_finalx(sess: &ParseSession) -> bool {
        PostfixExpr::is_first_finalx(sess) || match sess.tk {
            &Token::Sep(ref sep) if sep.is_category(SeperatorCategory::Unary) => true,
            _ => false,
        }
    }
}
impl ISyntaxItemParse for UnaryExpr {

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<UnaryExpr>, usize) {

        let mut current_len = 0;
        let mut operator_and_strposs = Vec::new();
        loop {
            match tokens.nth(index + current_len) {
                &Token::Sep(operator) if operator.is_category(SeperatorCategory::Unary) => {
                    operator_and_strposs.push((operator, tokens.pos(index + current_len)));
                    current_len += 1;
                }
                _ => match PostfixExpr::parse(tokens, messages, index + current_len) {
                    (None, length) => return (None, current_len + length),
                    (Some(postfix_expr), postfix_len) => {
                        let mut current_unary = UnaryExpr::new_postfix(postfix_expr);
                        for (operator, operator_strpos) in operator_and_strposs.into_iter().rev() {
                            current_unary = UnaryExpr::new_unary(operator, operator_strpos, current_unary);
                        }
                        return (Some(current_unary), current_len + postfix_len);
                    }
                }
            } 
        }
    }
}
#[cfg(feature = "parse_sess")]
impl ISyntaxItemParseX for UnaryExpr {

    fn parsex(sess: &mut ParseSession) -> ParseResult<UnaryExpr> {
        
        let mut operator_and_strposs = Vec::new();
        loop {
            match (sess.tk, sess.pos) {
                (&Token::Sep(operator), operator_strpos) if operator.is_category(SeperatorCategory::Unary) => {
                    sess.move_next();
                    operator_and_strposs.push((operator, operator_strpos));
                }
                _ => {
                    let postfix_expr = PostfixExpr::parsex(sess)?;
                    let mut current_unary = UnaryExpr::new_postfix(postfix_expr);
                    for (operator, operator_strpos) in operator_and_strposs.into_iter().rev() {
                        current_unary = UnaryExpr::new_unary(operator, operator_strpos, current_unary);
                    }
                    return Ok(current_unary);
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn unary_expr_parse() {
    use lexical::LitValue;
    use super::super::ISyntaxItemWithStr;
    
    assert_eq!{ UnaryExpr::with_test_str("1"), 
        UnaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))) 
    }

    assert_eq!{ UnaryExpr::with_test_str("!~!1"),
        UnaryExpr::new_unary(
            SeperatorKind::LogicalNot, make_str_pos!(1, 1, 1, 1),
            UnaryExpr::new_unary(
                SeperatorKind::BitNot, make_str_pos!(1, 2, 1, 2),            
                UnaryExpr::new_unary(
                    SeperatorKind::LogicalNot, make_str_pos!(1, 3, 1, 3),
                    UnaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(1), make_str_pos!(1, 4, 1, 4))),
                )
            )
        )
    }
}