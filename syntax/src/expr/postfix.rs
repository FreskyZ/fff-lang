///! fff-lang
///!
///! syntax/postfix_expr
// PostfixExpr = 
//     PrimaryExpr [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs TypeUse
//         ]*

use std::fmt;

use codemap::Span;

use super::Expr;
use super::PrimaryExpr;
use super::FnCallExpr;
use super::IndexCallExpr;
use super::MemberAccessExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum PostfixExpr {
    FnCall(FnCallExpr),
    IndexCall(IndexCallExpr),
    MemberAccess(MemberAccessExpr),
}
impl ISyntaxItemFormat for PostfixExpr {
    fn format(&self, indent: u32) -> String {
        match self {
            &PostfixExpr::FnCall(ref fn_call) => fn_call.format(indent),
            &PostfixExpr::IndexCall(ref index_call) => index_call.format(indent),
            &PostfixExpr::MemberAccess(ref member_access) => member_access.format(indent),
        }
    }
}
impl fmt::Debug for PostfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl PostfixExpr {
    pub fn get_all_span(&self) -> Span { 
        match self {
            &PostfixExpr::FnCall(ref fn_call) => fn_call.all_span,
            &PostfixExpr::IndexCall(ref index_call) => index_call.all_span,
            &PostfixExpr::MemberAccess(ref member_access) => member_access.all_span,
        }
    }
}
impl ISyntaxItemParse for PostfixExpr {
    type Target = Expr;

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {   
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let mut current_retval = PrimaryExpr::parse(sess)?;
        trace!("parsed primary, current is {:?}", current_retval);

        'postfix: loop {
            if MemberAccessExpr::is_first_final(sess) {
                let mut postfix = MemberAccessExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.name.span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::Postfix(PostfixExpr::MemberAccess(postfix));
            } else if FnCallExpr::is_first_final(sess) {
                let mut postfix = FnCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.paren_span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::Postfix(PostfixExpr::FnCall(postfix));
            } else if IndexCallExpr::is_first_final(sess) {
                let mut postfix = IndexCallExpr::parse(sess)?;
                postfix.all_span = current_retval.get_all_span().merge(&postfix.bracket_span);
                postfix.base = Box::new(current_retval);
                current_retval = Expr::Postfix(PostfixExpr::IndexCall(postfix));
            } else {
                break;
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_retval);
        return Ok(current_retval);
    }
}
