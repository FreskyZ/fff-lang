///! fff-lang
///!
///! syntax/priority level proxy
///! primary_expr = ident_expr | lit_expr | unit_lit | paren_expr | tuple_def | array_def

use lexical::Token;
use lexical::KeywordKind;

use super::LitExpr;
use super::IdentExpr;
use super::TupleDef;
use super::ArrayDef;
use super::Expr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemGrammar;

pub struct PrimaryExpr;
impl ISyntaxItemParse for PrimaryExpr {
    type Target = Expr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("start parsing, current token: {:?}", sess.tk);

        if LitExpr::is_first_final(sess) {
            return LitExpr::parse(sess);
        } else if IdentExpr::is_first_final(sess) {
            return IdentExpr::parse(sess);
        } else if TupleDef::is_first_final(sess) {
            return TupleDef::parse(sess);
        } else if ArrayDef::is_first_final(sess) {
            return ArrayDef::parse(sess);
        }

        if let (&Token::Keyword(KeywordKind::This), this_span) = (sess.tk, sess.pos) {
            sess.move_next();
            return Ok(Expr::Ident(IdentExpr::new(sess.symbols.intern_str("this"), this_span)));
        } else {
            return sess.push_unexpect("primary expr");
        }
    }
}
