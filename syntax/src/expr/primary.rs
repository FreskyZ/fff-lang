///! fff-lang
///!
///! syntax/primary_expr
// PrimaryExpr = 
//     ident_expr   // TODO: change to name 
//     | lit_expr | unit_lit | paren_expr | tuple_def
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

// TODO: split ArrayDef TupleDef out

use std::fmt;

use codemap::Span;

use lexical::Token;
use lexical::KeywordKind;

use super::LitExpr;
use super::IdentExpr;
use super::ParenExpr;
use super::TupleDef;
use super::ArrayDef;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub enum PrimaryExpr {
    Lit(LitExpr),
    Ident(IdentExpr),
    Paren(ParenExpr),
    Tuple(TupleDef),
    Array(ArrayDef),
}
impl ISyntaxItemFormat for PrimaryExpr {
    fn format(&self, indent: u32) -> String {
        match self {
            &PrimaryExpr::Lit(ref lit_expr) => lit_expr.format(indent),
            &PrimaryExpr::Ident(ref ident_expr) => ident_expr.format(indent),
            &PrimaryExpr::Paren(ref paren_expr) => paren_expr.format(indent),
            &PrimaryExpr::Tuple(ref tuple_def) => tuple_def.format(indent),
            &PrimaryExpr::Array(ref array_def) => array_def.format(indent),
        }
    }
}
impl fmt::Debug for PrimaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(0)) }
}
impl PrimaryExpr { // New
    
    pub fn get_all_span(&self) -> Span { 
        match self {
            &PrimaryExpr::Lit(ref lit_expr) => lit_expr.span,
            &PrimaryExpr::Ident(ref ident_expr) => ident_expr.span,
            &PrimaryExpr::Paren(ref paren_expr) => paren_expr.span,
            &PrimaryExpr::Tuple(ref tuple_def) => tuple_def.paren_span, 
            &PrimaryExpr::Array(ref array_def) => array_def.bracket_span,
        }
    }
}
impl ISyntaxItemGrammar for PrimaryExpr {
    fn is_first_final(sess: &ParseSession) -> bool {
        LitExpr::is_first_final(sess)
        || IdentExpr::is_first_final(sess)
        || TupleDef::is_first_final(sess)
        || ArrayDef::is_first_final(sess)
        || sess.tk == &Token::Keyword(KeywordKind::This)
    }
}
impl ISyntaxItemParse for PrimaryExpr {
    type Target = PrimaryExpr;
    
    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {
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
            return Ok(PrimaryExpr::Ident(IdentExpr::new(sess.symbols.intern_str("this"), this_span)));
        } else {
            return sess.push_unexpect("primary expr");
        }
    }
}
