///! fff-lang
///! 
///! syntax/member_access
///! member_access = expr '.' identifier

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
use message::Message;
use lexical::Token;
use lexical::SeperatorKind;

use super::Expr;
use super::IdentExpr;
use super::PrimaryExpr;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct MemberAccessExpr {
    pub base: Box<Expr>,
    pub dot_span: Span,
    pub name: IdentExpr,
    pub all_span: Span,
}
impl ISyntaxItemFormat for MemberAccessExpr {
    fn format(&self, indent: u32) -> String {
        format!("{}MemberAccess <{:?}>\n{}'.' <{:?}>\n{}", 
            MemberAccessExpr::indent_str(indent), self.all_span,
            MemberAccessExpr::indent_str(indent + 1), self.dot_span,
            self.name.format(indent + 1)
        )
    }
}
impl fmt::Debug for MemberAccessExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl ISyntaxItemGrammar for MemberAccessExpr {
    fn is_first_final(sess: &ParseSession) -> bool { sess.tk == &Token::Sep(SeperatorKind::Dot) }
}
impl ISyntaxItemParse for MemberAccessExpr {
    type Target = PostfixExpr;

    fn parse(sess: &mut ParseSession) -> ParseResult<PostfixExpr> {
        
    }
}