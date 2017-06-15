#![allow(dead_code)]
///! fff-lang
///! 
///! syntax/expr

use codemap::Span;
use codemap::SymbolID;
use lexical::LitValue;
use lexical::SeperatorKind;

mod primary_exprs;
mod binary_exprs;

use self::primary_exprs::LitExpr;
use self::primary_exprs::IdentExpr;
use self::binary_exprs::BinaryExpr;

use super::ParseResult;
use super::ParseSession;
use super::ISyntaxItemParse;

trait IExprParse {
    fn parse<'a, 'b, 'c, F: Fn(ParseSession<'a, 'b, 'c>) -> ParseResult<Expr>>(sess: ParseSession<'a, 'b, 'c>, last_parser: F) -> ParseResult<Expr>;
}

pub struct ParenExpr {
    pub expr: Box<Expr>,
    pub paren_span: Span,
}
pub struct ExprList {
    pub items: Vec<Expr>,
    pub quote_span: Span,
}
pub struct ArrayDef2Expr {
    pub expr1: Box<Expr>,
    pub expr2: Box<Expr>,
    pub bracket_span: Span,
    pub semicolon_span: Span,
}
pub struct MemberAccessExpr {
    pub base: Box<Expr>,  // Yes it should be PrimaryExpr but allows it in physical structure
    pub dot_span: Span,
    pub member: IdentExpr,
}
pub struct SubscriptionExpr {
    pub base: Box<Expr>,
    pub bracket_span: Span,
    pub indexers: ExprList,
}
pub struct FunctionCallExpr {
    pub base: Box<Expr>,
    pub paren_span: Span,
    pub params: ExprList,
}
pub struct PrefixExpr {
    pub prefix: SeperatorKind,
    pub prefix_span: Span,
    pub base: Box<Expr>,
}

// expr_list = expr { "," expr }

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub enum Expr {
    Lit(LitExpr),                   // = literal;
    Ident(IdentExpr),               // = identifier;
    Paren(ParenExpr),               // = "(" expr ")";
    TupleDef(ExprList),             // = "(" [ expr_list ] ")";
    ArrayDef(ExprList),             // = "[" [ expr_list ] "]";
    ArrayDef2(ArrayDef2Expr),       // = "[" expr ";" expr "]";
    MemberAccess(MemberAccessExpr), // = expr "." identifier;
    Subscription(SubscriptionExpr), // = expr "[" [ expr_list ] "]";
    FunctionCall(FunctionCallExpr), // = expr "(" [ expr_list ] "]";
    Prefix(PrefixExpr),             // = prefix_operator expr;
    Binary(BinaryExpr)              // = expr binary_operator expr;
}
impl Expr {
    pub fn get_all_span(&self) -> Span { Default::default() }
}

impl ISyntaxItemParse for Expr {

    fn parse(sess: &mut ParseSession) -> ParseResult<Expr> {
        Expr::Lit(LitValue::from(42))
    }
}

// TODO: abort this 
// Change every expr::*Expr to generate Expr instead of Self in parse, 
// every where use Expr instead of detailed UnaryExpr, PostfixExpr etc.
// so update ISyntaxItemParse to ISyntaxNodeParse<Target = Self>
// use IdentExpr to replace old xxx_ident and xxx_span pair
// use ExprList to replace old Vec<*Expr>