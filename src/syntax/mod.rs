///! fff-lang
///!
///! syntax, syntax tree types and generation

#[macro_use] 
mod expr;
mod parse_sess;
mod items;
mod statement;
mod format_helper;
mod module;

// common imports for syntax tree node parsers
pub mod prelude {
    pub use std::fmt;
    pub use crate::source::{FileSystem, Span, IsId};
    pub use crate::diagnostics::{Message, strings};
    pub use crate::lexical::{Token, Numeric, Separator, SeparatorKind, Keyword, KeywordKind};
    pub use super::parse_sess::{ParseSession, ParseResult, ISyntaxGrammar, ISyntaxParse};
    pub use super::format_helper::{Formatter, ISyntaxFormat};
}

pub use items::{fn_def::{FnParam, FnDef}, type_use::TypeUse, 
    block::Block, label_def::LabelDef, type_def::{TypeFieldDef, TypeDef}};
pub use expr::{LitValue, LitExpr, Expr, BinaryExpr, UnaryExpr, FnCallExpr, IndexCallExpr, 
    MemberAccessExpr, ParenExpr, TupleDef, ExprList, ArrayDef, RangeBothExpr, RangeFullExpr, 
    RangeLeftExpr, RangeRightExpr, SimpleName, Name};
pub use statement::{Statement, BlockStatement, VarDeclStatement, ReturnStatement, BreakStatement, ContinueStatement, 
    SimpleExprStatement, AssignExprStatement, LoopStatement, WhileStatement, ForStatement, IfClause, ElseIfClause, 
    ElseClause, IfStatement, UseStatement, ImportStatement, Item};
pub use module::Module;

#[cfg(test)]
pub(crate) use parse_sess::make_node;

// TODO: 
// replace more proper place by IdentExpr and ExprList
// abort IdentExpr to use Name, check name should be single segment at many where

pub fn parse<'ecx, 'scx, F>(chars: crate::lexical::Parser<'ecx, 'scx, F>) -> Result<Module, ()> where F: crate::source::FileSystem {
    use prelude::ISyntaxParse;
    let mut context = prelude::ParseSession::new(chars);
    let result = Module::parse(&mut context);
    context.base.finish();
    result
}
