///! fff-lang
///!
///! syntax, syntax tree types and generation

mod prelude;
// pub use prelude::{Node, Visitor, parse};
pub use prelude::{Formatter, ISyntaxFormat};
#[cfg(test)]
pub(crate) use prelude::make_node;
#[cfg(test)]
use crate::diagnostics::make_errors;

mod array_def;
mod binary_expr;
mod block_stmt;
mod block;
mod expr_list;
mod expr;
mod expr_stmt;
mod fn_call;
mod fn_def;
mod for_stmt;
mod if_stmt;
mod import_stmt;
mod index_call;
mod jump_stmt;
mod label_def;
mod lit_expr;
mod loop_stmt;
mod member_access;
mod module;
mod name;
mod priority_proxy;
mod range_expr;
mod ret_stmt;
mod stmt;
mod tuple_def;
mod type_def;
mod type_use;
mod unary_expr;
mod use_stmt;
mod var_decl;
mod while_stmt;

pub use array_def::ArrayDef;
pub use binary_expr::BinaryExpr;
pub use block_stmt::BlockStatement;
pub use block::Block;
pub use expr_list::ExprList;
use expr_list::ExprListParseResult;
#[cfg(test)]
use expr_list::make_exprs;
pub use expr::Expr;
pub use expr_stmt::{SimpleExprStatement, AssignExprStatement};
pub use fn_call::FnCallExpr;
pub use fn_def::{FnDef, FnParam};
pub use for_stmt::ForStatement;
pub use if_stmt::{IfStatement, IfClause, ElseIfClause, ElseClause};
pub use import_stmt::ImportStatement;
pub use index_call::IndexCallExpr;
pub use jump_stmt::{BreakStatement, ContinueStatement};
pub use label_def::LabelDef;
pub use lit_expr::{LitExpr, LitValue};
pub use loop_stmt::LoopStatement;
pub use member_access::MemberAccessExpr;
pub use module::Module;
pub use name::{Name, SimpleName};
use priority_proxy::PostfixExpr;
pub use range_expr::{RangeBothExpr, RangeFullExpr, RangeLeftExpr, RangeRightExpr};
use range_expr::RangeExpr;
pub use ret_stmt::ReturnStatement;
pub use stmt::{Statement, Item};
pub use tuple_def::{ParenExpr, TupleDef};
pub use type_def::{TypeDef, TypeFieldDef};
pub use type_use::TypeUse;
pub use unary_expr::UnaryExpr;
pub use use_stmt::UseStatement;
pub use var_decl::VarDeclStatement;
pub use while_stmt::WhileStatement;

// TODO: 
// replace more proper place by IdentExpr and ExprList
// abort IdentExpr to use Name, check name should be single segment at many where

pub fn parse<'ecx, 'scx, F>(chars: crate::lexical::Parser<'ecx, 'scx, F>) -> Result<Module, ()> where F: crate::source::FileSystem {
    use prelude::Node;
    let mut context = prelude::ParseSession::new(chars);
    let result = Module::parse(&mut context);
    context.base.finish();
    result
}
