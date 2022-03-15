///! syntax: syntax parse

mod prelude;
pub use prelude::{Node, Visitor};

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
mod index_call;
mod jump_stmt;
mod label_def;
mod lit_expr;
mod loop_stmt;
mod member_access;
mod module;
mod module_stmt;
mod name;
mod priority_proxy;
mod range_expr;
mod ret_stmt;
mod stmt;
mod tuple_def;
mod type_def;
mod type_ref;
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
pub use expr::Expr;
pub use expr_stmt::{SimpleExprStatement, AssignExprStatement};
pub use fn_call::FnCallExpr;
pub use fn_def::{FnDef, FnParam};
pub use for_stmt::ForStatement;
pub use if_stmt::{IfStatement, IfClause, ElseIfClause, ElseClause};
pub use index_call::IndexCallExpr;
pub use jump_stmt::{BreakStatement, ContinueStatement};
pub use label_def::LabelDef;
pub use lit_expr::{LitExpr, LitValue};
pub use loop_stmt::LoopStatement;
pub use member_access::MemberAccessExpr;
pub use module::Module;
pub use module_stmt::ModuleStatement;
pub use name::{Name, SimpleName};
use priority_proxy::PostfixExpr;
pub use range_expr::{RangeBothExpr, RangeFullExpr, RangeLeftExpr, RangeRightExpr};
use range_expr::RangeExpr;
pub use ret_stmt::ReturnStatement;
pub use stmt::{Statement, Item};
pub use tuple_def::{ParenExpr, TupleDef};
pub use type_def::{TypeDef, TypeFieldDef};
pub use type_ref::TypeRef;
pub use unary_expr::UnaryExpr;
pub use use_stmt::UseStatement;
pub use var_decl::VarDeclStatement;
pub use while_stmt::WhileStatement;

// parse any types of node for test
pub fn parse_any<O, N: Node<ParseOutput = O>>(chars: crate::lexical::Parser) -> Result<O, ()> {
    let mut context = prelude::ParseContext::new(chars);
    let result = N::parse(&mut context);
    context.finish();
    result
}
// formal public api only parses module
pub fn parse(chars: crate::lexical::Parser) -> Result<Module, ()> {
    parse_any::<_, Module>(chars)
}

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! make_node {
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        match parse_any::<_, $ty>(scx.entry("1")) {
            Ok(node) => { assert_eq!(ecx, crate::diagnostics::make_errors!()); node },
            Err(_) => { panic!("{}", ecx.display(&scx)) },
        }
    }};
}
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use make_node;
