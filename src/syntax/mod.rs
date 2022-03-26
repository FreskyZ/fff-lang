///! syntax: syntax parse

mod prelude;
pub use prelude::{Node, Parser, Visitor};

mod abc;
mod array_def;
mod array_type;
mod binary_expr;
mod block_stmt;
mod block;
mod enum_def;
mod expr_list;
mod expr;
mod expr_stmt;
mod fn_call;
mod fn_def;
mod fn_type;
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
mod object;
mod plain_type;
mod primitive_type;
mod priority_proxy;
mod range_expr;
mod ref_type;
mod ret_stmt;
mod tuple_def;
mod tuple_type;
mod type_def;
mod unary_expr;
mod use_stmt;
mod var_decl;
mod while_stmt;

pub use abc::{Statement, Item, TypeRef};
pub use array_def::ArrayDef;
pub use array_type::ArrayType;
pub use binary_expr::BinaryExpr;
pub use block_stmt::BlockStatement;
pub use block::Block;
pub use enum_def::{EnumDef, EnumVariant};
pub use expr_list::ExprList;
use expr_list::ExprListParseResult;
pub use expr::Expr;
pub use expr_stmt::{SimpleExprStatement, AssignExprStatement};
pub use fn_call::FnCallExpr;
pub use fn_def::{FnDef, FnParam};
pub use fn_type::{FnType, FnTypeParam};
pub use for_stmt::ForStatement;
pub use if_stmt::{IfStatement, IfClause, ElseClause};
pub use index_call::IndexCallExpr;
pub use jump_stmt::{BreakStatement, ContinueStatement};
pub use label_def::LabelDef;
pub use lit_expr::{LitExpr, LitValue};
pub use loop_stmt::LoopStatement;
pub use member_access::MemberAccessExpr;
pub use module::Module;
pub use module_stmt::ModuleStatement;
pub use name::{Name, NameSegment};
pub use object::{ObjectLiteral, ObjectLiteralField};
pub use plain_type::{TypeAsSegment, TypeSegment, PlainType};
pub use primitive_type::PrimitiveType;
use priority_proxy::PostfixExpr;
pub use range_expr::{RangeBothExpr, RangeFullExpr, RangeLeftExpr, RangeRightExpr};
use range_expr::RangeExpr;
pub use ref_type::RefType;
pub use ret_stmt::ReturnStatement;
pub use tuple_def::{ParenExpr, TupleDef};
pub use type_def::{TypeDef, TypeFieldDef};
pub use tuple_type::TupleType;
pub use unary_expr::UnaryExpr;
pub use use_stmt::UseStatement;
pub use var_decl::VarDeclStatement;
pub use while_stmt::WhileStatement;

// parse any types of node for test
pub fn parse_any<P: Parser>(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<P::Output> {
    let parser = crate::lexical::Parser::new(source, diagnostics);
    let mut context = prelude::ParseContext::new(parser);
    let result = P::parse(&mut context).ok();
    context.finish();
    result
}
// formal public api only parses module
pub fn parse(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<Module> {
    parse_any::<Module>(source, diagnostics)
}

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! make_node {
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        match parse_any::<_, $ty>(scx.entry("1"), &mut ecx) {
            Ok(node) => { assert_eq!(ecx, crate::diagnostics::make_errors!()); node },
            Err(_) => { panic!("{}", ecx.display(&scx)) },
        }
    }};
}
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use make_node;
