///! syntax: syntax parse

mod prelude;

pub mod ast;

mod visit;
mod visit_impl;
mod pretty;
mod parser;

#[cfg(test)]
mod tests;

pub use parser::Parser;
pub use visit::{Node, Visitor};

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
mod for_stmt;
mod if_stmt;
mod index_call;
mod jump_stmt;
mod label_def;
mod loop_stmt;
mod module;
mod module_stmt;
mod name;
mod plain_type;
mod priority_proxy;
mod range_expr;
mod ret_stmt;
mod tuple_def;
mod type_def;
mod unary_expr;
mod use_stmt;
mod var_decl;
mod while_stmt;

#[cfg(test)]
use parser::{PostfixExpr, RangeExpr};

// parse any types of node for test
pub fn parse_any<P: Parser>(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<P::Output> {
    let parser = crate::lexical::Parser::new(source, diagnostics);
    let mut context = prelude::ParseContext::new(parser);
    let result = P::parse(&mut context).ok();
    context.finish();
    result
}
// formal public api only parses module
pub fn parse(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<ast::Module> {
    parse_any::<ast::Module>(source, diagnostics)
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
