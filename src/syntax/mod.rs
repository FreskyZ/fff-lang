///! fff-lang
///!
///! syntax, syntax tree types and generation

#[macro_use] 
mod expr;
mod parse_sess;
mod items;
mod statement;
mod error_strings;
mod format_helper;
mod module;
//mod syntax_tree;
#[cfg(test)]
mod test_helper;

pub use items::fn_def::FnParam;
pub use items::fn_def::FnDef;
pub use items::type_use::TypeUse;
pub use items::block::Block;
pub use items::label_def::LabelDef;
pub use items::type_def::TypeFieldDef;
pub use items::type_def::TypeDef;
pub use expr::LitValue;
pub use expr::LitExpr;
pub use expr::Expr;
pub use expr::BinaryExpr;
pub use expr::UnaryExpr;
pub use expr::FnCallExpr;
pub use expr::IndexCallExpr;
pub use expr::MemberAccessExpr;
pub use expr::ParenExpr;
pub use expr::TupleDef;
pub use expr::ExprList;
pub use expr::ArrayDef;
pub use expr::RangeBothExpr;
pub use expr::RangeFullExpr;
pub use expr::RangeLeftExpr;
pub use expr::RangeRightExpr;
pub use expr::SimpleName;
pub use expr::Name;
pub use statement::Statement;
pub use statement::BlockStatement;
pub use statement::VarDeclStatement;
pub use statement::ReturnStatement;
pub use statement::BreakStatement;
pub use statement::ContinueStatement;
pub use statement::SimpleExprStatement;
pub use statement::AssignExprStatement;
pub use statement::LoopStatement;
pub use statement::WhileStatement;
pub use statement::ForStatement;
pub use statement::IfClause;
pub use statement::ElseIfClause;
pub use statement::ElseClause;
pub use statement::IfStatement;
pub use statement::UseStatement;
pub use statement::ImportStatement;
pub use statement::Item;
pub use module::Module;
// pub use syntax_tree::SyntaxTree;
// pub use syntax_tree::ImportMap;
use parse_sess::ParseSession;
use parse_sess::ParseResult;
use parse_sess::ISyntaxGrammar;
pub use parse_sess::ISyntaxParse;
pub use format_helper::Formatter;
pub use format_helper::ISyntaxFormat;
#[cfg(test)]
pub(crate) use test_helper::{make_node, try_make_node};

// TODO: 
// replace more proper place by IdentExpr and ExprList
// abort IdentExpr to use Name, check name should be single segment at many where

pub fn parse<'ecx, 'scx, F>(chars: crate::lexical::Parser<'ecx, 'scx, F>) -> Module where F: crate::source::FileSystem {
    let mut context = ParseSession::new(chars);
    match Module::parse(&mut context) {
        Ok(v) => { context.base.finish(); v },
        Err(_) => { println!("{:?}", context.base.diagnostics.format(Some(context.base.chars.context))); context.base.finish(); Module::new(Vec::new()) }
    }
}
