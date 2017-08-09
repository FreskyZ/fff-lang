#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]
///! fff-lang
///!
///! semantic, semantic analyze

#[cfg(test)] #[macro_use] extern crate util;      // difference is util is referenced only in test
#[cfg_attr(test, macro_use)] extern crate codemap;
// #[macro_use] extern crate messages as message;
// #[macro_use] extern crate util;
extern crate lexical;
extern crate syntax;

mod analyze_helper;
mod expr;
mod items;
mod statement;
mod package;
mod def_scope;

pub use self::items::Block;
pub use self::items::TypeUse;
pub use self::items::LabelDef;
pub use self::items::FnParam;
pub use self::items::FnDef;
pub use self::items::TypeFieldDef;
pub use self::items::TypeDef;
pub use self::items::Module;
pub use self::expr::ArrayDef;
pub use self::expr::BinaryExpr;
pub use self::expr::FnCall;
pub use self::expr::IndexCall;
pub use self::expr::LitExpr;
pub use self::expr::MemberAccess;
pub use self::expr::ParenExpr;
pub use self::expr::TupleDef;
pub use self::expr::UnaryExpr;
pub use self::expr::Expr;
pub use self::expr::Name;
pub use self::expr::SimpleName;
pub use self::statement::Statement;
pub use self::statement::BlockStatement;
pub use self::statement::VarDecl;
pub use self::statement::ReturnStatement;
pub use self::statement::BreakStatement;
pub use self::statement::ContinueStatement;
pub use self::statement::SimpleExprStatement;
pub use self::statement::AssignExprStatement;
pub use self::statement::LoopStatement;
pub use self::statement::WhileStatement;
pub use self::statement::ForStatement;
pub use self::statement::IfClause;
pub use self::statement::ElseIfClause;
pub use self::statement::ElseClause;
pub use self::statement::IfStatement;
pub use self::statement::Item;
pub use self::statement::ImportStatement;
pub use self::statement::UseStatement;
pub use self::package::Package;

use self::def_scope::SharedDefScope;
use self::analyze_helper::Formatter;
use self::analyze_helper::ISemanticAnalyze;
