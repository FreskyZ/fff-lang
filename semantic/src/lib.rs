///! fff-lang
///!
///! semantic, semantic analyze

#[cfg(test)] #[macro_use] extern crate util;       // difference is util is referenced only in test
#[cfg_attr(test, macro_use)] extern crate codemap;
#[cfg_attr(test, macro_use)] extern crate messages as message;
extern crate lexical;
extern crate syntax;

#[macro_use] mod def_scope;
mod definition;
mod format_helper;
mod analyze_helper;
mod expr;
mod items;
mod statement;
mod module;
mod package;

pub use self::module::Module;
pub use self::package::Package;
pub use self::items::{ Block, TypeUse, LabelDef, FnParam, FnDef, TypeFieldDef, TypeDef };
pub use self::expr::{ ArrayDef, BinaryExpr, FnCall, IndexCall, LitExpr, MemberAccess, 
    ParenExpr, TupleDef, UnaryExpr, Expr, Name, SimpleName };
pub use self::statement::{ Statement, BlockStatement, VarDecl, ReturnStatement, BreakStatement,
    ContinueStatement, SimpleExprStatement, AssignExprStatement, LoopStatement, WhileStatement, 
    ForStatement, IfClause, ElseIfClause, ElseClause, IfStatement, Item, ImportStatement, UseStatement };

use self::format_helper::Formatter;
use self::definition::{ Definition, DefinitionCollection, DefID };
use self::def_scope::{ SharedDefScope, ScopeType };
use self::analyze_helper::{ FromSession, CollectSession, ISemanticAnalyze };
