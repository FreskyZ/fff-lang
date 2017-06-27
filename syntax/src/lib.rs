///! fff-lang
///!
///! syntax, syntax tree types and generation

#[macro_use] extern crate messages as message;
#[cfg_attr(test, macro_use)] extern crate util;
#[cfg_attr(test, macro_use)] extern crate codemap;
extern crate lexical;

#[macro_use] mod expr;
mod traits;
mod syntax_tree;
mod statement;
mod items;
mod parse_sess;
mod error_strings;

pub use self::items::fn_def::FnParam;
pub use self::items::fn_def::FnDef;
pub use self::items::type_use::TypeUse;
pub use self::items::block::Block;
pub use self::items::label_def::LabelDef;
pub use self::items::name::NameSegment;
pub use self::items::name::Name;
pub use self::items::type_def::TypeFieldDef;
pub use self::items::type_def::TypeDef;
pub use self::expr::LitExpr;
pub use self::expr::IdentExpr;
pub use self::expr::Expr;
pub use self::expr::BinaryExpr;
pub use self::expr::UnaryExpr;
pub use self::expr::FnCallExpr;
pub use self::expr::IndexCallExpr;
pub use self::expr::MemberAccessExpr;
pub use self::expr::ParenExpr;
pub use self::expr::TupleDef;
pub use self::expr::ExprList;
pub use self::expr::ArrayDef;
pub use self::statement::Statement;
pub use self::statement::BlockStatement;
pub use self::statement::VarDeclStatement;
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
pub use self::syntax_tree::SyntaxTree;

use self::parse_sess::ParseSession;
use self::parse_sess::ParseResult;
use self::traits::ISyntaxItemFormat;
use self::traits::ISyntaxItemGrammar;
pub use self::traits::ISyntaxItemParse; // for semantic/traits
pub use self::traits::ISyntaxItemWithStr;

// TODO: 
// replace more proper place by IdentExpr and ExprList
// abort IdentExpr to use Name, check name should be single segment at many where