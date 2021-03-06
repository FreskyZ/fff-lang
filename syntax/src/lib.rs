///! fff-lang
///!
///! syntax, syntax tree types and generation

#[cfg_attr(test, macro_use)] extern crate messages as message;
#[cfg_attr(test, macro_use)] extern crate util;
#[cfg_attr(test, macro_use)] extern crate codemap;
#[cfg_attr(test, macro_use)] extern crate lexical;

#[macro_use] mod expr;
mod parse_sess;
mod items;
mod statement;
mod error_strings;
mod format_helper;
mod syntax_tree;
mod test_helper;

pub use self::items::fn_def::FnParam;
pub use self::items::fn_def::FnDef;
pub use self::items::type_use::TypeUse;
pub use self::items::block::Block;
pub use self::items::label_def::LabelDef;
pub use self::items::type_def::TypeFieldDef;
pub use self::items::type_def::TypeDef;
pub use self::items::module::Module;
pub use self::expr::LitExpr;
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
pub use self::expr::RangeBothExpr;
pub use self::expr::RangeFullExpr;
pub use self::expr::RangeLeftExpr;
pub use self::expr::RangeRightExpr;
pub use self::expr::SimpleName;
pub use self::expr::Name;
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
pub use self::statement::UseStatement;
pub use self::statement::ImportStatement;
pub use self::statement::Item;
pub use self::syntax_tree::SyntaxTree;
pub use self::syntax_tree::ImportMap;

use self::parse_sess::ParseSession;
use self::parse_sess::ParseResult;
use self::parse_sess::ISyntaxGrammar;
pub use self::parse_sess::ISyntaxParse;

pub use self::format_helper::Formatter;
pub use self::format_helper::ISyntaxFormat;
pub use self::test_helper::TestInput;
pub use self::test_helper::WithTestInput;

// TODO: 
// replace more proper place by IdentExpr and ExprList
// abort IdentExpr to use Name, check name should be single segment at many where