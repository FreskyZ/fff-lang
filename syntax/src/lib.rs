///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

#[macro_use] extern crate messages as message;
#[cfg_attr(test, macro_use)] extern crate util;
#[cfg_attr(test, macro_use)] extern crate codepos;
extern crate lexical;

#[macro_use] mod traits;
mod syntax_tree;
mod function_def;
mod type_use;
mod statement;
mod block;
mod expr;
mod label_def;

pub use self::syntax_tree::SyntaxTree;
pub use self::function_def::Argument;
pub use self::function_def::FunctionDef;
pub use self::expr::Expression;
pub use self::expr::BinaryExpr;
pub use self::expr::UnaryExpr;
pub use self::expr::PostfixExpr;
pub use self::expr::PrimaryExpr;
pub use self::statement::Statement;
pub use self::statement::VarDeclStatement;
pub use self::statement::ReturnStatement;
pub use self::statement::BreakStatement;
pub use self::statement::ContinueStatement;
pub use self::statement::ExpressionStatement;
pub use self::statement::LoopStatement;
pub use self::statement::WhileStatement;
pub use self::statement::ForStatement;
pub use self::statement::ElseIfBranch;
pub use self::statement::IfStatement;
pub use self::type_use::TypeUse;
pub use self::type_use::TypeUseF;
pub use self::block::Block;
pub use self::label_def::LabelDef;

use self::traits::ISyntaxItem;
use self::traits::ISyntaxItemFormat;
pub use self::traits::ISyntaxItemWithStr;

#[cfg(test)] use self::traits::TestCase;

// TODO: recoverable, recoverable is actually another syntax rule with emitting messages