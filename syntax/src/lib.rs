///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

#[macro_use] extern crate messages as message;
#[cfg_attr(test, macro_use)] extern crate util;
#[cfg_attr(test, macro_use)] extern crate codemap;
extern crate lexical;

mod traits;
mod syntax_tree;
mod statement;
mod expr;
mod items;
mod parse_sess;

pub use self::items::FnParam;
pub use self::items::FnDef;
pub use self::items::TypeUse;
pub use self::items::Block;
pub use self::items::LabelDef;
pub use self::items::NameSegment;
pub use self::items::Name;
pub use self::expr::BinaryExpr;
pub use self::expr::UnaryExpr;
pub use self::expr::PostfixExpr;
pub use self::expr::PrimaryExpr;
pub use self::statement::Statement;
pub use self::statement::VarDeclStatement;
pub use self::statement::ReturnStatement;
pub use self::statement::BreakStatement;
pub use self::statement::ContinueStatement;
pub use self::statement::ExprStatement;
pub use self::statement::LoopStatement;
pub use self::statement::WhileStatement;
pub use self::statement::ForStatement;
pub use self::statement::IfConditionBody;
pub use self::statement::IfStatement;
pub use self::syntax_tree::SyntaxTree;

use self::parse_sess::ParseSession;
use self::parse_sess::ParseResult;
use self::traits::ISyntaxItemFormat;
use self::traits::ISyntaxItemGrammar;
pub use self::traits::ISyntaxItemParse; // for semantic/traits
pub use self::traits::ISyntaxItemWithStr;
