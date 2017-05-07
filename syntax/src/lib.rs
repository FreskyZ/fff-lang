///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

#[macro_use] extern crate messages as message;
/* #[cfg_attr(test, macro_use)] */ extern crate util;
#[cfg_attr(test, macro_use)] extern crate codepos;
extern crate lexical;

#[macro_use] mod traits;
mod syntax_tree;
mod statement;
mod expr;
mod items;
#[cfg(feature = "parse_sess")] mod parse_sess;

pub use self::items::FnParam;
pub use self::items::FnDef;
pub use self::items::TypeUse;
pub use self::items::TypeUseF;
pub use self::items::Block;
pub use self::items::LabelDef;
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

#[cfg(feature = "parse_sess")] use self::parse_sess::ParseSession;
#[cfg(feature = "parse_sess")] use self::parse_sess::ParseResult;
#[cfg(feature = "parse_sess")] use self::parse_sess::ISyntaxItemParseX;
#[cfg(feature = "parse_sess")] use self::parse_sess::ISyntaxItemGrammarX;
use self::traits::ISyntaxItemParse;
use self::traits::ISyntaxItemFormat;
use self::traits::ISyntaxItemGrammar;
pub use self::traits::ISyntaxItemWithStr;
