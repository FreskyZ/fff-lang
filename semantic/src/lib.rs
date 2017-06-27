///! fff-lang
///!
///! semantic, semantic analyze

/* #[cfg_attr(test, macro_use)] */ extern crate codemap;
// #[macro_use] extern crate messages as message;
// #[macro_use] extern crate util;
extern crate lexical;
extern crate syntax;

mod expr;
mod items;
mod statement;

pub use self::items::Block;
pub use self::items::TypeUse;
pub use self::items::LabelDef;
pub use self::items::FnParam;
pub use self::items::FnDef;
pub use self::items::TypeFieldDef;
pub use self::items::TypeDef;
pub use self::items::Package;
pub use self::expr::ArrayDef;
pub use self::expr::BinaryExpr;
pub use self::expr::FnCall;
pub use self::expr::IdentExpr;
pub use self::expr::IndexCall;
pub use self::expr::LitExpr;
pub use self::expr::MemberAccess;
pub use self::expr::ParenExpr;
pub use self::expr::TupleDef;
pub use self::expr::UnaryExpr;
pub use self::expr::Expr;
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

// pub use self::block::Block;
// pub use self::fn_def::FnArg;
// pub use self::fn_def::FnName;
// pub use self::fn_def::FnImpl;
// pub use self::fn_def::FnCollection;
// pub use self::var_def::Var;
// pub use self::var_def::VarCollection;
// pub use self::vm_code::Operand;
// pub use self::vm_code::Code;
// pub use self::type_def::Type;
// pub use self::type_def::TypeCollection;
// pub use self::type_def::TypeField;
// pub use self::session::Program;
// pub use self::session::ItemID;

#[cfg(test)] #[test]
fn it_works() {
    println!("helloworld");
}
