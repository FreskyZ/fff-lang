///! fff-lang
///!
///! semantic, semantic analyze

/* #[cfg_attr(test, macro_use)] */ extern crate codemap;
// #[macro_use] extern crate messages as message;
// #[macro_use] extern crate util;
// extern crate lexical;
// extern crate syntax;

mod type_def;
// mod block;
// mod expression;
// mod fn_def;
// mod loop_def;
// mod type_def;
// mod session;
// mod statement;
// mod var_def;
// mod vm_code;

pub use self::type_def::TypeID;
pub use self::type_def::TypeField;
pub use self::type_def::TypeDef;
pub use self::type_def::TypeCollection;

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
