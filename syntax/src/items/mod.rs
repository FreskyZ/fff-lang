///! fff-lang
///!
///! syntax/items, common syntax item types

mod block;
mod function_def;
mod label_def;
mod type_use;

pub use self::block::Block;
pub use self::function_def::FunctionDef;
pub use self::function_def::Argument;
pub use self::type_use::TypeUse;
pub use self::type_use::TypeUseF;
pub use self::label_def::LabelDef;