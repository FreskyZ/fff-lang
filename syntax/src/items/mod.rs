///! fff-lang
///!
///! syntax/items, common syntax item types

mod block;
mod fn_def;
mod label_def;
mod type_use;
mod name;

pub use self::block::Block;
pub use self::fn_def::FnParam;
pub use self::fn_def::FnDef;
pub use self::type_use::TypeUse;
pub use self::type_use::TypeUseF;
pub use self::type_use::ActualTypeUse;
pub use self::label_def::LabelDef;
pub use self::name::NameSegment;
pub use self::name::Name;