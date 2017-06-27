///! fff-lang
///!
///! semantic/common nodes

use codemap::SymbolID;
use syntax;

mod fn_def;
mod type_def;
mod package;

pub use self::type_def::TypeFieldDef;
pub use self::type_def::TypeDef;
pub use self::fn_def::FnParam;
pub use self::fn_def::FnDef;
pub use self::package::Package;

use super::Statement;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeUse {
    pub base_name: SymbolID,
    pub params: Vec<TypeUse>,
}
impl From<syntax::TypeUse> for TypeUse {
    
    fn from(node: syntax::TypeUse) -> TypeUse {
        TypeUse{
            base_name: node.base,
            params: node.params.into_iter().map(Into::into).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LabelDef {
    pub name: SymbolID,
}
impl From<syntax::LabelDef> for LabelDef {
    fn from(node: syntax::LabelDef) -> LabelDef {
        LabelDef{
            name: node.name,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Block {
    pub items: Vec<Statement>,
}
impl From<syntax::Block> for Block {
    fn from(node: syntax::Block) -> Block {
        Block{
            items: node.items.into_iter().map(Into::into).collect(),
        }
    }
}