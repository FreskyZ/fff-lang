///! fff-lang
///!
///! semantic/common nodes

use codemap::SymbolID;
use syntax;

mod fn_def;
mod type_def;

pub use self::type_def::TypeFieldDef;
pub use self::type_def::TypeDef;
pub use self::fn_def::FnParam;
pub use self::fn_def::FnDef;

use super::Statement;
use super::FromSyntax;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeUse {
    pub base_name: SymbolID,
    pub params: Vec<TypeUse>,
}
impl FromSyntax<syntax::TypeUse> for TypeUse {
    
    fn from_syntax(node: syntax::TypeUse) -> TypeUse {
        TypeUse{
            base_name: node.base,
            params: node.params.into_iter().map(FromSyntax::from_syntax).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LabelDef {
    pub name: SymbolID,
}
impl FromSyntax<syntax::LabelDef> for LabelDef {
    fn from_syntax(node: syntax::LabelDef) -> LabelDef {
        LabelDef{
            name: node.name,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Block {
    pub items: Vec<Statement>,
}
impl FromSyntax<syntax::Block> for Block {
    fn from_syntax(node: syntax::Block) -> Block {
        Block{
            items: node.items.into_iter().map(FromSyntax::from_syntax).collect(),
        }
    }
}