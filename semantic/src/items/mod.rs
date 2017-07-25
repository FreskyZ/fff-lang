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
use super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeUse {
    pub base_name: SymbolID,
    pub params: Vec<TypeUse>,
}
impl FromSyntax<syntax::TypeUse> for TypeUse {
    
    fn from_syntax(node: syntax::TypeUse, parent_scope: SharedDefScope) -> TypeUse {
        TypeUse{
            base_name: node.base,
            params: node.params.into_iter().map(|param| FromSyntax::from_syntax(param, parent_scope.clone())).collect(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LabelDef {
    pub name: SymbolID,
}
impl FromSyntax<syntax::LabelDef> for LabelDef {
    fn from_syntax(node: syntax::LabelDef, parent_scope: SharedDefScope) -> LabelDef {
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
    fn from_syntax(node: syntax::Block, parent_scope: SharedDefScope) -> Block {
        Block{
            items: node.items.into_iter().map(|item| FromSyntax::from_syntax(item, parent_scope.clone())).collect(),
        }
    }
}