///! fff-lang
///!
///! semantic/common nodes

use codemap::SymbolID;
use codemap::SymbolCollection;
use syntax;

mod fn_def;
mod type_def;
mod module;

pub use self::type_def::TypeFieldDef;
pub use self::type_def::TypeDef;
pub use self::fn_def::FnParam;
pub use self::fn_def::FnDef;
pub use self::module::Module;

use super::Statement;
use super::SharedDefScope;
use super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeUse {
    pub base_name: SymbolID,
    pub params: Vec<TypeUse>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for TypeUse {
    
    type SyntaxItem = syntax::TypeUse;

    fn from_syntax(node: syntax::TypeUse, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> TypeUse {
        TypeUse{
            base_name: node.base,
            params: node.params.into_iter().map(|param| TypeUse::from_syntax(param, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LabelDef {
    pub name: SymbolID,
    pub parent_scope: SharedDefScope
}
impl ISemanticAnalyze for LabelDef {

    type SyntaxItem = syntax::LabelDef;

    fn from_syntax(node: syntax::LabelDef, parent_scope: SharedDefScope, _symbols: &mut SymbolCollection) -> LabelDef {
        LabelDef{
            name: node.name,
            parent_scope: parent_scope,
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Block {
    pub items: Vec<Statement>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for Block {

    type SyntaxItem = syntax::Block;
    
    fn from_syntax(node: syntax::Block, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Block {
        Block{
            items: node.items.into_iter().map(|item| Statement::from_syntax(item, parent_scope.clone(), symbols)).collect(),
            parent_scope: parent_scope,
        }
    }
}
