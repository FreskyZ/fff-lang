///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;
use super::super::ISemanticAnalyze;
use super::super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnDef {
    pub name: SymbolID,
    pub params: Vec<FnParam>,
    pub rettype: Option<TypeUse>,
    pub body: Block,
}
impl ISemanticAnalyze for FnDef {

    type SyntaxItem = syntax::FnDef;

    fn from_syntax(node: syntax::FnDef, parent_scope: SharedDefScope) -> FnDef {
        FnDef{
            name: node.name,
            params: node.params.into_iter().map(|param| {
                FnParam{
                    name: param.name,
                    typeuse: TypeUse::from_syntax(param.decltype, parent_scope.clone()),
                }
            }).collect(),
            rettype: node.ret_type.map(|ty| TypeUse::from_syntax(ty, parent_scope.clone())),
            body: Block::from_syntax(node.body, parent_scope.clone()),
        }
    }
}
