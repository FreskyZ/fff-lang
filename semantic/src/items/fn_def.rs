///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;
use super::super::FromSyntax;
use super::super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}
impl FromSyntax<syntax::FnParam> for FnParam {
    fn from_syntax(node: syntax::FnParam, parent_scope: SharedDefScope) -> FnParam {
        FnParam{
            name: node.name,
            typeuse: FromSyntax::from_syntax(node.decltype, parent_scope.clone()),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnDef {
    pub name: SymbolID,
    pub params: Vec<FnParam>,
    pub rettype: Option<TypeUse>,
    pub body: Block,
}
impl FromSyntax<syntax::FnDef> for FnDef {

    fn from_syntax(node: syntax::FnDef, parent_scope: SharedDefScope) -> FnDef {
        FnDef{
            name: node.name,
            params: node.params.into_iter().map(|param| FromSyntax::from_syntax(param, parent_scope.clone())).collect(),
            rettype: node.ret_type.map(|ty| FromSyntax::from_syntax(ty, parent_scope.clone())),
            body: FromSyntax::from_syntax(node.body, parent_scope.clone()),
        }
    }
}
