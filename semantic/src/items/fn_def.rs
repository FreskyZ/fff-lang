///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use codemap::SymbolCollection;
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
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for FnDef {

    type SyntaxItem = syntax::FnDef;

    fn from_syntax(node: syntax::FnDef, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> FnDef {
        let this_scope = parent_scope.sub(symbols.get(node.name).unwrap());
        FnDef{
            name: node.name,
            params: node.params.into_iter().map(|param| {
                FnParam{
                    name: param.name,
                    typeuse: TypeUse::from_syntax(param.decltype, this_scope.clone(), symbols),
                }
            }).collect(),
            rettype: node.ret_type.map(|ty| TypeUse::from_syntax(ty, this_scope.clone(), symbols)),
            body: Block::from_syntax(node.body, this_scope.clone(), symbols),
            this_scope: this_scope,
        }
    }
}
