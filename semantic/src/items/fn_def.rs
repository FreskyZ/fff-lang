///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;
use super::FromSyntax;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}
impl FromSyntax<syntax::FnParam> for FnParam {

    fn from_syntax(node: syntax::FnParam) -> FnParam {
        FnParam{
            name: node.name,
            typeuse: FromSyntax::from_syntax(node.decltype),
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

    fn from_syntax(node: syntax::FnDef) -> FnDef {
        FnDef{
            name: node.name,
            params: node.params.into_iter().map(FromSyntax::from_syntax).collect(),
            rettype: node.ret_type.map(FromSyntax::from_syntax),
            body: FromSyntax::from_syntax(node.body),
        }
    }
}

// pub struct FnParam {
//     pub name: SymbolID,
//     pub typeid: TypeID,
// }
// impl FnParam {
//     pub fn new(name: SymbolID, typeid: TypeID) -> FnParam { FnParam{ name, typeid } }
// }

// pub struct FnDef {
//     pub params: Vec<FnParam>,
//     pub rettype: TypeID,
//     pub body: syntax::Block,
// }
