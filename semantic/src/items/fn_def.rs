///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}
impl From<syntax::FnParam> for FnParam {

    fn from(node: syntax::FnParam) -> FnParam {
        FnParam{
            name: node.name,
            typeuse: node.decltype.into(),
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
impl From<syntax::FnDef> for FnDef {

    fn from(node: syntax::FnDef) -> FnDef {
        FnDef{
            name: node.name,
            params: node.params.into_iter().map(Into::into).collect(),
            rettype: node.ret_type.map(Into::into),
            body: node.body.into(),
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
