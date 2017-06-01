///! fff-lang
///!
///! semantic/scope
///! A scope is a package or namespace or typedef or fndef or blockstmt or ifstmt or forstmt or whilestmt
///! which is a scope for definitions

use super::super::DefID;
use super::TypeDef;
use super::FnDef;
use super::VarDef;

pub trait IDefinition {
}
pub trait IScopeDef {

    fn get_defs(&self) -> Vec<&IDefinition>;
    fn get_defs_mut(&mut self) -> Vec<&mut IDefinition>;
}