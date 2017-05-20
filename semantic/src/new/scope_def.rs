///! fff-lang
///!
///! semantic/scope
///! A scope is a package or namespace or typedef or fndef or blockstmt or ifstmt or forstmt or whilestmt
///! which is a scope for definitions

pub type DefID = usize;

pub struct VarDef {
    id: DefID,
    name: String,
    ty: DefID,
}

pub enum Definition {
    Variable(VarDef),
    Function(FnDef),
    Type(TypeDef),
}

pub trait ISemanticScope {

    fn get_defs(&self) -> &Vec<Definition>;
    fn get_defs_mut(&mut self) -> &mut Vec<Definition>;
}