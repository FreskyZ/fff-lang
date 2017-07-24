///! fff-lang
///!
///! semantic/traits

use std::rc::Rc;
use std::cell::RefCell;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct DefScope {

    // fn add_type_decl(&mut self, name: String);
    // fn add_type_def(&mut self /* typedef: TypeDef */);
    // fn get_type(&self, name: String) /* -> &TypeDef */;

    // fn add_variable_def(&mut self /* var: VarDef */);
    // fn get_variable(&self, name: String) /* -> &VarDef */;

    pub parent: Option<SharedDefScope>,
}
impl DefScope {
    pub fn new() -> DefScope {
        DefScope{
            parent: None,
        }
    }
    pub fn with_parent(parent: SharedDefScope) -> SharedDefScope {
        DefScope{
            parent: Some(parent),
        }
    }
}

pub type SharedDefScope = Rc<RefCell<DefScope>>;

pub trait FromSyntax<T> {
    fn from_syntax(item: T, parent_scope: SharedDefScope) -> Self;
}

pub trait ISemanticAnalyze {

    fn collect_type_declarations(&mut self);
}
