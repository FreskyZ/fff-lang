///! fff-lang
///!
///! semantic/def_scope

//use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct DefScope {
    pub name: String, // also definition prefix
    pub parent: Option<SharedDefScope>,
}
impl DefScope {
    pub fn new(name: String) -> DefScope {
        DefScope{
            name: name,
            parent: None,
        }
    }
    pub fn with_parent(ext_name: String, parent: SharedDefScope) -> SharedDefScope {
        let new_name = format!("{}::{}", parent.as_ref().borrow().name.clone(), ext_name);
        Rc::new(RefCell::new(DefScope{
            name: new_name,
            parent: Some(parent),
        }))
    }
}

pub type SharedDefScope = Rc<RefCell<DefScope>>;

pub trait FromSyntax<T> {
    fn from_syntax(item: T, parent_scope: SharedDefScope) -> Self;
}
