///! fff-lang
///!
///! semantic/def_scope

use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use codemap::SymbolID;

use super::DefID;

#[derive(Eq, PartialEq, Copy, Clone)]
pub enum ScopeType {
    Global,
    TypeDef,    // type scope prevent variable search walking through
    FnDef,      // fn scope may prevent variable search walking through (not designed)   
    Statement,
}
impl fmt::Debug for ScopeType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            ScopeType::Global => "global",
            ScopeType::TypeDef => "typedef",
            ScopeType::FnDef => "fndef",
            ScopeType::Statement => "statement",
        })
    }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
struct DefScope {
    name: String,
    scope_type: ScopeType,
    defs: Vec<DefID>,
    parent: Option<SharedDefScope>,
}

#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Clone)]
pub struct SharedDefScope(Rc<RefCell<DefScope>>); // shared_ptr<mutable<DefScode>>

impl fmt::Debug for SharedDefScope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        write!(f, "<scope {:?} {}>", self.get_scope_type(), self.get_full_name())
    }
}
impl SharedDefScope { // new

    fn _new(name: String, scope_type: ScopeType, parent: Option<SharedDefScope>) -> Self {  
        SharedDefScope(Rc::new(RefCell::new(DefScope{ name, scope_type, parent, defs: Vec::new() })))
    }

    pub fn new<T: Into<String>>(name: T, scope_type: ScopeType) -> Self { Self::_new(name.into(), scope_type, None) }
    
    /// Create sub scope
    pub fn sub<T: Into<String>>(&self, name: T, scope_type: ScopeType) -> Self { Self::_new(name.into(), scope_type, Some(self.clone())) }
}
impl SharedDefScope { // get

    pub fn get_this_name(&self) -> String { self.0.as_ref().borrow().name.clone() }
    pub fn get_scope_type(&self) -> ScopeType { self.0.as_ref().borrow().scope_type }
    fn get_parent_scope(&self) -> Option<SharedDefScope> { self.0.as_ref().borrow().parent.clone() }

    pub fn get_full_name(&self) -> String {
        match self.get_parent_scope() {
            None => self.get_this_name().to_owned(),
            Some(ref parent_scope) => format!("{}::{}", parent_scope.get_full_name(), self.get_this_name()),
        }
    }

    pub fn push_definition(&mut self, defid: DefID) {
        self.0.as_ref().borrow_mut().defs.push(defid);
    }
    pub fn search_definition(&self, _name: SymbolID) -> Option<usize> {
        unimplemented!()
    }
}

#[cfg(test)] #[test]
fn def_scope_usage() {

    macro_rules! test_case { ($scope: expr, $this: expr, $full: expr) => (assert_eq!(($scope.get_this_name(), $scope.get_full_name()), ($this.to_owned(), $full.to_owned())))  }

    let scope1 = SharedDefScope::new("global", ScopeType::Global);
    test_case!(scope1, "global", "global");

    let scope2 = scope1.sub("abc", ScopeType::Global);
    test_case!(scope2, "abc", "global::abc");
    let scope3 = scope2.clone();
    test_case!(scope3, "abc", "global::abc");

    assert_eq!(
        SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("b", ScopeType::Global), 
        SharedDefScope::new("", ScopeType::Global).sub("a", ScopeType::Global).sub("b", ScopeType::Global));
}
