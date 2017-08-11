///! fff-lang
///!
///! semantic/def_scope

use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Eq, PartialEq)]
struct DefScope {
    name: String,
    parent: Option<SharedDefScope>,
}

#[derive(Eq, PartialEq, Clone)]
pub struct SharedDefScope(Rc<RefCell<DefScope>>);

impl fmt::Debug for SharedDefScope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { 
        write!(f, "<scope {}>", self.get_full_name())
    }
}
impl SharedDefScope { // new

    fn _new(name: String, parent: Option<SharedDefScope>) -> Self {  
        SharedDefScope(Rc::new(RefCell::new(DefScope{ name, parent })))
    }

    pub fn new<T: Into<String>>(name: T) -> Self { Self::_new(name.into(), None) }
    
    /// Create sub scope
    pub fn sub<T: Into<String>>(&self, name: T) -> Self { Self::_new(name.into(), Some(self.clone())) }
}
impl SharedDefScope { // get

    pub fn get_this_name(&self) -> String { self.0.as_ref().borrow().name.clone() }
    fn get_parent_scope(&self) -> Option<SharedDefScope> { self.0.as_ref().borrow().parent.clone() }

    pub fn get_full_name(&self) -> String {
        match self.get_parent_scope() {
            None => self.get_this_name().to_owned(),
            Some(ref parent_scope) => format!("{}::{}", parent_scope.get_full_name(), self.get_this_name()),
        }
    }
}

#[cfg(test)] #[test]
fn def_scope_usage() {

    macro_rules! test_case { ($scope: expr, $this: expr, $full: expr) => (assert_eq!(($scope.get_this_name(), $scope.get_full_name()), ($this.to_owned(), $full.to_owned())))  }

    let scope1 = SharedDefScope::new("global");
    test_case!(scope1, "global", "global");

    let scope2 = scope1.sub("abc");
    test_case!(scope2, "abc", "global::abc");
    let scope3 = scope2.clone();
    test_case!(scope3, "abc", "global::abc");

    assert_eq!(SharedDefScope::new("").sub("a").sub("b"), SharedDefScope::new("").sub("a").sub("b"));
}
// scope def here is very abstract
//                
//               some-root{ scope, ... }
//              / (1)      / (2) \ (2)
//             |          |       -------_______
//            /          /                      \
//         some-node{ scope, ... }, other-node{ scope, uses, defs ... }, ...
//          | (1)       \ (2)
//          |            \
//        another-node{ scope, uses, defs, ... }, ...
// 
// (1) not very clear relationship
// (2) outer and inner scope
//
// for each def in node, every name definition itself is a simple name, but their full name ('qualified name') is prefixed with their scope's name
// for each use in node, just query this node's scope, either cloned from parent's scope or create this scope on its own
// if not found, then name not declared error
//     some things: 'forget to use' suggestion here
//
//
// in concrete
//
// main module              // <scope>
//     def a                // def a
//     def b                // def b
//     module c             // <scope c>
//         def d            // def c::d
//     modele e             // <scope e>
//         def f            // def e::f
//         def g            // def e::G
//         model h          // <scope e::h>
//             def i        // def e::h::i
//             fn j                     // <scope e::h::j>, def e::h::j
//                 block <<3>100-200>   // <scope e::h::j::<block<<3>100-200>>>
//                     var k            // def e::h::j::<block<<3>100-200>>::k
//                 for <<3>300-400>     // <scope e::h::j::<for<<3>300-400>>>
//                     if <<3>320-360>  // <scope e::h::j::<for<<3>300-400>>::<if<<3>320-360>>>
//                         var l        // def e::h::j::<for<<3>300-400>>::<if<<3>320-360>>::l
