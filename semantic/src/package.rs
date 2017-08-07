///! fff-lang
///!
///! semantic/package, a compilation unit

use std::collections::LinkedList;

use std::rc::Rc;
use std::cell::RefCell;

use syntax;

use super::Statement;
use super::ISemanticAnalyze;
use super::DefScope;
use super::SharedDefScope;
use super::Module;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    modules: Vec<Module>,
    global_scope: SharedDefScope,
}
impl Package {
    
    pub fn new(tree: syntax::SyntaxTree) -> Package {
        let global_scope = Rc::new(RefCell::new(DefScope::new(String::new()))); // yes 4 news
        let modules = tree.modules.into_iter().map(|module| Module::from_syntax(module, global_scope.clone())).collect::<Vec<Module>>();
        Package{ global_scope, modules }
    }
}

// TODO: add scope name, where global is package name, fn main is package name + "::main", fn main for stmt is package name + "::main::<for-stmt<5:5-10:5>>"
// add format method, maybe directly use syntax::ISyntaxFormat and syntax::Formatter