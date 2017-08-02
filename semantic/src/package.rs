///! fff-lang
///!
///! semantic/package, a compilation unit

use std::collections::LinkedList;

use std::rc::Rc;
use std::cell::RefCell;

use syntax;

use super::FromSyntax;
use super::Statement;
use super::ISemanticAnalyze;
use super::DefScope;
use super::SharedDefScope;
use super::Module;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    main_module: Vec<Module>,
    global_scope: SharedDefScope,
}
impl Package {
    
    pub fn from_syntax(root: syntax::SyntaxTree) -> Package {
        let global_scope = Rc::new(RefCell::new(DefScope::new(String::new()))); // yes 4 news
        let modules = root.modules.into_iter().map(|module| FromSyntax::from_syntax(module, global_scope.clone())).collect::<Vec<Module>>();
        Package{ global_scope, main_module: modules }
    }
}
impl ISemanticAnalyze for Package {
    fn collect_type_declarations(&mut self) {
    }
}

// TODO: add scope name, where global is package name, fn main is package name + "::main", fn main for stmt is package name + "::main::<for-stmt<5:5-10:5>>"
// add format method, maybe directly use syntax::ISyntaxFormat and syntax::Formatter