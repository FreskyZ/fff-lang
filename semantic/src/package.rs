///! fff-lang
///!
///! semantic/package, a compilation unit

use std::rc::Rc;
use std::cell::RefCell;

use syntax;

use super::FromSyntax;
use super::Statement;
use super::ISemanticAnalyze;
use super::DefScope;
use super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub items: Vec<Statement>,
    this_scope: SharedDefScope,
}
impl Package {
    pub fn from_syntax(root: syntax::SyntaxTree) -> Package {
        let this_scope = Rc::new(RefCell::new(DefScope::new()));
        Package{
            items: root.items.into_iter().map(|item| Statement::from_syntax(item, this_scope.clone())).collect(),
            this_scope,
        }
    }
}
impl ISemanticAnalyze for Package {

    fn collect_type_declarations(&mut self) {
    }
}

#[cfg(test)] #[test]
fn generatal_manual_from_syntax() {
    use syntax::WithTestInput;

    eprintln!("error something");
    let input = syntax::SyntaxTree::with_test_str(r#"
type a {
    a: i32, 
    b: u32, 
    c: string
} 
fn main() { 
    if sys.args().len() > 1 { 
        prinln("helloworld"); 
    }
}"#);
    println!("input: {:?}", input);
    println!("output: {:?}", Package::from_syntax(input));
    // panic!("no reason")
}

// TODO: add scope name, where global is package name, fn main is package name + "::main", fn main for stmt is package name + "::main::<for-stmt<5:5-10:5>>"
// add format method, maybe directly use syntax::ISyntaxFormat and syntax::Formatter