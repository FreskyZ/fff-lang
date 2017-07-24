///! fff-lang
///!
///! semantic/package, a compilation unit

use std::rc::Rc;
use std::cell::RefCell;

use syntax;

use super::FromSyntax;
use super::Statement;
use super::ISemanticAnalyze;
use super::SharedDefScope;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub items: Vec<Statement>,
    this_scope: SharedDefScope,
}
impl Package {
    fn from_syntax(root: syntax::SyntaxTree) -> Package {
        let this_scope = Rc::new(RefCell::new(DefScope::new()));
        Package{
            items: root.items.into_iter().map(|item| Statement::from_syntax(item, this_scope.clone())).collect(),
            this_scope,
        }
    }
}
impl ISemanticAnalyze for Package {

    fn collect_type_declarations(&mut self) {
        // for (index, item) in self.items.iter().enumerate() {
        //     match item {
        //         &Statement::Type(typedef) => 
        //     }
        // }
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