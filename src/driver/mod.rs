#![allow(dead_code)] // 2022: there is really many many dead code, although some is true dead code, most is reserved and even must be used later

use crate::source::SourceContext;
// use crate::diagnostics::MessageCollection;
// use crate::syntax::SyntaxTree;
// use semantic::Package;
// use vm::VirtualMachine;

pub fn main() {

    let mut scx: SourceContext = SourceContext::new();
    scx.entry("tests/syntax/inter/gcd_src.ff".into());
    println!("{:?}", scx);

    // let mut messages = MessageCollection::new();
    // let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).map_err(|_| format!("{:?}", messages));
    // println!("{:?}", syntax_tree);

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
