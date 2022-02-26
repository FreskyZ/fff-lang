#![allow(dead_code)] // 2022: there is really many many dead code, although some is true dead code, most is reserved and even must be used later

use crate::codemap::SourceMap;
use crate::codemap::SymbolCollection;
use crate::diagnostics::MessageCollection;
use crate::syntax::SyntaxTree;
// use semantic::Package;
// use vm::VirtualMachine;

pub fn main() {

    let file_name = "tests/syntax/inter/gcd_src.ff";
    let mut messages = MessageCollection::new();
    let mut symbols = SymbolCollection::new();
    let mut sources = SourceMap::new(file_name).map_err(|e| format!("{:?}", e)).unwrap();
    let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).map_err(|_| format!("{:?}", messages));
    println!("{:?}", syntax_tree);

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
