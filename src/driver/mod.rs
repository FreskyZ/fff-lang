#![allow(dead_code)] // 2022: there is really many many dead code, although some is true dead code, most is reserved and even must be used later

use crate::source::SourceContext;
use crate::diagnostics::MessageCollection;
use crate::lexical::Parser;
use crate::syntax::{parse, prelude::Formatter, prelude::ISyntaxFormat};
// use semantic::Package;
// use vm::VirtualMachine;

pub fn main() {

    let mut scx: SourceContext = SourceContext::new();
    let mut messages = MessageCollection::new();

    let args = std::env::args().collect::<Vec<_>>();
    let chars = scx.entry(&args[1]);
    let tree = parse(Parser::new(chars, &mut messages));
    println!("{}", tree.format(Formatter::new(Some(&scx))));

    // let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).map_err(|_| format!("{:?}", messages));
    // println!("{:?}", syntax_tree);

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
