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
    match parse(Parser::new(chars, &mut messages)) {
        Ok(tree) => println!("{}", tree.format(Formatter::new(Some(&scx)))),
        Err(_) => println!("{}", messages.format(Some(&scx))),
    }

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
