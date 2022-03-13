#![allow(dead_code)] // 2022: there is really many many dead code, although some is true dead code, most is reserved and even must be used later

use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::lexical::Parser;
use crate::syntax::{parse, Formatter, ISyntaxFormat};
// use semantic::Package;
// use vm::VirtualMachine;

pub fn main() {

    let mut diagnostics = Diagnostics::new();
    let mut scx: SourceContext = SourceContext::new();

    let args = std::env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        let chars = scx.entry(&args[1]);
        match parse(Parser::new(chars, &mut diagnostics)) {
            Ok(tree) => println!("{}", tree.format(Formatter::new(Some(&scx)))),
            Err(_) => println!("{}", diagnostics.display(&scx)),
        }
    } else {
        println!("input file name")
    }

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
