
use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::lexical::Parser;
use crate::syntax::{parse, Node};
// use crate::analysis::Program;
// use crate::vm::{VirtualMachine, CodeGenerator};

pub fn main() {

    let mut diagnostics = Diagnostics::new();
    let mut scx: SourceContext = SourceContext::new();

    let args = std::env::args().collect::<Vec<_>>();
    if args.len() > 1 {
        let chars = scx.entry(&args[1]);
        match parse(Parser::new(chars, &mut diagnostics)) {
            Ok(tree) => println!("{}", tree.display(&scx)),
            Err(_) => println!("{}", diagnostics.display(&scx)),
        }
    } else {
        println!("input file name")
    }

    // let program = Program::new(modules)?;
    // let code = CodeGenerator::new(program)?;
    // VirtualMachine::new(code).execute()
}
