
use std::collections::VecDeque;
use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::lexical::Parser;
use crate::syntax::parse;
// use crate::analysis::Program;
// use crate::vm::{VirtualMachine, CodeGenerator};

pub fn main() {

    let mut diagnostics = Diagnostics::new();
    let mut scx: SourceContext = SourceContext::new();

    let args = std::env::args().collect::<Vec<_>>();
    if args.is_empty() {
        println!("ffc main.f3");
        return;
    } else {
        if let Some(main_module) = parse(Parser::new(scx.entry(&args[1]), &mut diagnostics)) {
            
            let mut import_requests = VecDeque::new();
            import_requests.extend(main_module.collect_imports());
            let mut modules = vec![main_module];
            while !import_requests.is_empty() {
                if let Some(chars) = scx.import(import_requests[0].0, import_requests[0].1) {
                    if let Some(module) = parse(Parser::new(chars, &mut diagnostics)) {
                        import_requests.extend(module.collect_imports());
                        modules.push(module);
                        import_requests.pop_front();
                    } else {
                        println!("{}", diagnostics.display(&scx));
                        println!("failed to parse {} requested at {}", import_requests[0].1.display(&scx), import_requests[0].0.display(&scx));
                        return;
                    }
                } else {
                    println!("{}", diagnostics.display(&scx));
                    println!("failed to open {} requested at {}", import_requests[0].1.display(&scx), import_requests[0].0.display(&scx));
                    return;
                }
            }
            for module in &modules {
                println!("included {}", scx.get_relative_path(module.file).display());
            }
            println!("{}", diagnostics.display(&scx));
        } else {
            println!("{}", diagnostics.display(&scx));
        }
    }

    // let program = Program::new(modules)?;
    // let code = CodeGenerator::new(program)?;
    // VirtualMachine::new(code).execute()
}
