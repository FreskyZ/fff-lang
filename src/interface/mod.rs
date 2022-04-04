
use std::collections::VecDeque;
use std::io;
use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::syntax::{Node, parse};
// use crate::mast::Tree;
// use crate::ir::Graph;
// use crate::vm::{VirtualMachine, CodeGenerator};

mod argument;

fn run_compiler(args: argument::Argument, output: &mut impl io::Write) {

    let mut ecx = Diagnostics::new();
    let mut scx: SourceContext = SourceContext::new();

    let mut modules = Vec::new();
    let mut requests = VecDeque::new();
    scx.entry(&args.entry, &mut ecx)
        .and_then(|main_source| parse(main_source, &mut ecx)).map(|main_module| {
            requests.extend(main_module.imports());
            modules.push(main_module);
            while !requests.is_empty() {
                let request = &requests[0];
                scx.import(request.all_span, request.name, request.path.map(|(id, _)| id), &mut ecx)
                    .and_then(|source| parse(source, &mut ecx)).map(|module| {
                        requests.extend(module.imports());
                        modules.push(module);
                    });
                requests.pop_front();
            }
        });

    write!(output, "{}", ecx.display(&scx)).unwrap();
    if args.prints.iter().any(|p| *p == argument::PrintValue::AST) {
        for module in &modules {
            write!(output, "{}", module.display(&scx)).unwrap();
        }
    }
    if args.prints.iter().any(|p| *p == argument::PrintValue::ASTMemory) {
        for module in &modules {
            // TODO write!(output, "{}", module.memory_snapshot()); // not really snapshot, because ast is immutable
            write!(output, "{}", module.display(&scx)).unwrap();
        }
    }
    if args.prints.iter().any(|p| *p == argument::PrintValue::Files) {
        for module in &modules {
            writeln!(output, "{}", scx.get_relative_path(module.file).display()).unwrap();
        }
    }

    // let tcx = TypeContext::new();
    // let tree = mast::from(modules, &mut tcx);
    // if args.print_tree { println!("{}", tree.display(&scx, &tcx)); }

    // let functions = ir::from(tree, &mut tcx);
    // if args.print_ir { println!("{}", functions.display(&scx, &tcx)) }

    // // the first version of new vm directly executes cfg
    // VirtualMachine::new(&tcx, &functions).execute()
}

pub fn test_main() {
    use std::fmt::Write;
    // this very first version list .f3 files in tests/ast and run with --print ast and compare with .stdout
    // TODO: collect cases and send to thread pool

    // it's ok to hardcode solution dir because ct will only run by cargo rt
    let test_dir = [env!("CARGO_MANIFEST_DIR"), "tests", "ast"].into_iter().collect::<std::path::PathBuf>();

    let mut success_count = 0;
    let mut fail_cases = Vec::new();
    for entry in std::fs::read_dir(test_dir).expect("cannot read tests dir") {
        if let Ok(entry) = entry {
            let path = entry.path();
            if matches!(path.extension(), Some(extension) if extension == "f3") {
                let expect_path = { let mut p = path.clone(); p.set_extension("stdout"); p };
                let expect_output = std::fs::read_to_string(&expect_path).expect(&format!("failed to read {}", expect_path.display()));
                let mut actual_output = Vec::<u8>::with_capacity(8192);
                let args = argument::Argument{ entry: path.display().to_string(), prints: vec![argument::PrintValue::AST] };
                run_compiler(args, &mut actual_output);
                let actual_output = std::str::from_utf8(&actual_output).expect(&format!("failed to decode stdout {}", path.display()));
                if actual_output != expect_output {
                    let mut buf = format!("{} mismatch\n", path.display());
                    let (actual_lines, expect_lines) = (actual_output.lines().collect::<Vec<_>>(), expect_output.lines().collect::<Vec<_>>());
                    let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
                    for line in 0..common_line_count {
                        if actual_lines[line] != expect_lines[line] {
                            writeln!(buf, "{: >3} |A {}", line + 1, actual_lines[line]).unwrap();
                            writeln!(buf, "    |E {}", expect_lines[line]).unwrap();
                        } else {
                            writeln!(buf, "{: >3} |  {}", line + 1, actual_lines[line]).unwrap();
                        }
                    }
                    if actual_lines.len() > common_line_count {
                        for line in common_line_count..actual_lines.len() {
                            writeln!(buf, "{: >3} |A {}", line + 1, actual_lines[line]).unwrap();
                        }
                    }
                    if expect_lines.len() > common_line_count {
                        for line in common_line_count..expect_lines.len() {
                            writeln!(buf, "{: >3} |E {}", line + 1, expect_lines[line]).unwrap();
                        }
                    }
                    println!("{}", buf);
                    fail_cases.push(path.clone());
                } else {
                    success_count += 1;
                }
            }
        }
    }

    println!("ct complete, pass {}/{}", success_count, success_count + fail_cases.len());
    for fail_case in &fail_cases {
        println!("fail {}", fail_case.display());
    }
    std::process::exit(if fail_cases.is_empty() { 0 } else { 1 })
}

pub fn cli_main() {
    let stdout = io::stdout();
    let mut stdout_lock = stdout.lock();
    run_compiler(argument::Argument::new(), &mut stdout_lock)
}
