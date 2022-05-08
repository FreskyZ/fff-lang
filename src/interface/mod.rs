
use std::collections::VecDeque;
use std::io;
use crate::common::arena::Arena;
use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::syntax::{Visit as SyntaxVisit, parse, ast::asti};
use crate::semantic::{resolve, /* Visit as SemanticVisit */};
use crate::middle::{build, fur::TypeContext};
// use crate::vm::VirtualMachine;

mod argument;

fn run_compiler(args: argument::Argument, output: &mut impl io::Write) {

    let mut diagnostics = Diagnostics::new();
    let mut source: SourceContext = SourceContext::new();

    let arena = Arena::new();
    let mut modules = Vec::new();
    let mut requests = VecDeque::new();
    source.entry(&args.entry, &mut diagnostics)
        .and_then(|main_source| parse(main_source, &mut diagnostics, &arena))
        .map(|main_module| {
            requests.extend(main_module.imports(&arena));
            modules.push(main_module);
            while !requests.is_empty() {
                let request = &requests[0];
                source.import(request.span, request.name.id, request.path.map(|path| path.id), &mut diagnostics)
                    .and_then(|source| parse(source, &mut diagnostics, &arena)).map(|module| {
                        requests.extend(module.imports(&arena));
                        modules.push(module);
                    });
                requests.pop_front();
            }
        });

    // write!(output, "{}", arena.status(false)).unwrap();
    write!(output, "{}", diagnostics.display(&source)).unwrap();

    for debug_option in &args.debugs {
        match debug_option {
            argument::DebugOption::AST => {
                for module in &modules {
                    format!("{:?}", asti::debug(module, &arena));
                    write!(output, "{:?}", asti::debug(module, &arena)).unwrap();
                }  
            },
            argument::DebugOption::Memory => {
                let mut profiler = asti::MemoryProfiler::new();
                for module in &modules {
                    module.accept(&arena, &mut profiler);
                }
                profiler.dump(output);
            },
        }
    }
    for print_option in &args.prints {
        match print_option {
            argument::PrintOption::Files => {
                for module in &modules {
                    writeln!(output, "{}", source.get_relative_path(arena.get(*module).file).display()).unwrap();
                }
            },
            argument::PrintOption::AST => {
                for module in &modules {
                    write!(output, "{}", asti::display(module, &source, &arena)).unwrap();
                }
            }
        }
    }

    let program = resolve(modules, &mut diagnostics, &arena);
    // if print semantic tree { masti::display }

    let mut tcx = TypeContext;
    let _program = build(&mut tcx, program, &mut diagnostics, &arena, /* &graph_arena */);
    // if print fur { furi::display }

    // if debug syntax arena { arena.status(true) }
    std::mem::drop(arena);

    // // the first version of new vm directly executes cfg
    // VirtualMachine::new(tcx, program).execute()

    // transform(&mut program, config)
    // if args.interpret {
    //     let code = vm::generate(&tcx, &program);
    //     VirtualMachine::new(code).execute()
    // } else {
    //     let code = native::generate(&tcx, &program);
    //     // there is literally years of work after vfs.read before vfs.write
    //     fs.write(assemble(code));
    // }
}

pub fn test_main() {
    use std::fmt::Write;
    // this very first version list .f3 files in tests/ast and run with --print ast and compare with .stdout
    // TODO: collect cases and send to thread pool

    let filter = std::env::args().nth(1);
    // it's ok to hardcode solution dir because ct will only run by cargo rt
    let test_dir = [env!("CARGO_MANIFEST_DIR"), "tests", "ast"].into_iter().collect::<std::path::PathBuf>();

    let mut success_count = 0;
    let mut ignore_count = 0;
    let mut fail_cases = Vec::new();
    for entry in std::fs::read_dir(test_dir).expect("cannot read tests dir") {
        if let Ok(entry) = entry {
            let path = entry.path();
            if !matches!(path.extension(), Some(extension) if extension == "f3") {
                continue;
            }
            let path_no = path.display().to_string(); // path not osstr
            if !filter.as_ref().map(|f| path_no.contains(f)).unwrap_or(true) {
                ignore_count += 1;
                continue;
            }
            let expect_path = { let mut p = path.clone(); p.set_extension("stdout"); p };
            let expect_output = std::fs::read_to_string(&expect_path).expect(&format!("failed to read {}", expect_path.display()));
            let mut actual_output = Vec::<u8>::with_capacity(8192);
            let args = argument::Argument{ entry: path_no.clone(), prints: vec![argument::PrintOption::AST], debugs: vec![argument::DebugOption::Memory] };
            run_compiler(args, &mut actual_output);
            let actual_output = std::str::from_utf8(&actual_output).expect(&format!("failed to decode stdout {}", path_no));
            if actual_output != expect_output {
                let mut buf = format!("{} mismatch\n", path_no);
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

    println!("ct complete, pass {}/{} ignore {}", success_count, success_count + fail_cases.len(), ignore_count);
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
