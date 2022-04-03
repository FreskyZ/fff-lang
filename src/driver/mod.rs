
use std::collections::VecDeque;
use crate::source::SourceContext;
use crate::diagnostics::Diagnostics;
use crate::syntax::{Node, parse};
// use crate::analysis::Program;
// use crate::vm::{VirtualMachine, CodeGenerator};

#[derive(PartialEq)]
enum PrintValue {
    Asts,
    Files,
}

struct Argument {
    entry: String,
    prints: Vec<PrintValue>,
}

fn print_help(binary_name: &str) -> ! {
    println!("Usage:");
    println!("    {binary_name} [OPTIONS] INPUT");
    println!();
    println!("Options:");
    println!("    -h, --help     Display this message");
    println!("    --print ast    Print ast pretty");
    println!("    --print files  Print included files");
    println!("    -V, --version  Print version and exit");
    println!();
    std::process::exit(0)
}

fn print_error_help(error: String, binary_name: &str) -> ! {
    println!("error: {error}");
    println!();
    println!("Usage:");
    println!("    {binary_name} [OPTIONS] INPUT");
    println!();
    println!("For more information try --help");
    println!();
    std::process::exit(0)
}

fn print_version(binary_name: &str) -> ! {
    println!("{} {}", binary_name, env!("CARGO_PKG_VERSION"));
    println!();
    std::process::exit(0)
}

impl Argument {

    // not result: help if empty, panic if failed to parse, for now
    fn new() -> Argument {
        let mut args = std::env::args();

        // runtime command line binary name, use in help
        let binary_name = args.next().expect("args are empty");
        let raws = args.collect::<Vec<_>>();

        if raws.is_empty() || raws.iter().any(|r| r == "-h" || r == "--help") {
            print_help(&binary_name);
        }
        if raws.iter().any(|r| r == "-V" || r == "--version") {
            print_version(&binary_name);
        }

        let mut result = Argument{
            entry: String::new(),
            prints: Vec::new(),
        };

        let mut expect_value_for: Option<&'static str> = None;
        for raw in raws {
            if raw.starts_with("-") { // switch or option
                if expect_value_for.is_some() {
                    print_error_help(format!("unexpected switch {raw}"), &binary_name);
                } else if raw == "--print" {
                    expect_value_for = Some("print");
                } else {
                    print_error_help(format!("unknown switch {raw}"), &binary_name);
                }
            } else if expect_value_for == Some("print") {
                expect_value_for = None;
                if raw == "ast" {
                    result.prints.push(PrintValue::Asts);
                } else if raw == "files" {
                    result.prints.push(PrintValue::Files);
                } else {
                    print_error_help(format!("unknown print value {raw}"), &binary_name);
                }
            } else if !result.entry.is_empty() {
                print_error_help("unexpected multiple entry specified".to_owned(), &binary_name);
            } else {
                // // 1. I really don't understand how to let clap "collect 
                // //    all switchs and options and give the remaining plain value list to me", but this one can
                // // 2. this is another level of argument parsing compare to in the handin version,
                // //    I mean, although clap already exist when I start to write rust (before this project),
                // //    I really don't know these infrastructures for each language (structopt, argparse, etc.) at that time
                result.entry = raw.to_owned();
            }
        }
        if let Some(switch) = expect_value_for {
            print_error_help(format!("expect value for {switch}"), &binary_name);
        }
        if result.entry.is_empty() {
            print_error_help("entry required".to_owned(), &binary_name);
        }

        result
    }
}

pub fn main() {

    let args = Argument::new();
    let mut ecx = Diagnostics::new();
    let mut scx: SourceContext = SourceContext::new();

    let mut modules = Vec::new();
    let mut requests = VecDeque::new();
    scx.entry(&args.entry, &mut ecx).and_then(|main_source| parse(main_source, &mut ecx)).map(|main_module| {
        requests.extend(main_module.imports());
        modules.push(main_module);
        while !requests.is_empty() {
            let request = &requests[0];
            scx.import(request.all_span, request.name, request.path.map(|(id, _)| id), &mut ecx).and_then(|source| parse(source, &mut ecx)).map(|module| {
                requests.extend(module.imports());
                modules.push(module);
            });
            requests.pop_front();
        }
    });

    print!("{}", ecx.display(&scx));
    if args.prints.iter().any(|p| *p == PrintValue::Asts) {
        for module in &modules {
            print!("{}", module.display(&scx));
        }
    }
    if args.prints.iter().any(|p| *p == PrintValue::Files) {
        for module in &modules {
            println!("{}", scx.get_relative_path(module.file).display());
        }
    }

    // let program = Program::new(modules)?;
    // let code = CodeGenerator::new(program)?;
    // VirtualMachine::new(code).execute()
}
