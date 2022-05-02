use std::str::FromStr;

pub enum PrintOption {
    AST,
    Files,
}

impl FromStr for PrintOption {
    type Err = ();
    
    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "files" => Ok(Self::Files),
            "ast" => Ok(Self::AST),
            _ => Err(())
        }
    }
}

pub enum DebugOption {
    Memory,
    AST, // not pretty
}

impl FromStr for DebugOption {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        match s {
            "memory" => Ok(Self::Memory),
            "ast" => Ok(Self::AST),
            _ => Err(()),
        }
    }
}

pub struct Argument {
    pub entry: String,
    pub prints: Vec<PrintOption>,
    pub debugs: Vec<DebugOption>,
}

fn print_help(binary_name: &str) -> ! {
    println!("Usage:");
    println!("    {binary_name} [OPTIONS] INPUT");
    println!();
    println!("Options:");
    println!("    -h, --help       Display this message");
    println!("    --print files    Print included files");
    println!("    --print ast      Print ast pretty");
    println!("    --debug OPTIONS  Debug options");
    println!("    -V, --version    Print version and exit");
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

    pub fn new() -> Self {
        Self::new_with(std::env::args())
    }

    pub fn new_with(mut args: impl Iterator<Item = String>) -> Self {
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
            debugs: Vec::new(),
        };

        let mut expect_value_for: Option<&'static str> = None;
        for raw in raws {
            if raw.starts_with("-") { // switch or option
                if expect_value_for.is_some() {
                    print_error_help(format!("unexpected switch {raw}"), &binary_name);
                } else if raw == "--print" {
                    expect_value_for = Some("print");
                } else if raw == "--debug" {
                    expect_value_for = Some("debug");
                } else {
                    print_error_help(format!("unknown switch {raw}"), &binary_name);
                }
            } else if expect_value_for == Some("print") {
                expect_value_for = None;
                for raw in raw.split(',') {
                    if let Ok(option) = raw.parse() {
                        result.prints.push(option);
                    } else {
                        print_error_help(format!("unknown print value {raw}"), &binary_name);
                    }
                }
            } else if expect_value_for == Some("debug") {
                expect_value_for = None;
                for raw in raw.split(',') {
                    if let Ok(option) = raw.parse() {
                        result.debugs.push(option);
                    } else {
                        print_error_help(format!("unknown debug value {raw}"), &binary_name);
                    }
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
