#![allow(dead_code)]
#![allow(unused_imports)]

#[macro_use]
extern crate lexical_pos;
extern crate new_lexical;
extern crate codemap;

#[macro_use]
mod common;
mod message;
mod config;
mod file_map;
mod lexical;
mod syntax;
mod codegen;
mod vm;
mod driver;

const USAGE_STRING: &'static str = r"
Usage: 

    ffc [inputfile]
";
const VERSION_STRING: &'static str = "fff-lang compiler v0.1.1";

fn print_usage() {
    println!("{}{}", VERSION_STRING, USAGE_STRING);
}
fn print_version() {
    println!("{}", VERSION_STRING);
}
 
// For feel safe
fn returnable_main() {
    use std::env::args;
    use config::Config;
    use config::ConfigError;
    use config::CompileFileConfig;

    match Config::from_args(args()) {
        Ok(Config::Help) => print_usage(),
        Ok(Config::Version) => print_version(),
        Err(e @ ConfigError::UnexpectedArgument(..)) => {
            perrorln!("Error: {}", e);
        },
        Ok(Config::CompileFile(CompileFileConfig { file_name })) => {
            driver::compile_input(file_name);
        }
    }
}

fn main() {

    returnable_main();
}

#[test]
fn lexical_lib_available() {
    use new_lexical;

    assert_eq!(new_lexical::add(3, 2), 1);
} 

#[test]
fn codemap_lib_available() {
    use codemap;

    assert_eq!(codemap::sub(3, 5), 8);
}