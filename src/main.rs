#![allow(dead_code)]
#![allow(unused_assignments)]
#![allow(unused_imports)]
#![allow(unused_variables)]

#[macro_use]
extern crate fsz_common;

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

    smc [inputfile]
";
const VERSION_STRING: &'static str = "Fresky's SmallC compiler v0.1.0";

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

#[cfg(test)]
mod tests {
    
    #[test]
    #[ignore]
    fn sometest() {
        
    }
}

// TODOs:
// make v0lexer's buf to be Chars not string, pass v2_base's Chinese identifier test
// make a new ILexer interface for V4Lexer
// make ast_items to be in syntax module, not ast_item module
// numeric literal accept ' as seperator, not used together, full test of numeric literals
// codegens