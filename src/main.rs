#![allow(dead_code)]

#[macro_use]
extern crate fsz_common;

mod position;
mod config;
mod driver;
mod lexical;
mod syntax;
mod interpret;

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
            driver::compile_file(file_name);
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
        use fsz_common::dummyx2;

        assert_eq!(4, dummyx2(2));
        perrorln!("Something by perrorln");
    }
}