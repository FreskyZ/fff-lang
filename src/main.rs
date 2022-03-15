#![cfg_attr(not(test), allow(dead_code))]

// mod util;
mod source;
mod diagnostics;
mod lexical;
mod syntax;
mod driver;

fn main() {
    driver::main();
}
