#![cfg_attr(not(test), allow(dead_code))]

// mod util;
mod source;
mod diagnostics;
mod lexical;
mod syntax;
mod driver;
// mod semantic;

fn main() {
    driver::main();
}
