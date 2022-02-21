#![allow(dead_code)]
#![allow(unused_imports)]
///! fff-lang
///!
///! compiler driver

#[macro_use] extern crate util;
extern crate messages as message;  // legacy remove
extern crate codemap;
extern crate lexical;
extern crate syntax;
extern crate semantic;
// mod vm;

use codemap::SourceMap;
use codemap::SymbolCollection;
use message::MessageCollection;
use lexical::TokenStream;
use syntax::SyntaxTree;
use semantic::Package;
// use vm::VirtualMachine;

fn compile_input_result(file_name: String) -> Result<&'static str, String> {

    let mut messages = MessageCollection::new();
    let mut symbols = SymbolCollection::new();
    let mut sources = SourceMap::new(file_name).map_err(|e| format!("{:?}", e))?;
    let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).map_err(|_| format!("{:?}", messages));
    println!("{:?}", syntax_tree);

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
    
    Ok("Bye bye")
}

pub fn compile_input(file_name: String) {

    match compile_input_result(file_name) {
        Ok(msg) => println!("{}", msg),
        Err(msg) => println!("{}", msg),
    }
}

// TODO: 
// Move codegen out
// Create optimize
// Move vm out
