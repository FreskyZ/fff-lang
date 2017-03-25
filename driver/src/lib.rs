#![allow(dead_code)]
#![allow(unused_imports)]

///! fff-lang
///! compiler driver

#[macro_use] extern crate util;
#[macro_use] extern crate codepos; 
extern crate messages as message;  // TODO: this is for legacy compatibility, remove it
extern crate codemap;
extern crate lexical;
extern crate syntax;

mod codegen;
mod vm;

use message::MessageCollection;
use codemap::CodeMap;
use lexical::TokenStream;
use syntax::SyntaxTree;
use codegen::Program;
use vm::VirtualMachine;

// Handle and print error here
pub fn compile_input(file_name: String) {

    let mut messages = MessageCollection::new();

    let mut codemap = CodeMap::with_files(vec![file_name], &mut messages);          // read file
    if messages.is_uncontinuable() { println!("{:?}", messages); return; }
    let mut tokens = TokenStream::new(codemap.iter(), &mut messages);               // Lexical parse
    if messages.is_uncontinuable() { println!("{:?}", messages); return; }          // although it will not happen currently
    let syntax_tree = SyntaxTree::new(&mut tokens, &mut messages);                  // Syntax parse
    if messages.is_uncontinuable() { println!("{:?}", messages); return; }    
    let program = Program::new(syntax_tree, &mut messages);                         // Semantic parse
    if !messages.is_empty() { println!("{:?}", messages); return; }    

    let mut virtual_machine = VirtualMachine::new(program);
    virtual_machine.execute(&mut messages);                                         // run!
    if !messages.is_empty() { println!("{:?}", messages); return; }
    
    println!("Byebye");
}

// TODO: 
// Move syntax out
// Move codegen out
// Create optimize
// Move vm out
// Move main out and change this to be a lib