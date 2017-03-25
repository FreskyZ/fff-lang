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
use lexical::Lexer;
use syntax::parse;
use codegen::generate;
use vm::run;

// Handle and print error here
pub fn compile_input(file_name: String) {

    let mut messages = MessageCollection::new();
    let mut codemap = CodeMap::with_files(vec![file_name], &mut messages);
    if messages.is_uncontinuable() { println!("{:?}", messages); return; }

    let mut lexer = Lexer::new(codemap.iter());             // Lexical parse
    let ast_program = parse(&mut lexer);                    // Syntax parse
    if !lexer.messages().is_empty() {                       // Any error is not allowed to continue
        println!("{:?}", lexer.messages());
        return;
    }
    let vm_program = generate(ast_program.unwrap()); // Semantic parse
    if !vm_program.msgs.is_empty() {
        println!("{:?}", vm_program.msgs);
        return;
    }

    let maybe_exception = run(vm_program);                         // run!
    match maybe_exception {
        Some(exception) => perrorln!("{:?}", exception),
        None => (),
    }

    println!("Byebye");
}

// TODO: 
// Move syntax out
// Move codegen out
// Create optimize
// Move vm out
// Move main out and change this to be a lib