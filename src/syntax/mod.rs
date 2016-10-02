
// Syntax, form syntax tree

use message::MessageEmitter;
// use lexical::Token;
use lexical::Lexer;

pub struct AST {}

pub fn get_ast(lexer: &mut Lexer, messages: &mut MessageEmitter) -> AST {

    loop {
        match lexer.next(messages) {
            Some(t) => perrorln!("{:?}", t),
            None => break,
        }
    }

    AST{}
}

