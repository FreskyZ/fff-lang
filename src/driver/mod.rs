#![allow(dead_code)] // 2022: there is really many many dead code, although some is true dead code, most is reserved and even must be used later

use crate::source::SourceContext;
use crate::diagnostics::MessageCollection;
use crate::lexical::{TokenStream, Token};
// use crate::syntax::SyntaxTree;
// use semantic::Package;
// use vm::VirtualMachine;

pub fn main() {

    let mut scx: SourceContext = SourceContext::new();

    let mut messages = MessageCollection::new();
    let tokens = TokenStream::new(scx.entry("tests/syntax/inter/misc_src.ff"), &mut messages);
    for i in 0.. {
        if let Token::EOF = tokens.nth_token(i) {
            break;
        } else {
            println!("{}: {}", tokens.nth_span(i).display(&scx), tokens.nth_token(i).display(&scx));
        }
    }

    // let syntax_tree = SyntaxTree::new(&mut sources, &mut messages, &mut symbols).map_err(|_| format!("{:?}", messages));
    // println!("{:?}", syntax_tree);

    // let package = Package::from(syntax_tree)?;
    // let machine = VirtualMachine::new(package)?;
    // let result = machine.execute()?;
}
