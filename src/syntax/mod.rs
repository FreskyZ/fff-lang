///! syntax: syntax parse

pub mod ast;
mod visit;
mod visitee;
mod pretty;
mod parser;
#[cfg(test)]
mod tests;
pub use visit::{Node, Visitor};
use parser::Unexpected;
#[cfg(test)]
pub use parser::Parser;
#[cfg(not(test))]
use parser::Parser;

// `'impl' name` not `'impl' name_segment` ??
impl ast::Module {

    pub fn imports(&self) -> Vec<ast::ModuleStatement> {
        self.items.iter().filter_map(|item| match item {
            ast::Item::Import(module_stmt) => Some(module_stmt.clone()),
            _ => None,
        }).collect()
    }
}

// parse any types of node for test
pub fn parse_any<T>(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics, f: impl FnOnce(&mut Parser) -> Result<T, Unexpected>) -> Option<T> {
    let mut parser = Parser::new(crate::lexical::Parser::new(source, diagnostics));
    let result = f(&mut parser).ok();
    parser.finish();
    result
}
// formal public api only parses module
pub fn parse(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<ast::Module> {
    parse_any(source, diagnostics, |cx| cx.parse_module())
}

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! make_node {
    ($parser:ident $code:literal) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        match parse_any(scx.entry("1"), &mut ecx, |cx| cx.$parser()) {
            Ok(node) => { assert_eq!(ecx, crate::diagnostics::make_errors!()); node },
            Err(_) => { panic!("{}", ecx.display(&scx)) },
        }
    }};
}
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use make_node;
