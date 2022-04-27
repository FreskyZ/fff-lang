
pub mod ast;

mod visit;
pub use visit::{Visit, Visitor};

mod parser;
use parser::Unexpected;
#[cfg(not(test))]
use parser::Parser;
#[cfg(test)]
pub use parser::Parser;

impl<'a> crate::common::arena::Index<'a, ast::Module<'a>> {
    pub fn imports(&self, arena: &'a crate::common::arena::Arena) -> Vec<ast::ModuleStatement> {
        arena.get_iter(&arena.get(self).items).filter_map(|item| match item.as_repr().as_ref() {
            ast::Item::Import(module_stmt) => Some(arena.get(module_stmt).clone()),
            _ => None,
        }).collect()
    }
}

// parse any types of node for test
pub fn parse_any<'a, T>(
    source: crate::source::SourceChars, 
    diagnostics: &mut crate::diagnostics::Diagnostics, 
    f: impl FnOnce(&mut Parser) -> Result<crate::common::arena::Index<'a, T>, Unexpected>,
) -> Option<crate::common::arena::Index<'a, T>> {
    let mut parser = Parser::new(crate::lexical::Parser::new(source, diagnostics));
    let result = f(&mut parser).ok();
    parser.finish();
    result
}
// formal public api only parses module
pub fn parse<'a>(
    source: crate::source::SourceChars,
    diagnostics: &mut crate::diagnostics::Diagnostics,
    arena: &'a crate::common::arena::Arena,
) -> Option<crate::common::arena::Index<'a, ast::Module<'a>>> {
    parse_any(source, diagnostics, |cx| cx.parse_module(arena))
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
