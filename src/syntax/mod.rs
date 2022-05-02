
pub mod ast;

mod visit;
pub use visit::{Visit, Visitor};

mod parser;
use parser::Unexpected;
#[cfg(not(test))]
use parser::Parser;
#[cfg(test)]
pub use parser::Parser;

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
    ($parser:ident($code:literal, $arena:expr)) => {{
        let mut diagnostics = crate::diagnostics::make_errors!();
        let mut sources = crate::source::make_source!($code);
        match parse_any(sources.entry("1"), &mut diagnostics, |cx| cx.$parser($arena)) {
            Ok(node) => { assert_eq!(diagnostics, crate::diagnostics::make_errors!()); node },
            Err(_) => { panic!("{}", diagnostics.display(&sources)) },
        }
    }};
}
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use make_node;
