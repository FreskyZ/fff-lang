
pub mod ast;

mod visit;
pub use visit::{Visit, Visitor};

mod parser;
use parser::{Parser, Unexpected};

// parse any types of node for test
pub fn parse_any<'scx, 'ecx, 'a, T>(
    source: crate::source::SourceChars<'scx>, 
    diagnostics: &'ecx mut crate::diagnostics::Diagnostics,
    arena: &'a crate::common::arena::Arena,
    f: impl Fn(&mut Parser<'ecx, 'scx, 'a>) -> Result<crate::common::arena::Index<T>, Unexpected>,
) -> Option<crate::common::arena::Index<T>> {
    let mut parser = Parser::new(crate::lexical::Parser::new(source, diagnostics), arena);
    let result = f(&mut parser).ok();
    parser.finish();
    result
}
// formal public api only parses module
pub fn parse(
    source: crate::source::SourceChars,
    diagnostics: &mut crate::diagnostics::Diagnostics,
    arena: &crate::common::arena::Arena,
) -> Option<crate::common::arena::Index<ast::Module>> {
    parse_any(source, diagnostics, arena, Parser::parse_module)
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
