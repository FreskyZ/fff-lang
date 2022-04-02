///! syntax: syntax parse

pub mod ast;
mod visit;
mod visitee;
mod pretty;
mod parser;
#[cfg(test)]
mod tests;

pub use parser::Parser;
pub use visit::{Node, Visitor};

// may be abuse of visit but seems cool
struct CollectImportVisitor {
    requests: Vec<(crate::source::Span, crate::source::IsId)>,
}
impl Visitor for CollectImportVisitor {
    fn visit_module_stmt(&mut self, node: &ast::ModuleStatement) -> Result<(), ()> {
        self.requests.push((node.all_span, node.path.map(|(path, _)| path).unwrap_or(node.name)));
        Ok(())
    }
}

// `'impl' name` not `'impl' name_segment` ??
impl ast::Module {
    pub fn collect_imports(&self) -> Vec<(crate::source::Span, crate::source::IsId)> {
        let mut collector = CollectImportVisitor{ requests: Vec::new() };
        self.accept(&mut collector).unwrap();
        collector.requests
    }
}

// parse any types of node for test
pub fn parse_any<P: Parser>(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<P::Output> {
    let parser = crate::lexical::Parser::new(source, diagnostics);
    let mut context = parser::ParseContext::new(parser);
    let result = P::parse(&mut context).ok();
    context.finish();
    result
}
// formal public api only parses module
pub fn parse(source: crate::source::SourceChars, diagnostics: &mut crate::diagnostics::Diagnostics) -> Option<ast::Module> {
    parse_any::<ast::Module>(source, diagnostics)
}

#[cfg(test)]
#[allow(unused_macros)]
macro_rules! make_node {
    ($code:literal as $ty:ty) => {{
        let mut ecx = crate::diagnostics::make_errors!();
        let mut scx = crate::source::make_source!($code);
        match parse_any::<_, $ty>(scx.entry("1"), &mut ecx) {
            Ok(node) => { assert_eq!(ecx, crate::diagnostics::make_errors!()); node },
            Err(_) => { panic!("{}", ecx.display(&scx)) },
        }
    }};
}
#[cfg(test)]
#[allow(unused_imports)]
pub(crate) use make_node;
