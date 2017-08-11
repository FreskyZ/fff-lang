///! fff-lang
///!
///! semantic/traits

use std::fmt;

use codemap::SourceCode;
use codemap::SymbolCollection;

use super::SharedDefScope;
use super::Formatter;

// public because it used in public API
// allowed because this name is designed to used not directly, only like `format!("{}", node.display())`
#[allow(dead_code)]
pub struct Wrapper<'a, T: 'a>(&'a T);
impl<'a, T: ISemanticAnalyze> fmt::Display for Wrapper<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.0.format(Formatter::empty()))
    }
}

// pub struct FromSession {
//     scope: SharedDefScope,
//     source: &'a SourceCode,
//     symbols: &'a SymbolCollection;
// }

pub trait ISemanticAnalyze {

    // Display, which is actually Debug but I don't like `fn debug() -> String`, should not be implemented
    fn display<'a>(&'a self) -> Wrapper<'a, Self> where Self: Sized { Wrapper(self) }
    // format, should be implemented
    // TODO: legacy remove implementation
    fn format(&self, f: Formatter) -> String { let _f = f; "<unknown>".to_owned() }

    // phase 1: direct map from syntax node
    type SyntaxItem;
    // @param symbols: because when contructing scope tree, many nodes (type, fn) need to convert id to string as its path segment, mut ref for future use
    fn from_syntax(item: Self::SyntaxItem, parent_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Self;

    // TODO: an empty implement for compatibility temporarily, remove it in future
    fn collect_type_declarations(&mut self) { }
}
