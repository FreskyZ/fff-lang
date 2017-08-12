///! fff-lang
///!
///! semantic/traits

use std::fmt;

use codemap::Span;
use codemap::SymbolID;
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

/// Parameter for `ISyntaxAnalyze::from_syntax`
///
/// stores ref to symbols, to format namable scope like fn and type, 
/// stores ref to source code, to format unnamable scope with span like for and if
pub struct FromSession<'a, 'b> {
    scope: SharedDefScope,
    source: &'a SourceCode,
    symbols: &'b SymbolCollection,
}
impl<'a, 'b> FromSession<'a, 'b> {

    pub fn new(scope: SharedDefScope, source: &'a SourceCode, symbols: &'b SymbolCollection) -> Self {
        FromSession{ scope, source, symbols }
    }
    pub fn clone_scope(&self) -> Self {
        FromSession{ scope: self.scope.clone(), source: self.source, symbols: self.symbols }
    }

    /// Create sub scope with id for name, 
    /// May panic on invalid ID
    pub fn sub_with_symbol(&self, id: SymbolID) -> Self {
        FromSession{ scope: self.scope.sub(self.symbols.get(id).unwrap()), source: self.source, symbols: self.symbols }
    } 
    /// Create sub scope with not unique name like 'if', 'for', etc.
    /// which need adding span information to make unique
    pub fn sub_with_span(&self, name: &str, span: Span) -> Self {
        FromSession{ scope: self.scope.sub(format!("<{}{}>", name, span.format(Some(self.source)))), source: self.source, symbols: self.symbols }
    }

    pub fn into_scope(self) -> SharedDefScope { self.scope }
}

pub trait ISemanticAnalyze {

    // Display, which is actually Debug but I don't like `fn debug() -> String`, should not be implemented
    fn display<'a>(&'a self) -> Wrapper<'a, Self> where Self: Sized { Wrapper(self) }
    // format, should be implemented
    fn format(&self, f: Formatter) -> String;

    // phase 1: direct map from syntax node
    type SyntaxItem;
    fn from_syntax(item: Self::SyntaxItem, sess: FromSession) -> Self;

    // TODO: an empty implement for compatibility temporarily, remove it in future
    fn collect_type_declarations(&mut self) { }
}
