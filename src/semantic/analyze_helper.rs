///! fff-lang
///!
///! semantic/traits

use std::fmt;
use std::cell::RefCell;
use crate::source::{Span, IsId, SourceContext};
use crate::diagnostics::{Diagnostics};

use super::Formatter;
use super::ScopeType;
use super::Definition;
use super::SharedDefScope;
use super::DefinitionCollection;

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
    pub fn sub_with_symbol(&self, id: SymbolID, scope_type: ScopeType) -> Self {
        FromSession{ scope: self.scope.sub(self.symbols.get(id).unwrap(), scope_type), source: self.source, symbols: self.symbols }
    } 
    /// Create sub scope with not unique name like 'if', 'for', etc.
    /// which need adding span information to make unique
    pub fn sub_with_span(&self, name: &str, span: Span, scope_type: ScopeType) -> Self {
        FromSession{ scope: self.scope.sub(format!("<{}{}>", name, span.format(Some(self.source))), scope_type), source: self.source, symbols: self.symbols }
    }

    pub fn into_scope(self) -> SharedDefScope { self.scope }
}


/// Parameter for `ISemanticAnalyze::collect_definitions`
pub struct CollectSession<'a, 'b> {
    node_path: Vec<usize>,
    defs: &'a mut DefinitionCollection,
    messages: &'b mut MessageCollection,
}
impl<'a, 'b> CollectSession<'a, 'b> {

    pub fn new(defs: &'a mut DefinitionCollection, messages: &'b mut MessageCollection) -> Self {
        CollectSession{ defs, messages, node_path: Vec::new() }
    }

    pub fn push_path(&mut self, child_id: usize) {
        self.node_path.push(child_id);
    }
    pub fn pop_path(&mut self) {
        let _ = self.node_path.pop();
    }

    pub fn push_def(&mut self, name: SymbolID, name_span: Span, mut scope: SharedDefScope) {
        match self.defs.push(Definition::new(name, self.node_path.clone())) {
            Some(defid) => scope.push_definition(defid),
            None => self.messages.push(Message::new_by_str("name already defined", vec![(name_span, "")])),
        }
    }
    #[allow(dead_code)] // TODO: temp remove
    pub fn push_message(&mut self, message: Message) {
        self.messages.push(message);
    }
}

pub trait ISemanticAnalyze {

    // Display, which is actually Debug but I don't like `fn debug() -> String`, should not be implemented
    fn display<'a>(&'a self) -> Wrapper<'a, Self> where Self: Sized { Wrapper(self) }
    // format, should be implemented
    fn format(&self, f: Formatter) -> String;

    // phase 1: convert syntax node and collect definitions
    type SyntaxItem;
    // fn convert_collect(item: Self::SyntaxItem, sess: ConvertSession) -> Self;

    fn from_syntax(item: Self::SyntaxItem, sess: FromSession) -> Self;

    // TODO: an empty implement for compatibility temporarily, remove it in future
    // currently, only nodes with `this_scope` can add definition, which contains child node id in the node's child node list
    fn collect_definitions(&self, sess: &mut CollectSession) { let _ = sess; }
}

/// Parameter for `ISemanticAnalyze::convert_from`
#[allow(dead_code)]
pub struct ConvertSession<'a, 'b> {
    scope: SharedDefScope,
    source: &'a SourceCode,
    symbols: &'b SymbolCollection,
    global_defs: RefCell<DefinitionCollection>,
    messages: RefCell<MessageCollection>,
}
#[allow(dead_code)]
impl<'a, 'b> ConvertSession<'a, 'b> {
    
    pub fn new(scope: SharedDefScope, source: &'a SourceCode, symbols: &'b SymbolCollection, global_defs: DefinitionCollection, messages: MessageCollection) -> Self {
        ConvertSession{ scope, source, symbols, global_defs: RefCell::new(global_defs), messages: RefCell::new(messages) }
    }
}

#[cfg(test)] #[test]
fn p1_usage() {

    trait ISemanticAnalyze2 {
        fn convert_from(sess: &ConvertSession) -> Self;
    }

    // let syntax_tree = syntax::SyntaxTree::new_modules(vec![syntax::Module::new], vec![]);
}