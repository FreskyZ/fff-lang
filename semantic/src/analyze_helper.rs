///! fff-lang
///!
///! semantic/traits

use super::SharedDefScope;

pub trait ISemanticAnalyze {

    // phase 1: direct map from syntax node
    type SyntaxItem;
    fn from_syntax(item: Self::SyntaxItem, parent_scope: SharedDefScope) -> Self;

    // TODO: an empty implement for compatibility temporarily, remove it in future
    fn collect_type_declarations(&mut self) { }
}
