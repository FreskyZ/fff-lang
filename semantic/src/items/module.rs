///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SymbolID;

use super::super::Item;
use super::super::FromSyntax;
use super::super::SharedDefScope;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct Module {
    pub items: Vec<Item>,
    pub imports: Vec<Module>,
}
impl FromSyntax<syntax::Module> for Module {
    fn from_syntax(node: syntax::Module, parent_scope: SharedDefScope) -> Module {
        Module{
            imports: Vec::new(),
            items: node.items.into_iter().map(|item| FromSyntax::from_syntax(item, parent_scope.clone())).collect(),
        }
    }
}
