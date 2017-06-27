///! fff-lang
///!
///! semantic/package, a compilation unit

use syntax;

use super::super::Statement;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub items: Vec<Statement>,
}
impl From<syntax::SyntaxTree> for Package {
    fn from(root: syntax::SyntaxTree) -> Package {
        Package{
            items: root.items.into_iter().map(Into::into).collect(),
        }
    }
}