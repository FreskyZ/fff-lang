///! fff-lang
///!
///! semantic/package, a compilation unit

use syntax;

use super::TypeDef;
use super::FnDef;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Package {
    pub types: Vec<TypeDef>,   
    pub fns: Vec<FnDef>,
}
impl From<syntax::SyntaxTree> for Package {
    fn from(root: syntax::SyntaxTree) -> Package {
        Package{
            types: root.types.into_iter().map(Into::into).collect(),
            fns: root.fns.into_iter().map(Into::into).collect(),
        }
    }
}