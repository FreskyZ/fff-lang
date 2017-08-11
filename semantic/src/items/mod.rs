///! fff-lang
///!
///! semantic/common nodes

use codemap::SymbolID;
use syntax;

mod fn_def;
mod type_def;
mod module;

pub use self::fn_def::FnDef;
pub use self::module::Module;
pub use self::fn_def::FnParam;
pub use self::type_def::TypeDef;
pub use self::type_def::TypeFieldDef;

use super::Formatter;
use super::Statement;
use super::FromSession;
use super::SharedDefScope;
use super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TypeUse {
    pub base_name: SymbolID,
    pub params: Vec<TypeUse>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for TypeUse {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("type-use").space().sym(self.base_name).space().debug(&self.parent_scope)
            .foreach(&self.params, |f, typeuse| f.endl()
                .apply1_with_header_text("type-param", typeuse))
            .finish()
    }
    
    type SyntaxItem = syntax::TypeUse;

    fn from_syntax(node: syntax::TypeUse, sess: FromSession) -> TypeUse {
        TypeUse{
            base_name: node.base,
            params: node.params.into_iter().map(|param| TypeUse::from_syntax(param, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct LabelDef {
    pub name: SymbolID,
    pub parent_scope: SharedDefScope
}
impl ISemanticAnalyze for LabelDef {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("label-def").space().sym(self.name).space().debug(&self.parent_scope).finish()
    }

    type SyntaxItem = syntax::LabelDef;

    fn from_syntax(node: syntax::LabelDef, sess: FromSession) -> LabelDef {
        LabelDef{
            name: node.name,
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct Block {
    pub items: Vec<Statement>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for Block {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("block").space().debug(&self.parent_scope)
            .foreach(&self.items, |f, item| f.endl().apply1(item))
            .finish()
    }

    type SyntaxItem = syntax::Block;
    
    fn from_syntax(node: syntax::Block, sess: FromSession) -> Block {
        Block{
            items: node.items.into_iter().map(|item| Statement::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}
