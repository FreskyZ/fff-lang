///! fff-lang
///! 
///! semantic/sugar, exprs which are kind of sugar

use syntax;

use super::Expr;
use super::super::Formatter;
use super::super::FromSession;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct ArrayDef {
    pub items: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for ArrayDef {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("array-def").space().debug(&self.parent_scope)
            .foreach(&self.items, |f, expr| f.endl().apply1(expr)).finish()
    }

    type SyntaxItem = syntax::ArrayDef;

    fn from_syntax(node: syntax::ArrayDef, sess: FromSession) -> ArrayDef {
        ArrayDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct TupleDef {
    pub items: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for TupleDef {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("tuple-def").space().debug(&self.parent_scope)
            .foreach(&self.items, |f, expr| f.endl().apply1(expr)).finish()
    }

    type SyntaxItem = syntax::TupleDef;

    fn from_syntax(node: syntax::TupleDef, sess: FromSession) -> TupleDef {
        TupleDef{
            items: node.items.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct IndexCall {
    pub base: Box<Expr>,
    pub params: Vec<Expr>,
    pub parent_scope: SharedDefScope,
}
impl ISemanticAnalyze for IndexCall {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("fn-call").space().debug(&self.parent_scope).endl()
            .apply1_with_prefix_text("base-is", self.base.as_ref())
            .foreach_or_else(&self.params, |f, expr| f.endl().apply1(expr), |f| f.endl().indent1().lit("no-argument"))
            .finish()
    }

    type SyntaxItem = syntax::IndexCallExpr;

    fn from_syntax(node: syntax::IndexCallExpr, sess: FromSession) -> IndexCall {
        IndexCall{
            base: Box::new(Expr::from_syntax(syntax::Expr::unbox(node.base), sess.clone_scope())),
            params: node.params.items.into_iter().map(|item| Expr::from_syntax(item, sess.clone_scope())).collect(),
            parent_scope: sess.into_scope(),
        }
    }
}
