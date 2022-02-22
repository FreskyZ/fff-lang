///! fff-lang
///!
///! semantic/fn_def

use codemap::Span;
use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;
use super::super::ScopeType;
use super::super::Formatter;
use super::super::FromSession;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnParam {
    pub name: SymbolID,
    pub typeuse: TypeUse,
}

#[cfg_attr(test, derive(Eq, PartialEq, Debug))]
pub struct FnDef {
    pub name: SymbolID,
    pub name_span: Span,
    pub params: Vec<FnParam>,
    pub rettype: Option<TypeUse>,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for FnDef {

    fn format(&self, f: Formatter) -> String {
        f.indent().header_text_or("fn-def").endl()
            .this_scope1(&self.this_scope).endl()
            .indent1().lit("fn-name").space().sym(self.name).endl()
            .map_or_else(&self.rettype, |f, typeuse| f.apply1_with_header_text("return-type", typeuse), |f| f.indent1().lit("return-type-unit"))
            .foreach_or_else(&self.params, |f, &FnParam{ ref typeuse, ref name }| f.endl()
                .indent1().lit("parameter").space().sym(*name).endl()
                .apply2(typeuse), |f| f.endl().indent1().lit("no-paramater"))
            .endl()
            .apply1_with_header_text("body", &self.body)
            .finish()
    }

    type SyntaxItem = syntax::FnDef;

    fn from_syntax(node: syntax::FnDef, sess: FromSession) -> FnDef {
        let this_sess = sess.sub_with_symbol(node.name, ScopeType::FnDef);
        FnDef{
            name: node.name,
            name_span: node.name_span,
            params: node.params.into_iter().map(|param| {
                FnParam{
                    name: param.name,
                    typeuse: TypeUse::from_syntax(param.decltype, this_sess.clone_scope()),
                }
            }).collect(),
            rettype: node.ret_type.map(|ty| TypeUse::from_syntax(ty, this_sess.clone_scope())),
            body: Block::from_syntax(node.body, this_sess.clone_scope()),
            this_scope: this_sess.into_scope(),
        }
    }
}
