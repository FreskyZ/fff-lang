///! fff-lang
///!
///! semantic/fn_def

use codemap::SymbolID;
use syntax;

use super::TypeUse;
use super::Block;
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
    pub params: Vec<FnParam>,
    pub rettype: Option<TypeUse>,
    pub body: Block,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for FnDef {

    fn format(&self, f: Formatter) -> String {
        let f = f.indent().header_text_or("fn-def").space().debug(&self.this_scope).endl()
            .indent1().lit("fn-name").space().sym(self.name);
        let mut f = match self.rettype { 
            Some(ref ret_type) => f.endl().set_header_text("return-type").apply1(ret_type).unset_header_text(),
            None => f.endl().indent1().lit("return-type-unit"),
        };
        if self.params.len() == 0 {
            f = f.endl().indent1().lit("no-parameter");
        }
        for &FnParam{ ref typeuse, ref name } in &self.params {
            f = f.endl().indent1().lit("parameter").space().sym(*name).endl().apply2(typeuse)
        }
        f.endl().set_header_text("body").apply1(&self.body).finish()
    }

    type SyntaxItem = syntax::FnDef;

    fn from_syntax(node: syntax::FnDef, sess: FromSession) -> FnDef {
        let this_sess = sess.sub_with_symbol(node.name);
        FnDef{
            name: node.name,
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
