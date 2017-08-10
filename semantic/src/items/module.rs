///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SymbolCollection;

use super::super::Item;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;
use super::super::Formatter;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct Module {
    pub module_id: usize,
    pub items: Vec<Item>,
    pub this_scope: SharedDefScope,
}
impl ISemanticAnalyze for Module {

    fn format(&self, f: Formatter) -> String {
        let mut f = f.indent().header_text_or("module").lit("#").debug(&self.module_id).space().debug(&self.this_scope).endl();
        for item in &self.items {
            f = f.apply1(item);
        }
        f.finish()
    }

    type SyntaxItem = syntax::Module;

    // here the trait method's second parameter changes from parent_scope to this_scope
    // because multiple file support in Package need more control of module's scope
    fn from_syntax(node: syntax::Module, this_scope: SharedDefScope, symbols: &mut SymbolCollection) -> Module {
        Module{
            module_id: node.source.get_file_id(),
            items: node.items.into_iter().map(|item| Item::from_syntax(item, this_scope.clone(), symbols)).collect(),
            this_scope: this_scope,
        }
    }
}
impl Module {

    pub fn buildup_imports(&mut self, import_maps: &Vec<syntax::ImportMap>, modules: &mut Vec<syntax::Module>, symbols: &mut SymbolCollection) {

        let module_id = self.module_id; // if use `self.module_id` in the for expr then rustc complains about mutably borrowed self in `&mut self.items` ... hope non lexical lifetime solve this
        for item in &mut self.items {
            if let &mut Item::Import(ref mut import_stmt) = item {
                let imported_file_id = import_maps.into_iter()
                    .filter(|import_map| import_map.file_id == module_id && import_map.import_name == import_stmt.name.value)
                    .next().unwrap().imported_file_id;  // valid syntax tree will make sure this next and this unwrap is success
                let mut imported_module = Module::from_syntax(
                    modules[imported_file_id].move_out(), 
                    self.this_scope.sub(symbols.get(import_stmt.name.value).unwrap()), // valid syntax tree will make sure this unwrap is success
                    symbols
                );
                imported_module.buildup_imports(import_maps, modules, symbols);
                import_stmt.module = Some(imported_module);
            }
        }
    }
}