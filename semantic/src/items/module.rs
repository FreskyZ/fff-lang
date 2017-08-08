///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SymbolID;

use super::super::Item;
use super::super::SharedDefScope;
use super::super::ISemanticAnalyze;

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
pub struct Module {
    pub module_id: usize,
    pub items: Vec<Item>,
}
impl ISemanticAnalyze for Module {

    type SyntaxItem = syntax::Module;

    fn from_syntax(node: syntax::Module, parent_scope: SharedDefScope) -> Module {
        Module{
            module_id: node.source.get_file_id(),
            items: node.items.into_iter().map(|item| Item::from_syntax(item, parent_scope.clone())).collect(),
        }
    }
}
impl Module {

    // phase 1 special, move content out, because it is hard to move out of vec
    pub fn move_out(&mut self) -> Module { 
        let mut retval = Module{ module_id: self.module_id, items: Vec::new() };
        retval.items.append(&mut self.items);
        return retval; 
    }
    pub fn buildup_imports(&mut self, import_maps: &Vec<syntax::ImportMap>, modules: &mut Vec<Module>) {

        let module_id = self.module_id; // if use `self.module_id` in the for expr then rustc complains about mutably borrowed self in `&mut self.items` ... hope non lexical lifetime solve this
        for item in &mut self.items {
            if let &mut Item::Import(ref mut import_stmt) = item {
                let imported_file_id = import_maps.into_iter()
                    .filter(|import_map| import_map.file_id == module_id && import_map.import_name == import_stmt.name.value)
                    .next().unwrap().imported_file_id;  // valid syntax tree will make sure this next and this unwrap is success
                let mut imported_module = modules[imported_file_id].move_out();
                imported_module.buildup_imports(import_maps, modules);
                import_stmt.module = Some(imported_module);
            }
        }
    }
}