///! fff-lang
///!
///! semantic/module

use syntax;

use codemap::SymbolID;

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
        let mut f = f.indent().header_text_or("module").lit("#").debug(&self.module_id).endl()
            .indent1().debug(&self.this_scope).endl();
        for item in &self.items {
            f = f.apply1(item);
        }
        f.finish()
    }

    type SyntaxItem = syntax::Module;

    fn from_syntax(node: syntax::Module, parent_scope: SharedDefScope) -> Module {
        Module{
            module_id: node.source.get_file_id(),
            items: node.items.into_iter().map(|item| Item::from_syntax(item, parent_scope.clone())).collect(),
            this_scope: parent_scope.sub(node.source.get_file_stem().unwrap()), // auto panic on Path::get_file_stem and OsStr::to_str failure
        }
    }
}
impl Module {

    // phase 1 special, move content out, because it is hard to move out of vec
    pub fn move_out(&mut self) -> Module { 
        let mut retval = Module{
            module_id: self.module_id,
            this_scope: self.this_scope.clone(), 
            items: Vec::new(),
        };
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