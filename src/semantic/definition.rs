///! fff-lang
///!
///! semantic/definition, definition and definition collection and id

use std::fmt;

use codemap::SymbolID;

#[derive(Eq, PartialEq)]
pub struct Definition {
    name: SymbolID,
    node_path: Vec<usize>, // child node indexes
}
impl fmt::Debug for Definition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "def {:?} at {:?}", self.name, self.node_path)       
    }
}
impl Definition {
    pub fn get_node_path(&self) -> &Vec<usize> { &self.node_path }
}

impl Definition {
    pub fn new(name: SymbolID, node_path: Vec<usize>) -> Definition { Definition{ name, node_path } }
}
#[derive(Eq, PartialEq, Clone, Copy)]
pub struct DefID(usize);

impl fmt::Debug for DefID {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#{}", self.0)       
    }
}
impl DefID {
    pub fn new(id: usize) -> DefID { DefID(id) }
}

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct DefinitionCollection {
    items: Vec<Definition>,
}
impl fmt::Debug for DefinitionCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.items)
    }
}
impl DefinitionCollection {

    pub fn new() -> DefinitionCollection { DefinitionCollection{ items: Vec::new() } }

    /// None for already exists
    pub fn push(&mut self, def: Definition) -> Option<DefID> {
        for item in &self.items {
            if item == &def {
                return None;
            }
        }
        self.items.push(def);
        return Some(DefID(self.items.len() - 1));
    }
}