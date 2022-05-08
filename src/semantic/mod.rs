
// more abstract syntax tree with name and type resolved
pub mod mast;

use crate::common::arena::{Arena, Index};
use crate::diagnostics::Diagnostics;
use crate::syntax::ast;

pub fn resolve(_modules: Vec<Index<ast::Module>>, _diagnostics: &mut Diagnostics, arena: &Arena) -> Index<mast::Program> {
    arena.emplace(|n| { *n = mast::Program; })
}
