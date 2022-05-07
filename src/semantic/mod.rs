
// more abstract syntax tree with name and type resolved
pub mod mast;

use crate::common::arena::{Arena, Index};
use crate::diagnostics::Diagnostics;
use crate::syntax::ast;

pub fn resolve<'a>(_modules: Vec<Index<'a, ast::Module<'a>>>, _diagnostics: &mut Diagnostics, arena: &'a Arena) -> Index<'a, mast::Program> {
    arena.emplace(|n| { *n = mast::Program; })
}
