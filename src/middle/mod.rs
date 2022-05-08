
// Fff-lang intermediate (U not included) Representation
// // there is normal abbreviation like MAST: More Abstract Syntax Tree
// // and recursive abbreviation like GNU: GNU is Not Unix, then what's this abbreviation? negative recursive abbreviation?
pub mod fur;

use crate::common::arena::{Arena, Index};
use crate::diagnostics::Diagnostics;
use crate::semantic::mast;

pub fn build(_context: &mut fur::TypeContext, _program: Index<mast::Program>, _diagnostics: &mut Diagnostics, _arena: &Arena) -> fur::Program {
    fur::Program
}

// entry for optimize
// pub fn transform(program, configs)
