#![feature(allocator_api)]
#![feature(strict_provenance)]
#![feature(try_trait_v2)]
#![feature(control_flow_enum)]

// TODO: remove pub except interface
pub mod common;
mod source;
mod diagnostics;
mod lexical;
pub mod syntax;
pub mod interface;
