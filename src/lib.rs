#![feature(allocator_api)]
#![feature(control_flow_enum)]
#![feature(let_chains)]
#![feature(strict_provenance)]
#![feature(try_trait_v2)]

// TODO: remove pub except interface
pub mod common;
mod source;
mod diagnostics;
mod lexical;
pub mod syntax;
pub mod semantic;
pub mod middle;
pub mod interface;
