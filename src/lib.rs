#![feature(allocator_api)]
#![feature(strict_provenance)]

// TODO: remove pub except interface
pub mod common;
mod source;
mod diagnostics;
mod lexical;
pub mod syntax;
pub mod interface;
