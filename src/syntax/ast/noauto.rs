///! Not Auto part of ast type definition

use std::convert::Infallible;
use std::ops::{Try, FromResidual, ControlFlow};
use super::*;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub enum LitValue {
    Unit,
    Bool(bool),
    Char(char),
    Str(IsId),
    Num(Numeric),
}

impl Expr {
    pub fn dummy(arena: &Arena) -> Self {
        Expr::Lit(arena.emplace_lit_expr(Span::new(0, 0), LitValue::Num(Numeric::I32(0))))
    }
}

// derive Clone for interface to record imports, whether actually derive Clone is not important
impl ModuleStatement {
    pub fn clone(&self) -> Self {
        Self{ span: self.span, name: self.name, path: self.path }
    }
}

impl Index<Module> {
    pub fn imports(self, arena: &Arena) -> Vec<ModuleStatement> {
        arena.get_iter(arena.get(self).items).filter_map(|item| match item {
            Item::Import(module_stmt) => Some(arena.get(*module_stmt).clone()),
            _ => None,
        }).collect()
    }
}

// logically a Result<(), ()> or ControlFlow<(), ()>, but they do not have Default
// pretty visitor and equal visitor both need this style return value, may can be used
pub struct EmptyResult(pub bool);

impl Default for EmptyResult {
    fn default() -> Self{
        Self(true) // true is ok so default is ok, not bool::default(), which is false
    }
}

impl EmptyResult {
    pub fn into_result(self) -> Result<(), ()> {
        if self.0 { Ok(()) } else { Err(()) }
    }
}

impl Try for EmptyResult {

    type Output = ();
    type Residual = ();

    fn from_output(_: ()) -> Self {
        Self(true)
    }

    fn branch(self) -> ControlFlow<()> {
        if self.0 { ControlFlow::CONTINUE } else { ControlFlow::BREAK }
    }
}

impl FromResidual for EmptyResult {

    fn from_residual(_: ()) -> Self {
        Self(false)
    }
}

impl FromResidual<Result<Infallible, std::fmt::Error>> for EmptyResult {

    fn from_residual(_: Result<Infallible, std::fmt::Error>) -> Self {
        Self(false)
    }
}
