
// VM Instruction design

use lexical::NumLitValue;
use syntax::SMType;

#[derive(Debug, Eq, PartialEq)]
pub enum LiteralValue {
    StringLiteral(String),
    CharLiteral(char),
    BooleanLiteral(bool),
    NumericLiteral(NumLitValue),
    EmptyArray,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operand {
    Literal(LiteralValue),
    Identifier(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Operator {

}

#[derive(Debug, Eq, PartialEq)]
pub enum VMCode {
    
    Mov(usize, usize),
    UnOp(usize, Operator),
    BinOp(usize, usize, Operator),

    Call(usize),
    
    Goto(usize),
    GotoIf(bool, usize),
}

pub struct VMCodeCollection {
    codes: Vec<VMCode>,
}

impl VMCodeCollection {
    
    pub fn new() -> VMCodeCollection {
        VMCodeCollection{ codes: Vec::new() }
    }
}