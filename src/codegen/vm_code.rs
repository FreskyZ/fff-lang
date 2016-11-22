
// VM Instruction design

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    IntegralLiteral(u64),  // integral, char, bool
    FloatingLiteral(f64),  // f32, f64
    StringLiteral(String),  
}
impl Eq for LiteralValue {
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