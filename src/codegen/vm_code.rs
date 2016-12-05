
// VM Instruction design

use std::fmt;

use common::format_vector_debug;

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use codegen::VarID;
use codegen::TypeID;


#[derive(Eq, PartialEq, Debug)]
pub enum Operand {
    Lit(LexicalLiteral),
    Stack(usize),
    // Heap(usize),
    Register,     // act as register rax, every operation return at stacktop, only store moves it some where
} 

#[derive(Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Increase,
    Decrease, 
    Negative,
    LogicalNot,
}

#[derive(Eq, PartialEq, Debug)]
pub enum BinaryOperator {
    Add, 
    Sub, 
    Mul, 
    Div,
    Rem, 
    ShiftLeft, 
    ShiftRight, 
    Equal, 
    NotEqual,
    Great, 
    Less, 
    GreatEqual, 
    LessEqual,
    BitAnd, 
    BitOr, 
    BitXor, 
    LogicalAnd, 
    LogicalOr,
}

#[derive(Eq, PartialEq, Debug)]
pub enum AssignOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    RemAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
}
impl From<SeperatorKind> for AssignOperator {
    fn from(sep: SeperatorKind) -> AssignOperator {
        match sep {
            SeperatorKind::Assign => AssignOperator::Assign,
            SeperatorKind::AddAssign => AssignOperator::AddAssign,
            SeperatorKind::SubAssign => AssignOperator::SubAssign,
            SeperatorKind::MulAssign => AssignOperator::MulAssign,
            SeperatorKind::DivAssign => AssignOperator::DivAssign,
            SeperatorKind::RemAssign => AssignOperator::RemAssign,
            SeperatorKind::BitAndAssign => AssignOperator::BitAndAssign,
            SeperatorKind::BitOrAssign => AssignOperator::BitOrAssign,
            SeperatorKind::BitXorAssign => AssignOperator::BitXorAssign,
            _ => unreachable!(), // Very confident about this
        }
    }
}

pub enum Code {
    PlaceHolder,

    DeclareLocal(String, TypeID, bool),         // name, type, is const
    ScopeBarrier(bool),                         // push or pop

    CallGlobal(String, Vec<Operand>),
    CallMember(Operand, String, Vec<Operand>),
    Binary(Operand, BinaryOperator, Operand),
    Unary(Operand, UnaryOperator),
    TypeCast(Operand, TypeID),
    Assign(VarID, AssignOperator, Operand),  // use the assign operator to assign the var

    Goto(CodeID),
    GotoIf(Operand, CodeID),

    Halt,                                     // Special Halt at head for vm exit
}
impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Code::PlaceHolder => 
                write!(f, "placeholder"),
            Code::DeclareLocal(ref name, ref ty, true) => 
                write!(f, "declare const {:?} {}", ty, name),
            Code::DeclareLocal(ref name, ref ty, false) => 
                write!(f, "declare var {:?} {}", ty, name),
            Code::ScopeBarrier(true) => 
                write!(f, "scope enter"),
            Code::ScopeBarrier(false) => 
                write!(f, "scope exit"),
            Code::CallGlobal(ref name, ref params) => 
                write!(f, "call {}, {}", name, format_vector_debug(params, ", ")),
            Code::CallMember(ref this, ref name, ref params) =>
                write!(f, "call {:?}.{}, {}", this, name, format_vector_debug(params, ", ")),
            Code::Binary(ref left, ref op, ref right) =>
                write!(f, "{:?} {:?} {:?}", op, left, right),
            Code::Unary(ref left, ref op) =>
                write!(f, "{:?} {:?}", op, left),
            Code::TypeCast(ref op, ref typeid) => 
                write!(f, "cast {:?} {:?}", op, typeid),
            Code::Assign(ref varid, ref assignop, ref op) =>
                write!(f, "store {:?} {:?} {:?}", varid, assignop, op),
            Code::Goto(ref id) => 
                write!(f, "goto {:?}", id),
            Code::GotoIf(ref op, ref id) =>
                write!(f, "goto {:?} if {:?}", id, op),
            Code::Halt => 
                write!(f, "halt"),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub struct CodeID(usize); // CodeID are always valid
impl CodeID {
    pub fn dummy() -> CodeID{ CodeID(!0) }
}

pub struct CodeCollection {
    codes: Vec<Code>,
}

impl CodeCollection {
    
    pub fn new() -> CodeCollection {
        CodeCollection{ codes: vec![Code::Halt] }
    }

    pub fn emit(&mut self, code: Code) -> CodeID {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return CodeID(ret_val);
    }
    pub fn refill(&mut self, id: CodeID, code: Code) {
        let CodeID(id) = id;
        self.codes[id] = code;
    }

    pub fn dump(&self) -> String {
        format_vector_debug(&self.codes, "\n")
    }
}