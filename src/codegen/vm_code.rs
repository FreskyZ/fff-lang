
// VM Instruction design

use std::fmt;

use common::format_vector_debug;

use lexical::LitValue;
use lexical::SeperatorKind;

use codegen::TypeID;

#[derive(Eq, PartialEq, Clone)]
pub enum Operand {
    Unknown,
    Lit(LitValue),
    Stack(usize), // [rbp - n]
    // Heap(usize),
    Register,     // act as register rax, every operation return at stacktop, only store moves it some where
} 
impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Unknown => write!(f, "<unknown>"),
            Operand::Lit(ref lit) => write!(f, "{}", lit),
            Operand::Stack(ref offset) => write!(f, "[rbp - {}]", offset),
            Operand::Register => write!(f, "rax"),
        }
    }
}

#[derive(Eq, Clone, PartialEq, Debug)]
pub enum UnaryOperator {
    Increase,
    Decrease, 
    BitNot,
    Negative,
    LogicalNot,
}
impl From<SeperatorKind> for UnaryOperator {
    fn from(sep: SeperatorKind) -> UnaryOperator {
        match sep {
            SeperatorKind::BitNot => UnaryOperator::BitNot,
            SeperatorKind::LogicalNot => UnaryOperator::LogicalNot,
            SeperatorKind::Sub => UnaryOperator::Negative,
            SeperatorKind::Increase => UnaryOperator::Increase,
            SeperatorKind::Decrease => UnaryOperator::Decrease, 
            _ => unreachable!(), // Very confident about this
        }
    }
}

#[derive(Eq, Clone, PartialEq, Debug)]
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
impl From<SeperatorKind> for BinaryOperator {
    fn from(sep: SeperatorKind) -> BinaryOperator {
        match sep {
            SeperatorKind::Add => BinaryOperator::Add,
            SeperatorKind::Sub => BinaryOperator::Sub,
            SeperatorKind::Mul => BinaryOperator::Mul,
            SeperatorKind::Div => BinaryOperator::Div,
            SeperatorKind::Rem => BinaryOperator::Rem,
            SeperatorKind::ShiftLeft => BinaryOperator::ShiftLeft,
            SeperatorKind::ShiftRight => BinaryOperator::ShiftRight,
            SeperatorKind::Equal => BinaryOperator::Equal,
            SeperatorKind::NotEqual => BinaryOperator::NotEqual,
            SeperatorKind::Great => BinaryOperator::Great,
            SeperatorKind::Less => BinaryOperator::Less,
            SeperatorKind::GreatEqual => BinaryOperator::GreatEqual,
            SeperatorKind::LessEqual => BinaryOperator::LessEqual,
            SeperatorKind::BitAnd => BinaryOperator::BitAnd,
            SeperatorKind::BitOr => BinaryOperator::BitOr,
            SeperatorKind::BitXor => BinaryOperator::BitXor,
            SeperatorKind::LogicalAnd => BinaryOperator::LogicalAnd,
            SeperatorKind::LogicalOr => BinaryOperator::LogicalOr,
            _ => unreachable!(), // Very confident about this
        }
    }
}

#[derive(Eq, Clone, PartialEq, Debug)]
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

#[derive(Clone)]
pub enum Code {
    PlaceHolder,

    DeclareVar(TypeID, bool),         // name, type, is const

    CallGlobal(String, Vec<Operand>),
    CallMember(Operand, String, Vec<Operand>),
    Binary(Operand, BinaryOperator, Operand),
    Unary(Operand, UnaryOperator),
    TypeCast(Operand, TypeID),
    Assign(Operand, AssignOperator, Operand),  // use the assign operator to assign the var

    Return(Operand),                          // return; is return ();
    Goto(usize),
    GotoIf(Operand, bool, usize),
}
impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Code::PlaceHolder => 
                write!(f, "placeholder"),
            Code::DeclareVar(ref ty, true) => 
                write!(f, "declare const {:?}", ty),
            Code::DeclareVar(ref ty, false) => 
                write!(f, "declare var {:?}", ty),
            Code::CallGlobal(ref name, ref params) => 
                write!(f, "call {}, {}", name, format_vector_debug(params, ", ")),
            Code::CallMember(ref this, ref name, ref params) =>
                write!(f, "call {:?}.{}, {}", this, name, format_vector_debug(params, ", ")),
            Code::Binary(ref left, ref op, ref right) =>
                write!(f, "{:?} {:?}, {:?}", op, left, right),
            Code::Unary(ref left, ref op) =>
                write!(f, "{:?} {:?}", op, left),
            Code::TypeCast(ref op, ref typeid) => 
                write!(f, "{:?} as {:?}", op, typeid),
            Code::Assign(ref target, ref assignop, ref src) =>
                write!(f, "{:?} {:?} {:?}", assignop, target, src),
            Code::Return(ref op) => 
                write!(f, "ret {:?}", op),
            Code::Goto(ref id) => 
                write!(f, "br {:?}", id),
            Code::GotoIf(ref op, true, ref id) =>
                write!(f, "brtrue {:?} if {:?}", id, op),
            Code::GotoIf(ref op, false, ref id) => 
                write!(f, "brfalse {:?} if {:?}", id, op),
        }
    }
}

pub struct CodeCollection {
    pub codes: Vec<Code>,
}

impl CodeCollection {
    
    pub fn new() -> CodeCollection {
        CodeCollection{ codes: Vec::new() }
    }

    pub fn emit(&mut self, code: Code) -> usize {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return ret_val;
    }
    pub fn emit_silent(&mut self, code: Code) {
        self.codes.push(code);
    }

    pub fn next_id(&self) -> usize {
        self.codes.len()
    }
    pub fn dummy_id() -> usize {
        !0
    }

    pub fn refill(&mut self, id: usize, code: Code) {
        self.codes[id] = code;
    }
    pub fn refill_addr(&mut self, gotoid: usize, target_id: usize) {
        match self.codes[gotoid] {
            Code::Goto(ref mut id) => *id = target_id,
            Code::GotoIf(ref _op, ref _bool, ref mut id) => *id = target_id,
            _ => unreachable!(),
        }
    }

    pub fn as_slice(&self) -> &[Code] {
        self.codes.as_slice()
    }

    pub fn dump(&self) -> String {
        format_vector_debug(&self.codes.iter().enumerate().collect(), "\n")
    }
}