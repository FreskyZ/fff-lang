
// VM Instruction design

use std::fmt;

use common::format_vector_debug;

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use codegen::VarID;
use codegen::TypeID;


#[derive(Eq, PartialEq, Clone)]
pub enum Operand {
    Unknown,
    Lit(LexicalLiteral),
    Stack(VarID),
    // Heap(usize),
    Register,     // act as register rax, every operation return at stacktop, only store moves it some where
} 
impl fmt::Debug for Operand {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Operand::Unknown => write!(f, "<unknown>"),
            Operand::Lit(ref lit) => write!(f, "{}", lit),
            Operand::Stack(VarID::Some(id)) => write!(f, "[rbp - {}]", id),
            Operand::Stack(VarID::Invalid) => write!(f, "<unknown-local>"),
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

    DeclareVar(String, TypeID, bool),         // name, type, is const
    ScopeBarrier(bool),                         // push or pop

    CallGlobal(String, Vec<Operand>),
    CallMember(Operand, String, Vec<Operand>),
    Binary(Operand, BinaryOperator, Operand),
    Unary(Operand, UnaryOperator),
    TypeCast(Operand, TypeID),
    Assign(Operand, AssignOperator, Operand),  // use the assign operator to assign the var

    Return(Operand),                          // return; is return ();
    Goto(CodeID),
    GotoIf(Operand, bool, CodeID),
}
impl fmt::Debug for Code {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Code::PlaceHolder => 
                write!(f, "placeholder"),
            Code::DeclareVar(ref name, ref ty, true) => 
                write!(f, "declare const {:?} {}", ty, name),
            Code::DeclareVar(ref name, ref ty, false) => 
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
        CodeCollection{ codes: Vec::new() }
    }

    pub fn emit(&mut self, code: Code) -> CodeID {
        let ret_val = self.codes.len();
        self.codes.push(code);
        return CodeID(ret_val);
    }
    pub fn emit_silent(&mut self, code: Code) {
        self.codes.push(code);
    }

    pub fn refill(&mut self, id: CodeID, code: Code) {
        let CodeID(id) = id;
        self.codes[id] = code;
    }

    pub fn next_id(&self) -> CodeID {
        CodeID(self.codes.len())
    }
    pub fn refill_addr(&mut self, gotoid: CodeID, target_id: CodeID) {
        let CodeID(gotoid) = gotoid;
        match self.codes[gotoid] {
            Code::Goto(ref mut id) => *id = target_id,
            Code::GotoIf(ref _op, ref _bool, ref mut id) => *id = target_id,
            _ => unreachable!(),
        }
    }

    pub fn move_from(&mut self, other: &mut CodeCollection) {
        self.codes.extend_from_slice(other.codes.as_slice()); 
        other.codes.clear();
    }

    pub fn dump(&self) -> String {
        format_vector_debug(&self.codes.iter().enumerate().collect(), "\n")
    }
}