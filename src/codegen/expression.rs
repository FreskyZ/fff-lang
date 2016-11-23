
// Expression generator

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use syntax::FullExpressionBase;
use syntax::FullExpression;

use codegen::VarID;
use codegen::FnID;
use codegen::session::GenerationSession;

pub struct SimpleAssignStatement {
    pub left: VarID,
    pub right: Expression
}

// About unit type
// unit type is (), only one value, which is (),
// unit type is identical to other type, can be used in function overload
// unit type only have equality operator, where eq is replaced with true and ne is replaced with false
// unit type var has 0 size and thus the same place as the previous local var or tuple element
// simple unit literal can be simple expression, the more complex equality compare one is replaced with true or false value
// so simple unit literal can be at assign right
// simple unit literal cannot be at left 

enum SimpleBase {
    Unit(),        
    Lit(LexicalLiteral),
    Ident(VarID),  // Paren is here
    TupleDef(Vec<VarID>),
    ArrayDef(Vec<VarID>),
    ArrayDupDef(VarID, VarID),
    FunctionCall(FnID, Vec<VarID>),  // global and member function call is here
}
enum SimpleOp {
    MemberFunctionCall(String, Vec<VarID>),
    UnOp(SeperatorKind),                  // Primitive numeric type binary and unary operator are here, which return at rax
    BinOp(VarID, SeperatorKind),
    GetIndex(VarID),      // only one parameter accepted here, and changed to var id
    MemberAccess(usize),  // Directly to string
}
struct SimpleExpr {
    base: SimpleBase,
    ops: Vec<SimpleOp>,
}

fn expr_base_expand(expr_base: FullExpressionBase, sess: &mut GenerationSession) -> (Vec<SimpleAssignStatement>, Expression) {

}

// expand full syntax::Expression to Simplize1Expression, which do not have embeded expression in base and ops, which are seperated in SimpleAssignStatements
fn expr_expand(expr: FullExpression, _sess: &mut GererationSession) -> (Vec<SimpleAssignStatement>, Expression) {
    (Vec::new(), expr)
} 
#[cfg(test)] #[test]
fn gen_expr_simplize_1() {

}

// Process expression, check type and operator existence
// return returned value addr or eax or lit and type
pub fn gen_expr(_expr: FullExpression, _sess: &mut GenerationSession) {

}

#[cfg(test)]
#[test]
fn gen_expr_all() {

}