
// Expression generator

use common::StringPosition;

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use syntax::ExpressionBase as FullExpressionBase;
use syntax::ExpressionOperator as FullExpressionOperator;
use syntax::Expression as FullExpression;

use codegen::Operand;
use codegen::var_def::VarID;
use codegen::fn_def::FnID;
use codegen::type_def::TypeID;
use codegen::Code;
use codegen::session::GenerationSession;

pub struct SimpleAssignStatement {
    pub left: VarID,
    pub right: FullExpression,
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
    Unit(StringPosition),        
    Lit(LexicalLiteral, StringPosition),
    Ident(VarID, StringPosition),  // Paren is here
    TupleDef(Vec<VarID>, StringPosition),
    ArrayDef(Vec<VarID>, StringPosition),
    ArrayDupDef(VarID, VarID, StringPosition),
}
enum SimpleOp {
    FunctionCall(FnID, Vec<VarID>, [StringPosition; 2]), // call of operator()
    MemberFunctionCall(FnID, Vec<VarID>, [StringPosition; 2]),
    UnOp(SeperatorKind, StringPosition),                // Primitive numeric type binary and unary operator are here, which return at rax
    BinOp(VarID, SeperatorKind, StringPosition),
    GetIndex(VarID, StringPosition),      // only one parameter accepted here, and changed to var id
    MemberAccess(usize, StringPosition),  // Directly to string
}
struct SimpleExpr {
    base: SimpleBase,
    ops: Vec<SimpleOp>,
}

fn expr_base_expand(expr_base: FullExpressionBase, _sess: &mut GenerationSession) -> (Vec<SimpleAssignStatement>, FullExpressionBase) {
    (Vec::new(), expr_base)
}

// expand full syntax::Expression to Simplize1Expression, which do not have embeded expression in base and ops, which are seperated in SimpleAssignStatements
fn expr_expand(expr: FullExpression, _sess: &mut GenerationSession) -> (Vec<SimpleAssignStatement>, FullExpression) {
    (Vec::new(), expr)
} 
#[cfg(test)] #[test]
fn gen_expr_simplize_1() {

}

// Process expression base, to new temp representation
fn gen_expr_base(_expr_base: FullExpressionBase, _sess: &mut GenerationSession) {

}

// Process expression, check type and operator existence
// return returned value addr or eax or lit and type
pub fn gen_expr(expr: FullExpression, sess: &mut GenerationSession) -> (Operand, TypeID) {
    use std::ops::Deref;

    let mut ident_name = String::new();
    let mut base_is_ident = false;
    match expr.base.as_ref().clone() {
        FullExpressionBase::Ident(name, _pos) => {
            ident_name = name;
            base_is_ident = true;
        }
        FullExpressionBase::Lit(LexicalLiteral::Str(val), _pos) => {
            let val = LexicalLiteral::Str(val).get_str_lit_not_option();
            let varid = sess.vars.push_temp(TypeID::Some(13), true, &mut sess.types, &mut sess.msgs);
            let offset = sess.vars.get_offset(varid);
            
            // These alloca stacks at vm not here
            // let current_sp = sess.vars.current_offset();
            // sess.codes.emit(Code::Mov(Operand::Stack(current_sp), Operand::Lit(LitValue::Int(val.len()))));
            // sess.codes.emit(Code::CallInternal(InternalFn::HeapAlloc));
            sess.codes.emit(Code::LoadStr(val, offset));

            return (Operand::Stack(offset), TypeID::Some(13));
        }
        _ => (),
    }

    let mut is_first = true;
    for op in expr.ops {
        match op {
            FullExpressionOperator::FunctionCall(exprs, _pos) => {
                if is_first {
                    if base_is_ident { // global fn call
                        let mut ops = Vec::new();
                        let mut types = Vec::new();
                        for expr in exprs {
                            let ret = gen_expr(expr, sess);
                            ops.push(ret.0);
                            types.push(ret.1);
                        }

                        let fnid = sess.fns.find_by_sign(&ident_name, &types);
                        let current_sp = sess.vars.current_offset();    // push parameter

                        // match sess.fns.get_if_internal(fnid) {
                        //     Some(internal) => sess.codes.emit(Code::CallInternal(internal)), 
                        //     None => (),
                        // }
                    }
                }
            } 
            _ => (),
        }
        is_first = false;
    }

    (Operand::Stack(0), TypeID::Invalid)
}

#[cfg(test)] #[test]
fn gen_expr_helloworld() {

}

#[cfg(test)]
#[test]
fn gen_expr_all() {

}