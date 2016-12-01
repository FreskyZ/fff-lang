
// Expression generator

use std::fmt;

use common::StringPosition;
use common::format_vector_debug;
use message::CodegenMessage;

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use syntax::ExpressionBase as FullExpressionBase;
use syntax::ExpressionOperator as FullExpressionOperator;
use syntax::Expression as FullExpression;
use syntax::SMType;

use codegen::Operand;
use codegen::var_def::VarID;
use codegen::var_def::VarCollection;
use codegen::fn_def::FnID;
use codegen::type_def::TypeID;
use codegen::Code;
use codegen::session::GenerationSession;

// About unit type
// unit type is (), only one value, which is (),
// unit type is identical to other type, can be used in function overload
// unit type only have equality operator, where eq is replaced with true and ne is replaced with false
// unit type var has 0 size and thus the same place as the previous local var or tuple element
// simple unit literal can be simple expression, the more complex equality compare one is replaced with true or false value
// so simple unit literal can be at assign right
// simple unit literal cannot be at left 

#[derive(Eq, PartialEq)]
pub enum SimpleBase {
    Lit(LexicalLiteral, StringPosition),
    Ident(VarID, StringPosition),             // Paren is here
    FunctionCall(String, Vec<SimpleBase>, [StringPosition; 2]),      // call of operator()
}
impl fmt::Debug for SimpleBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleBase::Lit(ref lit, ref pos) => write!(f, "{:?} @ {:?}", lit, pos),
            SimpleBase::Ident(ref id, ref pos) => write!(f, "{:?} @ {:?}", id, pos),
            SimpleBase::FunctionCall(ref name, ref params, ref pos) => write!(f, "call {}, {} @ {:?}", name, format_vector_debug(params, ", "), pos),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum SimpleOp {
    MemberFunctionCall(String, Vec<SimpleBase>, [StringPosition; 2]),
    UnOp(SeperatorKind, StringPosition),               
    BinOp(SimpleBase, SeperatorKind, StringPosition),
    GetIndex(SimpleBase, StringPosition), // only one parameter accepted here, and changed to var id
    MemberAccess(String, StringPosition),  // Directly to string
    TypeCast(TypeID, StringPosition),
}

#[derive(Eq, PartialEq, Debug)]
struct SimpleExpr {
    base: SimpleBase,
    ops: Vec<SimpleOp>,
}

#[derive(Eq, PartialEq)]
struct SimpleAssignment {
    left: VarID,
    right: SimpleExpr,
}
impl fmt::Debug for SimpleAssignment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} = {:?}", self.left, self.right)
    }
}

// Change to simple base if can 
fn check_pure_simple_expr(full_expr: FullExpression, sess: &mut GenerationSession) -> Result<SimpleBase, FullExpression> {

    if full_expr.ops.len() != 0 { return Err(full_expr); } // if have ops, absolutely not pure simple

    match full_expr.base.as_ref().clone() {
        FullExpressionBase::Lit(lit, pos) => Ok(SimpleBase::Lit(lit, pos)),
        FullExpressionBase::Ident(name, pos) => {
            let varid = sess.vars.find_by_name(&name);
            Ok(SimpleBase::Ident(varid, pos))
        } 
        _ => Err(full_expr),
    }
}

// return none for error and this expression is abandoned
// ++ limit is not here, ++anything's type is unit, thus prevent any more operation
fn simplize_expr(expr: FullExpression, sess: &mut GenerationSession, assigns: &mut Vec<SimpleAssignment>) -> Option<SimpleExpr> {
    
    macro_rules! push_temp_expr { 
        ($sess: expr, $expr: expr, $assigns: expr, $pos: expr) => ({
            let simple_expr = match simplize_expr($expr, $sess, $assigns) {
                Some(simple_expr) => simple_expr,
                None => return None,
            };
            let varid = $sess.vars.push_temp(TypeID::Some(14), false, &mut $sess.types, &mut $sess.msgs);
            $assigns.push(SimpleAssignment{ left: varid, right: simple_expr });
            SimpleBase::Ident(varid, $pos)
        }) 
    }

    let mut ident_name = None; // if function call, need another to record fn name because it (may) cannot be found in vars, may means local var shadow the global fn
    let mut simple_base = match expr.base.as_ref().clone() {
        FullExpressionBase::Lit(lit, pos) => SimpleBase::Lit(lit, pos),
        FullExpressionBase::Ident(name, pos) => {
            let this_ret_val = SimpleBase::Ident(sess.vars.find_by_name(&name), pos);
            ident_name = Some(name);
            this_ret_val
        }
        FullExpressionBase::Paren(expr, pos) => { // allocate temp var and push to assigns
            push_temp_expr!(sess, expr, assigns, pos)
        }
        FullExpressionBase::ArrayDef(exprs, pos) => {
            let mut bases = Vec::new();
            for expr in exprs {
                match check_pure_simple_expr(expr, sess) {
                    Ok(simple_base) => bases.push(simple_base),
                    Err(expr) => bases.push(push_temp_expr!(sess, expr, assigns, pos)),
                }
            }
            SimpleBase::FunctionCall("?new_array".to_owned(), bases, [StringPosition::new(), pos])
        }
        FullExpressionBase::ArrayDupDef(expr1, expr2, pos) => {
            let base1 = match check_pure_simple_expr(expr1, sess) { 
                Ok(simple_base) => simple_base, 
                Err(expr) => push_temp_expr!(sess, expr, assigns, pos[0]),
            };
            let base2 = match check_pure_simple_expr(expr2, sess) { 
                Ok(simple_base) => simple_base, 
                Err(expr) => push_temp_expr!(sess, expr, assigns, pos[1]),
            };
            SimpleBase::FunctionCall("?new_dup_array".to_owned(), vec![base1, base2], pos)
        }
        FullExpressionBase::TupleDef(exprs, pos) => {
            let mut bases = Vec::new();
            for expr in exprs {
                match check_pure_simple_expr(expr, sess) {
                    Ok(simple_base) => bases.push(simple_base),
                    Err(expr) => bases.push(push_temp_expr!(sess, expr, assigns, pos)),
                }
            }
            SimpleBase::FunctionCall("?new_tuple".to_owned(), bases, [StringPosition::new(), pos])
        }
    };

    let mut ops = Vec::new();
    let mut is_first = true;
    for op in expr.ops {
        match op {
            FullExpressionOperator::FunctionCall(exprs, pos) => {
                let fn_name = if !is_first || ident_name.is_none() {
                    sess.msgs.push(CodegenMessage::FunctionCallOperatorNotAppliedToIdentifier{ pos: pos });
                    return None;
                } else {
                    let fn_name = ident_name.clone().unwrap();
                    ident_name = None;
                    fn_name
                };

                let mut bases = Vec::new();
                for expr in exprs {
                    match check_pure_simple_expr(expr, sess) {
                        Ok(simple_base) => bases.push(simple_base),
                        Err(expr) => {
                            bases.push(push_temp_expr!(sess, expr, assigns, pos));
                        }
                    }
                }
                simple_base = SimpleBase::FunctionCall(fn_name, bases, [StringPosition::new(), pos]);
            }
            FullExpressionOperator::MemberFunctionCall(name, exprs, pos) => {
                if ident_name.is_some() { ident_name = None; } // remove ident name
                
                let mut bases = Vec::new();
                for expr in exprs {
                    match check_pure_simple_expr(expr, sess) {
                        Ok(simple_base) => bases.push(simple_base),
                        Err(expr) => {
                            bases.push(push_temp_expr!(sess, expr, assigns, pos[1]));
                        }
                    }
                }
                ops.push(SimpleOp::MemberFunctionCall(name, bases, pos));
            }
            FullExpressionOperator::MemberAccess(name, pos) => {
                if ident_name.is_some() { ident_name = None; }
                ops.push(SimpleOp::MemberAccess(name, pos));
            }
            FullExpressionOperator::Binary(sep, pos, expr) => {
                if ident_name.is_some() { ident_name = None; }

                let simple_base = match check_pure_simple_expr(expr, sess) {
                    Ok(simple_base) => simple_base,
                    Err(expr) => {
                        push_temp_expr!(sess, expr, assigns, pos)
                    }
                };
                ops.push(SimpleOp::BinOp(simple_base, sep, pos));
            }
            FullExpressionOperator::Unary(sep, pos) => {
                if ident_name.is_some() { ident_name = None; }
                ops.push(SimpleOp::UnOp(sep, pos));
            }
            FullExpressionOperator::GetIndex(mut exprs, pos) => {
                if ident_name.is_some() { ident_name = None; }

                if exprs.len() != 1 {
                    sess.msgs.push(CodegenMessage::SubscriptionOperatorParameterCountNot1{ pos: pos });
                    return None;
                }

                let simple_base = match check_pure_simple_expr(exprs.pop().unwrap(), sess) {
                    Ok(simple_base) => simple_base,
                    Err(expr) => {
                        push_temp_expr!(sess, expr, assigns, pos)
                    }
                };
                ops.push(SimpleOp::GetIndex(simple_base, pos));
            }
            FullExpressionOperator::TypeCast(smt, pos) => {
                if ident_name.is_some() { ident_name = None; }
                let typeid = sess.types.try_get_id(smt, &mut sess.msgs);
                ops.push(SimpleOp::TypeCast(typeid, pos));
            }
        }
        is_first = false;
    }

    Some(SimpleExpr{ base: simple_base, ops: ops }) 
}

// Process expression, check type and operator existence
// return returned value addr or eax or lit and type
pub fn gen_expr(expr: FullExpression, sess: &mut GenerationSession) -> Option<SimpleBase> {
   
    let mut assigns = Vec::new();
    let simple_expr = match simplize_expr(expr, sess, &mut assigns) {
        None => return None,
        Some(simple_expr) => simple_expr,
    };


    None
}

#[cfg(test)] #[test]
fn gen_expr_simple() {
    use codegen::var_def::Var;
    use message::MessageEmitter;

    // function call not after ident
    let expr = FullExpression::from_str("1()", 0);
    let mut sess = GenerationSession::new();
    let mut assigns = Vec::new();
    let final_expr = simplize_expr(expr, &mut sess, &mut assigns);
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::FunctionCallOperatorNotAppliedToIdentifier{ pos: make_str_pos!(1, 2, 1, 3) });
    assert_eq!(final_expr, None);
    assert_eq!(&sess.msgs, expect_message);

    // subscription 1 parameter          1234567
    let expr = FullExpression::from_str("a[2, 3]", 0);
    let mut sess = GenerationSession::new();
    let mut assigns = Vec::new();
    let final_expr = simplize_expr(expr, &mut sess, &mut assigns);
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::SubscriptionOperatorParameterCountNot1{ pos: make_str_pos!(1, 2, 1, 7) });
    assert_eq!(final_expr, None);
    assert_eq!(&sess.msgs, expect_message);

    let expr = FullExpression::from_str("1[]", 0);
    let mut sess = GenerationSession::new();
    let mut assigns = Vec::new();
    let final_expr = simplize_expr(expr, &mut sess, &mut assigns);
    let expect_message = &mut MessageEmitter::new();
    expect_message.push(CodegenMessage::SubscriptionOperatorParameterCountNot1{ pos: make_str_pos!(1, 2, 1, 3) });
    assert_eq!(final_expr, None);
    assert_eq!(&sess.msgs, expect_message);
    
    macro_rules! prepare_vars { 
        ($sess: expr, $($name: expr, )*) => (
            $(
                $sess.vars.try_push(Var::new($name.to_owned(), TypeID::Some(1), false, StringPosition::new()), &mut $sess.types, &mut $sess.msgs);
            )*
        )
    }

    // Normal, lit, ident, fn call, array, array dup, tuple, bin, un, member fn call, member access, type cast, get index
    let expr = FullExpression::from_str(
    //   0        1         2         3         4         5         6         7         8         9         A         B
    //   12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
        "1 + 1.to_string()[abc(++defg) as u32 * (a - bcd)].substring((2, \"abc\") / global(hijk + lmn - [1, 2, [4 ; 5]])) - 1", 0
    );
    let mut assigns = Vec::new();
    let mut sess = GenerationSession::new();
    prepare_vars!{ sess, "defg", "a", "bcd", "hijk", "lmn", }
    let final_expr = simplize_expr(expr, &mut sess, &mut assigns);

    assert_eq!(sess.vars.len(), 15);
    perrorln!("assigns: \n{}", format_vector_debug(&assigns, "\n"));
    perrorln!("final expr: \n{:?}", final_expr);
}

#[cfg(test)] #[test]
fn gen_expr_practice() {

    let right_expr = FullExpression::from_str("x * x + y * y - 1", 0);
    let mut assigns = Vec::new();
    let mut sess = GenerationSession::new();
    let final_expr = simplize_expr(right_expr, &mut sess, &mut assigns);
    perrorln!("messages: \n{:?}", sess.msgs);
    perrorln!("assigns: \n{:?}", assigns);
    perrorln!("final expr: \n{:?}", final_expr);

    let right_expr = FullExpression::from_str("a * a * a - x * x * y * y * y <= 0f32", 0);
    let mut assigns = Vec::new();
    let mut sess = GenerationSession::new();
    let final_expr = simplize_expr(right_expr, &mut sess, &mut assigns);
    perrorln!("messages: \n{:?}", sess.msgs);
    perrorln!("assigns: \n{:?}", assigns);
    perrorln!("final expr: \n{:?}", final_expr);
}

#[cfg(test)] #[test]
fn gen_expr_all() {

}