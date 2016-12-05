
// Expression generator

use std::fmt;

use common::From2;
use common::StringPosition;
use common::format_vector_debug;
use message::CodegenMessage;

use lexical::LexicalLiteral;
use lexical::SeperatorKind;

use syntax::ExpressionBase as FullExpressionBase;
use syntax::ExpressionOperator as FullExpressionOperator;
use syntax::Expression as FullExpression;
use syntax::ExpressionStatement as FullExpressionStatement;
use syntax::SMType;

use codegen::var_def::VarID;
use codegen::var_def::VarCollection;
use codegen::fn_def::FnID;
use codegen::type_def::TypeID;
use codegen::Operand;
use codegen::Code;
use codegen::CodeID;
use codegen::AssignOperator;
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
                    Err(expr) => push_temp_expr!(sess, expr, assigns, pos),
                };
                ops.push(SimpleOp::MemberFunctionCall("get_Index".to_owned(), vec![simple_base], [pos, StringPosition::new()]));
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

fn gen_simple_expr(_simple_expr: SimpleExpr, _sess: &mut GenerationSession) -> Option<Operand> {
    None
}

pub fn gen_expr(expr: FullExpression, sess: &mut GenerationSession) -> Option<Operand> {
   
    let mut assigns = Vec::new();
    let simple_expr = match simplize_expr(expr, sess, &mut assigns) {
        None => return None,
        Some(simple_expr) => simple_expr,
    };

    for assign in assigns {
        gen_simple_expr(assign.right, sess);
        
    }
    
    None
}

// Currently, valid expression statement, is one of
// Assignment or OpAssignment statement, left of assign operator is identifier
// single expression statement, one of
//     last of ops is function call or member function call
//     last of ops is increase or decrease
pub fn gen_expr_stmt(expr_stmt: FullExpressionStatement, sess: &mut GenerationSession, ignore_left_const: bool) {

    if expr_stmt.op.is_none() { // expression statement
        let expr = expr_stmt.left_expr;
        let pos = expr_stmt.pos[1];      // position of semicolon

        if expr.ops.len() == 0 {
            sess.msgs.push(CodegenMessage::InvalidExpressionStatementSingleSimpleExpression{ pos: StringPosition::from2(expr.pub_pos_all().start_pos, pos.end_pos) });
            return;
        }

        match expr.ops.iter().last().unwrap() {
            &FullExpressionOperator::FunctionCall(_, _) 
            | &FullExpressionOperator::MemberFunctionCall(_, _, _)
            | &FullExpressionOperator::Unary(SeperatorKind::Increase, _)
            | &FullExpressionOperator::Unary(SeperatorKind::Decrease, _) => (),
            _ => {
                sess.msgs.push(CodegenMessage::InvalidExpressionStatementLastOpNotValid{ pos: StringPosition::from2(expr.pub_pos_all().start_pos, pos.end_pos) });
                return;
            }
        }

        let _maybe_codeop = gen_expr(expr, sess); // ignore it
    } else {               // assignment statement
        let left_expr = expr_stmt.left_expr;
        let semicolon_pos = expr_stmt.pos[1];
        let left_expr_pos = left_expr.pub_pos_all();
        let mut left_ident_name = None;
        if left_expr.ops.len() != 0 {
            match left_expr.base.as_ref() {
                &FullExpressionBase::Ident(ref name, _) => left_ident_name = Some(name.clone()),
                _ => (),
            }
        }
        if left_ident_name.is_none() {
            sess.msgs.push(CodegenMessage::LeftOfAssignmentStatementCannotBeComplex{ pos: left_expr.pub_pos_all() });
            return;
        }

        let left_varid = sess.vars.find_by_name(&left_ident_name.unwrap());
        let op = expr_stmt.op.unwrap();
        let right_expr = expr_stmt.right_expr.unwrap();

        match sess.vars.find_by_id(left_varid) {
            Some(ref var) => if var.is_const && !ignore_left_const { // is const but not ignore const
                sess.msgs.push(CodegenMessage::AssignToConstVar{
                    name: var.name.clone(),
                    pos: StringPosition::from2(left_expr_pos.start_pos, semicolon_pos.end_pos),
                });
                // still generate and continue
            },
            None => (), // message emitted
        }

        match gen_expr(right_expr, sess) {
            Some(operand) => sess.codes.emit(Code::Assign(left_varid, AssignOperator::from(op), operand)),
            None => CodeID::dummy(),
        };
    }
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

#[cfg(test)] #[test]
fn gen_expr_stmt_all() {

}