
// Expression generator

use std::fmt;

use common::From2;
use common::StringPosition;
use common::format_vector_debug;
use message::CodegenMessage;

use lexical::LitValue;
use lexical::SeperatorKind;

use syntax::ExpressionBase as FullExpressionBase;
use syntax::ExpressionOperator as FullExpressionOperator;
use syntax::Expression as FullExpression;
use syntax::ExpressionStatement as FullExpressionStatement;
use syntax::SMType;

use codegen::var_def::VarCollection;
use codegen::ItemID;
use codegen::type_def::TypeCollection;
use codegen::Operand;
use codegen::Code;
use codegen::AssignOperator;
use codegen::BinaryOperator;
use codegen::UnaryOperator;
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
    Lit(LitValue, ItemID, StringPosition),
    Ident(usize, ItemID, StringPosition),                            // Paren is here
    FunctionCall(ItemID, ItemID, Vec<SimpleBase>, [StringPosition; 2]),      // call global
}
impl fmt::Debug for SimpleBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleBase::Lit(ref lit, ref typeid, ref pos) => write!(f, "{:?} @ {:?}, typeid = {:?}", lit, pos, typeid),
            SimpleBase::Ident(ref id, ref typeid, ref pos) => write!(f, "{:?} @ {:?}, typeid = {:?}", id, pos, typeid),
            SimpleBase::FunctionCall(ref name, ref typeid, ref params, ref pos) => write!(f, "call {:?}, {} @ {:?}, ret typeid = {:?}", name, format_vector_debug(params, ", "), pos, typeid),
        }
    }
}
impl SimpleBase {
    fn final_typeid(&self) -> ItemID {
        match *self {
            SimpleBase::Lit(ref _lit_val, ref typeid, ref _pos) => *typeid,
            SimpleBase::Ident(ref _offset, ref typeid, ref _pos) => *typeid,
            SimpleBase::FunctionCall(ref _name, ref typeid, ref _params, ref _pos) => *typeid,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum SimpleOp {
    MemberFunctionCall(String, ItemID, Vec<SimpleBase>, [StringPosition; 2]),
    UnOp(UnaryOperator, ItemID, StringPosition),                    // sep, result typeid, pos
    BinOp(SimpleBase, BinaryOperator, ItemID, StringPosition),      // right_expr, sep, result typeid, pos
    MemberAccess(String, ItemID, StringPosition),  // Directly to string
    TypeCast(ItemID, StringPosition),
}
impl SimpleOp {
    fn final_typeid(&self) -> ItemID {
        match *self {
            SimpleOp::MemberFunctionCall(ref _name, ref typeid, ref _params, ref _pos) => *typeid,
            SimpleOp::UnOp(ref _unop, ref typeid, ref _pos) => *typeid,
            SimpleOp::BinOp(ref _operand, ref _operator, ref typeid, ref _pos) => *typeid,
            SimpleOp::MemberAccess(ref _name, ref typeid, ref _pos) => *typeid,
            SimpleOp::TypeCast(ref typeid, ref _pos) => *typeid,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
struct SimpleExpr {
    base: SimpleBase,
    ops: Vec<SimpleOp>,
}
impl SimpleExpr {
    fn final_typeid(&self) -> ItemID {
        match self.ops.len() {
            0 => self.base.final_typeid(),
            n => self.ops[n - 1].final_typeid(),
        }
    }
}

#[derive(Eq, PartialEq)]
struct SimpleAssignment {
    left: ItemID, // var id
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
        FullExpressionBase::Lit(lit, pos) => {
            let typeid = TypeCollection::get_id_by_lit(&lit);
            return Ok(SimpleBase::Lit(lit, typeid, pos));
        } 
        FullExpressionBase::Ident(name, pos) => {
            let varid = sess.vars.find_by_name(&name);
            let vartype = sess.vars.get_type(varid);
            let varoffset = sess.vars.get_offset(varid);
            Ok(SimpleBase::Ident(varoffset, vartype, pos))
        } 
        _ => Err(full_expr),
    }
}

// return none for error and this expression is abandoned
// ++ limit is not here, ++anything's type is unit, thus prevent any more operation
fn simplize_expr(expr: FullExpression, sess: &mut GenerationSession, assigns: &mut Vec<SimpleAssignment>) -> Option<SimpleExpr> {
    
    // When meet nested fullexpr, use this and returns the ident as simple base and its type
    macro_rules! process_nested_full_expr { 
        ($sess: expr, $expr: expr, $assigns: expr, $pos: expr) => ({
            match check_pure_simple_expr($expr, $sess) {
                Ok(simple_base) => {
                    let simple_base_typeid = simple_base.final_typeid();
                    (simple_base, simple_base_typeid)
                }
                Err(expr) => {
                    let simple_expr = match simplize_expr(expr, $sess, $assigns) {
                        Some(simple_expr) => simple_expr,
                        None => return None,
                    };
                    let simple_expr_typeid = simple_expr.final_typeid();
                    let varid = $sess.vars.push_temp(simple_expr_typeid, false, &mut $sess.types, &mut $sess.msgs);
                    let varoffset = $sess.vars.get_offset(varid);
                    $sess.codes.emit(Code::DeclareVar(simple_expr_typeid));
                    $assigns.push(SimpleAssignment{ left: varid, right: simple_expr });
                    (SimpleBase::Ident(varoffset, simple_expr_typeid, $pos), simple_expr_typeid)
                }
            }
        }) 
    }

    let mut ident_name = None; // if function call, need another to record fn name because it (may) cannot be found in vars, may means local var shadow the global fn
    let mut current_prev_pos = expr.base.pos();
    let mut simple_base = match expr.base.as_ref().clone() {
        FullExpressionBase::Lit(lit, pos) => {
            let lit_typeid = TypeCollection::get_id_by_lit(&lit);
            SimpleBase::Lit(lit, lit_typeid, pos)
        }
        FullExpressionBase::Ident(name, pos) => {
            let ident_varid = sess.vars.find_by_name(&name);
            let ident_offset = sess.vars.get_offset(ident_varid);
            let ident_typeid = sess.vars.get_type(ident_varid);
            let this_ret_val = SimpleBase::Ident(ident_offset, ident_typeid, pos);
            ident_name = Some(name);
            this_ret_val
        }
        FullExpressionBase::Paren(expr, pos) => { // allocate temp var and push to assigns
            process_nested_full_expr!(sess, expr, assigns, pos).0
        }
        FullExpressionBase::ArrayDef(exprs, pos) => {
            let mut bases = Vec::new();
            let mut item0_type: Option<ItemID> = None;
            let mut item0_desc = String::new();
            for expr in exprs {
                let expr_pos = expr.pub_pos_all();
                let (item, item_type) = process_nested_full_expr!(sess, expr, assigns, pos);
                if item0_type.is_none() {
                    item0_type = Some(item_type);
                    item0_desc = sess.types.fmt_by_id(item_type);
                } else {
                    if Some(item_type) != item0_type {
                        let actual_desc = sess.types.fmt_by_id(item_type);
                        sess.msgs.push(CodegenMessage::ArrayInitializeElementNotSameType{ expect: item0_desc.clone(), actual: actual_desc, pos: expr_pos });
                        // continue
                    }
                }
                bases.push(item);
            }
            let fnid = sess.fns.find_by_sign("?new_array", &vec![item0_type.unwrap()]); // array no content has been denied at syntax
            let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
            SimpleBase::FunctionCall(fnid, ret_type, bases, [StringPosition::new(), pos])
        }
        FullExpressionBase::ArrayDupDef(expr1, expr2, pos) => {
            let expr2_pos = expr2.pub_pos_all();
            let (base1, typeid1) = process_nested_full_expr!(sess, expr1, assigns, pos[0]);
            let (base2, typeid2) = process_nested_full_expr!(sess, expr2, assigns, pos[1]);

            if typeid2 != ItemID::new(5) || typeid2 != ItemID::new(8) {
                let actual_desc = sess.types.fmt_by_id(typeid2);
                sess.msgs.push(CodegenMessage::ArrayDupDefSecondParameterTypeNotExpected{ actual: actual_desc, pos: expr2_pos });
            }
            let fnid = sess.fns.find_by_sign("?new_dup_array", &vec![typeid1, typeid2]);
            let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
            SimpleBase::FunctionCall(fnid, ret_type, vec![base1, base2], pos)
        }
        FullExpressionBase::TupleDef(exprs, pos) => {
            let mut bases = Vec::new();
            let mut item_types = Vec::new();
            for expr in exprs {
                let (simple_base, base_typeid) = process_nested_full_expr!(sess, expr, assigns, pos);
                bases.push(simple_base);
                item_types.push(base_typeid);
            }
            let fnid = sess.fns.find_by_sign("?new_tuple", &item_types);
            let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
            SimpleBase::FunctionCall(fnid, ret_type, bases, [StringPosition::new(), pos])
        }
    };
    let mut cur_typeid = simple_base.final_typeid();

    let mut ops = Vec::new();
    let mut is_first = true;
    for op in expr.ops {
        match op {
            FullExpressionOperator::FunctionCall(exprs, pos) => {
                let fn_name = if !is_first || ident_name.is_none() {
                    sess.msgs.push(CodegenMessage::FunctionCallOperatorNotAppliedToIdentifier{ pos: pos });
                    return None;
                } else {
                    ident_name.clone().unwrap()
                };

                let mut bases = Vec::new();
                let mut param_types = Vec::new();
                for expr in exprs {
                    let (simple_base, base_typeid) = process_nested_full_expr!(sess, expr, assigns, pos);
                    bases.push(simple_base);
                    param_types.push(base_typeid);
                }

                let fnid = sess.fns.find_by_sign(fn_name, &param_types);
                let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
                simple_base = SimpleBase::FunctionCall(fnid, ret_type, bases, [StringPosition::new(), pos]);
                current_prev_pos.end_pos = pos.end_pos;
                cur_typeid = ret_type;
            }
            FullExpressionOperator::MemberFunctionCall(name, exprs, pos) => {
                let mut bases = Vec::new();
                let mut param_types = vec![cur_typeid]; // to be searched in fns like a global fn
                for expr in exprs {
                    let (simple_base, base_typeid) = process_nested_full_expr!(sess, expr, assigns, pos[1]);
                    bases.push(simple_base);
                    param_types.push(base_typeid);
                }
                let fnid = sess.fns.find_by_sign(name.clone(), &param_types);
                let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
                ops.push(SimpleOp::MemberFunctionCall(name, ret_type, bases, pos));
                current_prev_pos.end_pos = pos[1].end_pos;
                cur_typeid = ret_type;
            }
            FullExpressionOperator::MemberAccess(name, pos) => {
                match sess.types.find_by_id(cur_typeid).unwrap().find_field(&name) {
                    None => (), // TODO: emit message
                    Some(field) => {
                        let ret_type = field.typeid;
                        ops.push(SimpleOp::MemberAccess(name, ret_type, pos));
                        current_prev_pos.end_pos = pos.end_pos;
                        cur_typeid = ret_type;
                    }
                }
            }
            FullExpressionOperator::Binary(sep, pos, expr) => {
                let (right_simple_base, right_typeid) = process_nested_full_expr!(sess, expr, assigns, pos);
                let ret_type = if cur_typeid.is_valid() { // check only previous has no type error
                    // let ret_type = sess.types.check_binop_ret_type(cur_typeid, right_typeid, &sep);
                    let ret_type = ItemID::new_invalid();
                    if ret_type.is_invalid() {
                        let prev_type_desc = sess.types.fmt_by_id(cur_typeid);
                        let right_type_desc = sess.types.fmt_by_id(right_typeid);
                        sess.msgs.push(CodegenMessage::MemberNotExist{
                            prev_expr_pos: current_prev_pos, // previous part position
                            prev_type_desc: prev_type_desc,  // previous part type display name
                            op_pos: pos,                     // this op pos
                            member_name: match &sep {
                                &SeperatorKind::Mul => format!(".operator*({})", right_type_desc),
                                &SeperatorKind::Div => format!(".operator/({})", right_type_desc),
                                &SeperatorKind::Rem => format!(".operator%({})", right_type_desc),
                                &SeperatorKind::Add => format!(".operator+({})", right_type_desc),
                                &SeperatorKind::Sub => format!(".operator-({})", right_type_desc),
                                &SeperatorKind::ShiftLeft => format!(".operator<<({})", right_type_desc),
                                &SeperatorKind::ShiftRight => format!(".operator>>({})", right_type_desc),
                                &SeperatorKind::Equal => format!(".operator==({})", right_type_desc),
                                &SeperatorKind::NotEqual => format!(".operator!=({})", right_type_desc),
                                &SeperatorKind::Great => format!(".operator>({})", right_type_desc),
                                &SeperatorKind::Less => format!(".operator<({})", right_type_desc),
                                &SeperatorKind::GreatEqual => format!(".operator>=({})", right_type_desc),
                                &SeperatorKind::LessEqual => format!(".operator<=({})", right_type_desc),
                                &SeperatorKind::BitAnd => format!(".operator&({})", right_type_desc),
                                &SeperatorKind::BitOr => format!(".operator|({})", right_type_desc),
                                &SeperatorKind::BitXor => format!(".operator^({})", right_type_desc),
                                &SeperatorKind::LogicalAnd => format!(".operator&&({})", right_type_desc),
                                &SeperatorKind::LogicalOr => format!(".operator||({})", right_type_desc),
                                _ => unreachable!(), // confident about this (many many times)
                            },           // member name
                        });
                    }
                    cur_typeid = right_typeid;
                    cur_typeid
                } else {
                    ItemID::new_invalid()
                };
                ops.push(SimpleOp::BinOp(right_simple_base, BinaryOperator::from(sep), ret_type, pos));
                current_prev_pos.end_pos = pos.end_pos;
            }
            FullExpressionOperator::Unary(sep, pos) => {
                let ret_type = if cur_typeid.is_valid() { // check only previous has no type error
                    // let ret_type = sess.types.check_unop_ret_type(cur_typeid, &sep);
                    let ret_type = ItemID::new_invalid();
                    if ret_type.is_invalid() {
                        let prev_type_desc = sess.types.fmt_by_id(cur_typeid);
                        sess.msgs.push(CodegenMessage::MemberNotExist{
                            prev_expr_pos: current_prev_pos, // previous part position
                            prev_type_desc: prev_type_desc,  // previous part type display name
                            op_pos: pos,                     // this op pos
                            member_name: match &sep {
                                &SeperatorKind::Increase => format!("operator++()"),
                                &SeperatorKind::Decrease => format!("operator--()"),
                                &SeperatorKind::BitNot => format!("operator^()"),
                                &SeperatorKind::LogicalNot => format!("operator!()"),
                                &SeperatorKind::Sub => format!("operator-()"),
                                _ => unreachable!(), // confident about this (many many times)
                            },           // member name
                        });
                    }
                    cur_typeid = ret_type;
                    cur_typeid
                } else {
                    ItemID::new_invalid()
                };
                ops.push(SimpleOp::UnOp(UnaryOperator::from(sep), ret_type, pos));
                current_prev_pos.end_pos = pos.end_pos;
            }
            FullExpressionOperator::GetIndex(mut exprs, pos) => {
                if exprs.len() != 1 {
                    sess.msgs.push(CodegenMessage::SubscriptionOperatorParameterCountNot1{ pos: pos });
                    return None;
                }

                let (simple_base, base_typeid) = process_nested_full_expr!(sess, exprs.pop().unwrap(), assigns, pos);
                let fnid = sess.fns.find_by_sign("get_index", &vec![cur_typeid, base_typeid]);
                let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
                ops.push(SimpleOp::MemberFunctionCall("get_index".to_owned(), ret_type, vec![simple_base], [pos, StringPosition::new()]));
                current_prev_pos.end_pos = pos.end_pos;
                cur_typeid = ret_type;
            }
            FullExpressionOperator::TypeCast(smt, pos) => {
                let typeid = sess.types.get_id_by_smtype(smt, &mut sess.msgs, &mut sess.fns);
                // TODO, check this cast existence
                ops.push(SimpleOp::TypeCast(typeid, pos));
                current_prev_pos.end_pos = pos.end_pos;
                cur_typeid = typeid;
            }
        }
        is_first = false;
    }

    Some(SimpleExpr{ base: simple_base, ops: ops }) 
}

fn gen_simple_expr_base(simple_base: SimpleBase, sess: &mut GenerationSession) -> Operand {
    
    match simple_base {
        SimpleBase::Ident(varoffset, _typeid, _pos) => Operand::Stack(varoffset),
        SimpleBase::Lit(lit, _typeid, _pos) => Operand::Lit(lit),
        SimpleBase::FunctionCall(fnid, _typeid, bases, _pos) => {
            let mut ops = Vec::new();
            for base in bases {
                ops.push(gen_simple_expr_base(base, sess));
            }
            sess.codes.emit(Code::CallGlobal(fnid, ops));
            Operand::Register
        }
    }
}
fn gen_simple_expr(simple_expr: SimpleExpr, sess: &mut GenerationSession) -> Operand {

    let mut last_operand = gen_simple_expr_base(simple_expr.base, sess);

    for operator in simple_expr.ops {
        match operator {
            SimpleOp::MemberFunctionCall(name, _type, bases, _pos) => {
                let mut ops = Vec::new();
                for base in bases {
                    ops.push(gen_simple_expr_base(base, sess));
                }
                sess.codes.emit(Code::CallMember(last_operand.clone(), name, ops));
                last_operand = Operand::Register;
            }
            SimpleOp::MemberAccess(_, _type, pos) => {
                sess.msgs.push(CodegenMessage::MemberAccessNotSupportedCurrently{ pos: pos });
                last_operand = Operand::Register;
            }
            SimpleOp::TypeCast(typeid, _pos) => {
                sess.codes.emit(Code::TypeCast(last_operand, typeid));
                last_operand = Operand::Register;
            }
            SimpleOp::UnOp(sep, _typeid, _pos) => {
                sess.codes.emit(Code::Unary(last_operand, sep));
                last_operand = Operand::Register;
            }
            SimpleOp::BinOp(base, sep, _typeid, _pos) => {
                let operand = gen_simple_expr_base(base, sess);
                sess.codes.emit(Code::Binary(last_operand, sep, operand));
                last_operand = Operand::Register;
            }
        }
    }

    last_operand
}

// First simplize it by simplize_expr, then all operand is SimpleBase
// generate code for each base and add assign to generated assignments
pub fn gen_expr(expr: FullExpression, sess: &mut GenerationSession) -> Operand {
   
    let mut assigns = Vec::new();
    let simple_expr = match simplize_expr(expr, sess, &mut assigns) {
        None => return Operand::Unknown,
        Some(simple_expr) => simple_expr,
    };

    for assign in assigns {
        let operand = gen_simple_expr(assign.right, sess);
        sess.codes.emit_silent(Code::Assign(Operand::Stack(sess.vars.get_offset(assign.left)), AssignOperator::Assign, operand));
    }

    gen_simple_expr(simple_expr, sess)
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
        if left_expr.ops.len() == 0 {
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

        perrorln!("Assign branch run to here");
        let operand = gen_expr(right_expr, sess);
        sess.codes.emit_silent(Code::Assign(Operand::Stack(sess.vars.get_offset(left_varid)), AssignOperator::from(op), operand));
    }
}

#[cfg(test)] #[test]
fn gen_expr_pure_simple_test() {

    macro_rules! test_case{
        ($prog: expr, $result: expr) => (
            assert_eq!(
                check_pure_simple_expr(FullExpression::from_str($prog, 0), &mut GenerationSession::new()),
                $result
            );
        )
    }

    test_case!{ "1",
        Ok(SimpleBase::Lit(LitValue::from(1), ItemID::new(5), make_str_pos!(1, 1, 1, 1)))
    }
    test_case!{ "2u32", 
        Ok(SimpleBase::Lit(LitValue::from(2u32), ItemID::new(6), make_str_pos!(1, 1, 1, 4)))
    }
    test_case!{ "\"2f32\"", 
        Ok(SimpleBase::Lit(LitValue::from("2f32"), ItemID::new(13), make_str_pos!(1, 1, 1, 6)))
    }
    test_case!{ "1 as u32", 
        Err(FullExpression::from_str("1 as u32", 0))
    }
}

#[cfg(test)] #[test]
fn gen_expr_simple_test() {
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
                $sess.vars.try_push(Var::new($name.to_owned(), ItemID::new(1), false, StringPosition::new()), &mut $sess.types, &mut $sess.msgs);
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
fn gen_expr_practice_test() {

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

#[cfg(test)] #[test] #[ignore]
fn gen_expr_stmt_inter() {
    use std::io::stdin;

    let mut sess = GenerationSession::new();
    loop {
        let mut buf = String::new();

        perrorln!("Input:");
        match stdin().read_line(&mut buf) {
            Ok(_) => (),
            Err(_) => break,
        }

        if buf != "break\r\n" {
            match FullExpressionStatement::from_str(&buf, 0).0 {
                Some(expr_stmt) => {
                    gen_expr_stmt(expr_stmt, &mut sess, false);
                    perrorln!("Code: {}", sess.codes.dump());
                    perrorln!("Messages: {:?}", sess.msgs);
                }
                None => {
                    perrorln!("Unexpectedly failed");
                }
            }
        } else {
            break;
        }
    }
}