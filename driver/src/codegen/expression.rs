#![allow(unused_assignments)] // You don't see this line
// Expression generator

use std::fmt;

use lexical_pos::StringPosition;
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
use codegen::Type;
use codegen::TypeCollection;
use codegen::Operand;
use codegen::Code;
use codegen::FnName;
use codegen::FnImpl;
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
    FunctionCall(ItemID, ItemID, Vec<SimpleBase>, [StringPosition; 2]),  // fnid, ret type id
    ArrayDef(ItemID, ItemID, usize, ItemID, Vec<SimpleBase>),            // ctor fnid, push fnid, temp var offset, ret typeid, init items
    // array dup def and tuple def not here because tuple def only generates one code and a SimpleBase::FunctionCall is enough 
}
impl fmt::Debug for SimpleBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SimpleBase::Lit(ref lit, ref typeid, ref pos) => write!(f, "{:?} @ {:?}, typeid = {:?}", lit, pos, typeid),
            SimpleBase::Ident(ref id, ref typeid, ref pos) => write!(f, "{:?} @ {:?}, typeid = {:?}", id, pos, typeid),
            SimpleBase::FunctionCall(ref name, ref typeid, ref params, ref pos) => 
                write!(f, "call {:?}, {} @ {:?}, ret typeid = {:?}", name, format_vector_debug(params, ", "), pos, typeid),
            SimpleBase::ArrayDef(ref ctor_fnid, ref push_fnid, ref temp_offset, ref ret_typeid, ref params) => 
                write!(f, "array def, ctor: {:?}, push: {:?}, temp_offset: {} init: {}, ret typeid = {:?}", ctor_fnid, push_fnid, temp_offset, format_vector_debug(params, ", "), ret_typeid),
        }
    }
}
impl SimpleBase {
    fn final_typeid(&self) -> ItemID {
        match *self {
            SimpleBase::Lit(ref _lit_val, ref typeid, ref _pos) => *typeid,
            SimpleBase::Ident(ref _offset, ref typeid, ref _pos) => *typeid,
            SimpleBase::FunctionCall(ref _name, ref typeid, ref _params, ref _pos) => *typeid,
            SimpleBase::ArrayDef(ref _ctor_fnid, ref _push_fnid, ref _temp_offset, ref ret_typeid, ref _params) => *ret_typeid,
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
enum SimpleOp {
    MemberFunctionCall(ItemID, ItemID, Vec<SimpleBase>, [StringPosition; 2]), // fn id, ret type id, arguments, mystery pos
    MemberAccess(usize, ItemID, StringPosition),                              // field offset, ret type
}
impl SimpleOp {
    fn final_typeid(&self) -> ItemID {
        match *self {
            SimpleOp::MemberFunctionCall(ref _fnid, ref typeid, ref _params, ref _pos) => *typeid,
            SimpleOp::MemberAccess(ref _field_offset, ref typeid, ref _pos) => *typeid,
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

// Simplize, 
// simplize_expr is a recursive call to convert syntax expression to codegen expression (simple expr)
// while the recursion is complex and devided in 2, simplize_expr only calls process_nested_full_expr and process_nested_full_expr only calls simple_expr
// while the recursion becomes more complex and simple_expr devided into 2, simplize_expr_base and simplize_expr_ops
fn process_nested_full_expr(full_expr: FullExpression, sess: &mut GenerationSession, assigns: &mut Vec<SimpleAssignment>) -> Option<SimpleBase> {

   match (full_expr.base.as_ref().clone(), full_expr.ops.len()) {
        (FullExpressionBase::Lit(lit, pos), 0) => {
            let lit_typeid = TypeCollection::get_id_by_lit(&lit);
            Some(SimpleBase::Lit(lit, lit_typeid, pos))
        } 
        (FullExpressionBase::Ident(name, pos), 0) => {
            let ident_varid = sess.vars.find_by_name(&name);
            let ident_typeid = sess.vars.get_type(ident_varid);
            let ident_offset = sess.vars.get_offset(ident_varid);
            Some(SimpleBase::Ident(ident_offset, ident_typeid, pos))
        }
        (_, _) => {
            let simple_expr = match simplize_expr(full_expr, sess, assigns) {
                Some(simple_expr) => simple_expr,
                None => return None, // none for none
            };
            let simple_expr_typeid = simple_expr.final_typeid();
            let temp_varid = sess.vars.push_temp(simple_expr_typeid, false, &mut sess.types, &mut sess.msgs); // cannot be error
            let temp_varoffset = sess.vars.get_offset(temp_varid);
            assigns.push(SimpleAssignment{ left: temp_varid, right: simple_expr });
            Some(SimpleBase::Ident(temp_varoffset, simple_expr_typeid, StringPosition::new()))
        }
    }
}

// if function call, need another to record fn name because it (may) cannot be found in vars, may means local var shadow the global fn
fn simplize_expr_base(expr_base: FullExpressionBase, sess: &mut GenerationSession, assigns: &mut Vec<SimpleAssignment>) 
    -> (Option<SimpleBase>, Option<String>, ItemID, StringPosition) {  // simple_base, maybe_ident_name, current typeid, current prev pos

    let current_prev_pos = expr_base.pos();

    match expr_base {
        FullExpressionBase::Lit(lit, pos) => {             // `1`
            let lit_typeid = TypeCollection::get_id_by_lit(&lit);
            (Some(SimpleBase::Lit(lit, lit_typeid, pos)), None, lit_typeid, current_prev_pos)
        }
        FullExpressionBase::Ident(name, pos) => {          // `a`
            let ident_varid = sess.vars.find_by_name(&name);
            let ident_offset = sess.vars.get_offset(ident_varid);
            let ident_typeid = sess.vars.get_type(ident_varid);
            (Some(SimpleBase::Ident(ident_offset, ident_typeid, pos)), Some(name), ident_typeid, current_prev_pos)
        }
        FullExpressionBase::Paren(expr, _pos) => { // directly move to temp assignment
            match process_nested_full_expr(expr, sess, assigns) {
                None => (None, None, ItemID::new_invalid(), current_prev_pos),
                Some(simple_base) => {
                    let simple_base_typeid = simple_base.final_typeid();
                    (Some(simple_base), None, simple_base_typeid, current_prev_pos)
                }
            }
        }

        // convert expr1 and expr2 to simple base and act like a global call to ?new_array
        FullExpressionBase::ArrayDupDef(expr1, expr2, pos) => {
            
            let expr2_pos = expr2.pub_pos_all();
            match process_nested_full_expr(expr1, sess, assigns) {
                None => (None, None, ItemID::new_invalid(), current_prev_pos),
                Some(simple_base1) => match process_nested_full_expr(expr2, sess, assigns) {
                    None => (None, None, ItemID::new_invalid(), current_prev_pos),
                    Some(simple_base2) => {
                        let simple_base_typeid1 = simple_base1.final_typeid(); // should not be invalid
                        let simple_base_typeid2 = simple_base2.final_typeid(); // should not be invalid
                        let array_typeid = sess.types.push_builtin_template_type(Type::Array(simple_base_typeid1.as_option().unwrap()), &mut sess.fns);
                        let ctor_fnid = sess.fns.find_by_sign("?new_array", &vec![simple_base_typeid1, simple_base_typeid2]);
                        
                        if ctor_fnid.is_invalid() {
                            sess.msgs.push(CodegenMessage::ArrayDupDefSecondParameterTypeNotExpected{ 
                                actual: sess.types.fmt_by_id(simple_base_typeid2), 
                                pos: expr2_pos 
                            });
                            return (None, None, ItemID::new_invalid(), current_prev_pos);
                        }
                        (Some(SimpleBase::FunctionCall(ctor_fnid, array_typeid, vec![simple_base1, simple_base2], pos)), None, array_typeid, current_prev_pos)
                    }
                }
            }
        }
        FullExpressionBase::TupleDef(exprs, pos) => {

            let mut simple_bases = Vec::new();
            let mut item_types = Vec::new();      // Vec<TypeID>
            let mut real_item_types = Vec::new(); // Vec<usize> for construct Type::Tuple
            let mut has_failed = false;
            for expr in exprs {
                match process_nested_full_expr(expr, sess, assigns) {
                    Some(simple_base) => {
                        let simple_base_typeid = simple_base.final_typeid(); // should not be invalid
                        let real_typeid = simple_base_typeid.as_option().unwrap();
                        item_types.push(simple_base_typeid);
                        real_item_types.push(real_typeid);
                        simple_bases.push(simple_base);
                    }
                    None => {
                        has_failed = true;
                        continue; 
                    }
                }
            }
            if !has_failed {
                let tuple_typeid = sess.types.push_builtin_template_type(Type::Tuple(real_item_types), &mut sess.fns);
                let ctor_fnid = sess.fns.find_by_sign("?new_tuple", &item_types);
                (Some(SimpleBase::FunctionCall(ctor_fnid, tuple_typeid, simple_bases, [pos, pos])), None, tuple_typeid, current_prev_pos)
            } else {
                (None, None, ItemID::new_invalid(), current_prev_pos)
            }
        }

        // check item expr type, process all, then, if any type mismatch, return None
        // get typeid of the array, after got, the ctor and member fns of the instantiated array type are already prepared
        // return SimpleBase::ArrayDef because it emits multi code
        FullExpressionBase::ArrayDef(exprs, _pos) => {

            let mut simple_bases = Vec::new();
            let mut item_typeid = ItemID::new_invalid(); 
            let mut is_first = true;
            let mut has_failed = false;
            for expr in exprs {
                let expr_pos = expr.pub_pos_all();
                match process_nested_full_expr(expr, sess, assigns) {
                    None => {
                        has_failed = true;
                        continue; // just ignore the invalid full_expr
                    }
                    Some(item_simple_base) => {
                        let this_item_typeid = item_simple_base.final_typeid();
                        if is_first {
                            item_typeid = this_item_typeid;
                        } else {
                            // only emit message when is valid, that is, if first expr of array init expr is invalid
                            // then the type of the array is unknown, except for error about the first expr item, no more messages about the whole expr
                            if item_typeid.is_valid() {
                                if item_typeid != this_item_typeid {
                                    sess.msgs.push(CodegenMessage::ArrayInitializeElementNotSameType{ 
                                        expect: sess.types.fmt_by_id(item_typeid), 
                                        actual: sess.types.fmt_by_id(this_item_typeid), 
                                        pos: expr_pos
                                    });
                                    has_failed = true;
                                    continue; // ignore invalid type full_expr
                                }
                            } else {
                                has_failed = true;
                                continue;     // if first init expr is invalid, also no more actions on it
                            }
                        }
                        simple_bases.push(item_simple_base);
                    }
                }
                is_first = false;
            }

            if !has_failed {
                let real_item_typeid = item_typeid.as_option().unwrap(); // if no fail, this should work
                let array_typeid = sess.types.push_builtin_template_type(Type::Array(real_item_typeid), &mut sess.fns);
                let ctor_fnid = sess.fns.find_by_sign(format!("?new_array_{}", real_item_typeid), &Vec::new());
                let push_fnid = sess.fns.find_by_sign("push", &vec![array_typeid, item_typeid]);
                let temp_varid = sess.vars.push_temp(array_typeid, false, &mut sess.types, &mut sess.msgs);
                let temp_offset = sess.vars.get_offset(temp_varid);
                (Some(SimpleBase::ArrayDef(ctor_fnid, push_fnid, temp_offset, array_typeid, simple_bases)), None, array_typeid, current_prev_pos)
            } else {
                (None, None, ItemID::new_invalid(), current_prev_pos)
            }
        }

    }
}
fn simplize_expr(expr: FullExpression, sess: &mut GenerationSession, assigns: &mut Vec<SimpleAssignment>) -> Option<SimpleExpr> {
   
    macro_rules! process_member_call {
        ($sess: expr, $exprs: expr, $fn_name: expr, $assigns: expr, $ops: expr, $pos: expr, $current_typeid: expr, $current_prev_pos: expr) => ({
            let mut bases = Vec::new();
            let mut param_types = vec![$current_typeid]; // to be searched in fns like a global fn
            let mut has_failed = false;
            for expr in $exprs {
                match process_nested_full_expr(expr, $sess, $assigns) {
                    Some(simple_base) => {
                        let simple_base_typeid = simple_base.final_typeid();
                        bases.push(simple_base);
                        param_types.push(simple_base_typeid);
                    }
                    None => {
                        has_failed = true;
                        continue;
                    }
                }
            }

            if !has_failed {
                let fnid = $sess.fns.find_by_sign($fn_name.clone(), &param_types);
                if fnid.is_invalid() {
                    $sess.msgs.push(CodegenMessage::FunctionNotDefined{ 
                        sign: FnImpl::fmt_display_sign_temp($fn_name, &param_types, &sess.types),
                        pos: $pos, 
                    });
                    continue;
                }
                let ret_typeid = $sess.fns.find_by_id(fnid).unwrap().ret_type;
                $ops.push(SimpleOp::MemberFunctionCall(fnid, ret_typeid, bases, [$pos, $pos]));
                $current_prev_pos.end_pos = $pos.end_pos;
                $current_typeid = ret_typeid;
            }
        })
    }

    let (maybe_simple_base, mut maybe_ident_name, mut current_typeid, mut current_prev_pos) = simplize_expr_base(expr.base.as_ref().clone(), sess, assigns);
    let mut simple_base = match maybe_simple_base {
        Some(simple_base) => simple_base,
        None => return None, // type cannot be judged and members type cannot be checked, no more things to do
    };

    let mut ops = Vec::new();
    let mut is_first = true;
    for op in expr.ops {
        match op {
            FullExpressionOperator::FunctionCall(exprs, pos) => {
                let fn_name = if !is_first || maybe_ident_name.is_none() {
                    sess.msgs.push(CodegenMessage::FunctionCallOperatorNotAppliedToIdentifier{ pos: pos });
                    return None;
                } else {
                    let ident_name = maybe_ident_name.clone().unwrap();
                    maybe_ident_name = None;
                    ident_name
                };

                let mut bases = Vec::new();
                let mut param_types = Vec::new();
                let mut has_failed = false;
                for expr in exprs {
                    match process_nested_full_expr(expr, sess, assigns) {
                        Some(simple_base) => {
                            let simple_base_typeid = simple_base.final_typeid();
                            bases.push(simple_base);
                            param_types.push(simple_base_typeid);
                        }
                        None => {
                            has_failed = true;
                            continue;
                        }
                    }
                }

                if !has_failed {
                    let fnid = sess.fns.find_by_sign(fn_name.clone(), &param_types);
                    if fnid.is_invalid() {
                        sess.msgs.push(CodegenMessage::FunctionNotDefined{ 
                            sign: FnImpl::fmt_display_sign_temp(fn_name, &param_types, &sess.types),
                            pos: pos, 
                        });
                        continue;
                    }
                    let ret_type = sess.fns.find_by_id(fnid).unwrap().ret_type;
                    simple_base = SimpleBase::FunctionCall(fnid, ret_type, bases, [pos, pos]);
                    current_typeid = ret_type;
                    current_prev_pos.end_pos = pos.end_pos;
                }
            }
            FullExpressionOperator::MemberAccess(name, pos) => {
                match sess.types.find_by_id(current_typeid).unwrap().find_field(&name) {
                    None => (), // TODO: emit message
                    Some(field) => {
                        let ret_type = field.typeid;
                        let offset = field.offset;
                        ops.push(SimpleOp::MemberAccess(offset, ret_type, pos));
                        current_prev_pos.end_pos = pos.end_pos;
                        current_typeid = ret_type;
                    }
                }
            }
            FullExpressionOperator::MemberFunctionCall(name, exprs, pos) => {
                process_member_call!(sess, exprs, name, assigns, ops, pos[0], current_typeid, current_prev_pos);
            }
            FullExpressionOperator::Binary(sep, pos, expr) => {
                process_member_call!(sess, vec![expr], sep, assigns, ops, pos, current_typeid, current_prev_pos);
            }
            FullExpressionOperator::Unary(sep, pos) => {
                process_member_call!(sess, Vec::new(), sep, assigns, ops, pos, current_typeid, current_prev_pos);
            }
            FullExpressionOperator::GetIndex(exprs, pos) => {
                process_member_call!(sess, exprs, "get_index", assigns, ops, pos, current_typeid, current_prev_pos);
            }
            FullExpressionOperator::TypeCast(smt, pos) => {
                let typeid = sess.types.get_id_by_smtype(smt, &mut sess.msgs, &mut sess.fns);
                match typeid.as_option() {
                    Some(typeid) => {
                        process_member_call!(sess, Vec::new(), typeid, assigns, ops, pos, current_typeid, current_prev_pos);
                    }
                    None => (), // message maybe emitted
                } 
            }
        }

        if is_first && maybe_ident_name.is_some() { // is first and maybe ident is not used by fn call, check varid invalid
            match simple_base {
                SimpleBase::Ident(ref _offset, ref ret_type, ref pos) => {
                    if ret_type.is_invalid() {
                        sess.msgs.push(CodegenMessage::IdentNotDeclared{ name: maybe_ident_name.unwrap(), pos: *pos });
                    }
                }
                _ => unreachable!()
            }
            is_first = false;
            maybe_ident_name = None;
        }
    }

    Some(SimpleExpr{ base: simple_base, ops: ops }) 
}

// operand and its type
fn gen_simple_expr_base(simple_base: SimpleBase, sess: &mut GenerationSession) -> (Operand, ItemID) {
    
    match simple_base {
        SimpleBase::Lit(lit, typeid, _pos) => (Operand::Lit(lit), typeid),
        SimpleBase::Ident(varoffset, typeid, _pos) => (Operand::Stack(varoffset), typeid),
        SimpleBase::FunctionCall(fnid, typeid, bases, _pos) => {
            let mut ops = Vec::new();
            for base in bases {
                ops.push(gen_simple_expr_base(base, sess).0);
            }
            sess.codes.emit_silent(Code::Call(fnid, ops));
            (Operand::Register, typeid)
        }
        SimpleBase::ArrayDef(ctor_fnid, push_fnid, temp_varoffset, array_typeid, bases) => {
            sess.codes.emit_silent(Code::Call(ctor_fnid, Vec::new()));
            let array_operand = Operand::Stack(temp_varoffset);
            sess.codes.emit_silent(Code::Store(temp_varoffset, Operand::Register));
            for base in bases {
                let (right_operand, _right_typeid) = gen_simple_expr_base(base, sess);
                sess.codes.emit(Code::Call(push_fnid, vec![array_operand.clone(), right_operand]));
            }
            (array_operand, array_typeid)
        }
    }
}
fn gen_simple_expr(simple_expr: SimpleExpr, sess: &mut GenerationSession) -> (Operand, ItemID) {

    let (mut last_operand, mut last_typeid) = gen_simple_expr_base(simple_expr.base, sess);

    for operator in simple_expr.ops {
        match operator {
            SimpleOp::MemberFunctionCall(fnid, ret_typeid, bases, _pos) => {
                let mut ops = vec![last_operand.clone()];
                for base in bases {
                    ops.push(gen_simple_expr_base(base, sess).0);
                }
                sess.codes.emit(Code::Call(fnid, ops));
                last_operand = Operand::Register;
                last_typeid = ret_typeid;
            }
            SimpleOp::MemberAccess(field_id, typeid, _pos) => {
                sess.codes.emit_silent(Code::FieldAccess(last_operand.clone(), field_id));
                last_operand = Operand::Register;
                last_typeid = typeid;
            }
        }
    }

    (last_operand, last_typeid)
}

// First simplize it by simplize_expr, then all operand is SimpleBase
// generate code for each base and add assign to generated assignments
pub fn gen_expr(expr: FullExpression, sess: &mut GenerationSession) -> (Operand, ItemID) {
   
    let mut assigns = Vec::new();
    match simplize_expr(expr, sess, &mut assigns) {
        None => (Operand::Unknown, ItemID::new_invalid()),
        Some(simple_expr) => {
            for assign in assigns {
                let (operand, _typeid) = gen_simple_expr(assign.right, sess);
                sess.codes.emit_silent(Code::Store(sess.vars.get_offset(assign.left), operand));
            }
            gen_simple_expr(simple_expr, sess)
        }
    }
}

fn assignment_seperator_to_seperator(sep: SeperatorKind) -> Option<SeperatorKind> {
    match sep {
        SeperatorKind::AddAssign => Some(SeperatorKind::Add),
        SeperatorKind::SubAssign => Some(SeperatorKind::Sub),
        SeperatorKind::MulAssign => Some(SeperatorKind::Mul),
        SeperatorKind::DivAssign => Some(SeperatorKind::Div),
        SeperatorKind::RemAssign => Some(SeperatorKind::Rem),
        SeperatorKind::BitAndAssign => Some(SeperatorKind::BitAnd),
        SeperatorKind::BitOrAssign => Some(SeperatorKind::BitOr),
        SeperatorKind::BitXorAssign => Some(SeperatorKind::BitXor),
        _ => None,
    }
}
// Currently, valid expression statement, is one of
// Assignment or OpAssignment statement, left of assign operator is identifier
// single expression statement, one of
//     last of ops is function call or member function call
//     last of ops is increase or decrease
pub fn gen_expr_stmt(expr_stmt: FullExpressionStatement, sess: &mut GenerationSession, ignore_left_const: bool) {

    if expr_stmt.op.is_none() {          // expression statement
        let left_expr = expr_stmt.left_expr;
        let pos_semicolon = expr_stmt.pos[1];

        if left_expr.ops.len() == 0 {
            sess.msgs.push(CodegenMessage::InvalidExpressionStatementSingleSimpleExpression{ pos: StringPosition::from2(left_expr.pub_pos_all().start_pos, pos_semicolon.end_pos) });
            return;
        }
        
        match left_expr.ops.iter().last().unwrap() {
            &FullExpressionOperator::FunctionCall(_, _) 
            | &FullExpressionOperator::MemberFunctionCall(_, _, _) => (),
            &FullExpressionOperator::Unary(SeperatorKind::Increase, _)
            | &FullExpressionOperator::Unary(SeperatorKind::Decrease, _) => {
                let mut left_ident_name = None;
                if left_expr.ops.len() == 1 { // only ++a is acceptable, ++a[10] is not
                    match left_expr.base.as_ref() {
                        &FullExpressionBase::Ident(ref name, _) => left_ident_name = Some(name.clone()),
                        _ => (),
                    }
                }
                if left_ident_name.is_none() {
                    sess.msgs.push(CodegenMessage::LeftOfAssignmentStatementCannotBeComplex{ pos: left_expr.pub_pos_all() });
                    return;
                }
            }
            _ => {
                sess.msgs.push(CodegenMessage::InvalidExpressionStatementLastOpNotValid{ pos: StringPosition::from2(left_expr.pub_pos_all().start_pos, pos_semicolon.end_pos) });
                return;
            }
        }

        let _maybe_codeop = gen_expr(left_expr, sess); // ignore result
    } else {               // assignment statement
        let left_expr = expr_stmt.left_expr;
        let assign_op_pos = expr_stmt.pos[0];
        let semicolon_pos = expr_stmt.pos[1];
        let left_expr_pos = left_expr.pub_pos_all();

        // Currently can only be single identifier
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
        let left_typeid = sess.vars.get_type(left_varid);
        let left_offset = sess.vars.get_offset(left_varid);
        let op = expr_stmt.op.unwrap();
        let right_expr = expr_stmt.right_expr.unwrap();

        // check assign able
        match sess.vars.find_by_id(left_varid) {
            Some(ref var) => if var.is_const && !ignore_left_const { // is const but not ignore const
                sess.msgs.push(CodegenMessage::AssignToConstVar{
                    name: var.name.clone(),
                    pos: StringPosition::from2(left_expr_pos.start_pos, semicolon_pos.end_pos),
                });
            },
            None => (), // message emitted
        }

        let maybe_op = assignment_seperator_to_seperator(op);
        let mut assigns = Vec::new();
        match simplize_expr(right_expr, sess, &mut assigns) {
            None => (), // msg emitted
            Some(right_simple_expr) => {
                let right_typeid = right_simple_expr.final_typeid();

                for assign in assigns {
                    let (operand, _typeid) = gen_simple_expr(assign.right, sess);
                    sess.codes.emit_silent(Code::Store(sess.vars.get_offset(assign.left), operand));
                }
                
                if maybe_op.is_some() { // some xxassignment
                    let op = maybe_op.unwrap();
                    let fnid = sess.fns.find_by_sign(op.clone(), &vec![left_typeid, right_typeid]);
                    if fnid.is_invalid() {
                        sess.msgs.push(CodegenMessage::FunctionNotDefined{ 
                            sign: FnImpl::fmt_display_sign_temp(op.clone(), &vec![left_typeid, right_typeid], &sess.types),
                            pos: assign_op_pos, 
                        });
                        return;
                    }

                    let (right_operand, _right_typeid) = gen_simple_expr(right_simple_expr, sess);
                    sess.codes.emit_silent(Code::Call(fnid, vec![Operand::Stack(left_offset), right_operand]));
                    sess.codes.emit_silent(Code::Store(left_offset, Operand::Register));
                } else { // direct assignment
                    if left_typeid != right_typeid {
                        sess.msgs.push(CodegenMessage::AssignmentTypeMismatch{
                            pos: assign_op_pos,
                            left_desc: sess.types.fmt_by_id(left_typeid),
                            right_desc: sess.types.fmt_by_id(right_typeid),
                        });
                        return;
                    }
                    let (right_operand, _right_typeid) = gen_simple_expr(right_simple_expr, sess);
                    sess.codes.emit_silent(Code::Store(left_offset, right_operand));
                }
            }
        }
    }
}

#[cfg(test)] #[test]
fn gen_expr_pure_simple_test() {

    // macro_rules! test_case{
    //     ($prog: expr, $result: expr) => (
    //         assert_eq!(
    //             check_pure_simple_expr(FullExpression::from_str($prog, 0), &mut GenerationSession::new()),
    //             $result
    //         );
    //     )
    // }

    // test_case!{ "1",
    //     Ok(SimpleBase::Lit(LitValue::from(1), ItemID::new(5), make_str_pos!(1, 1, 1, 1)))
    // }
    // test_case!{ "2u32", 
    //     Ok(SimpleBase::Lit(LitValue::from(2u32), ItemID::new(6), make_str_pos!(1, 1, 1, 4)))
    // }
    // test_case!{ "\"2f32\"", 
    //     Ok(SimpleBase::Lit(LitValue::from("2f32"), ItemID::new(13), make_str_pos!(1, 1, 1, 6)))
    // }
    // test_case!{ "1 as u32", 
    //     Err(FullExpression::from_str("1 as u32", 0))
    // }
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
    let _final_expr = simplize_expr(expr, &mut sess, &mut assigns);
    let _expect_message = &mut MessageEmitter::new();
    // assert_eq!(final_expr, None);
    // assert_eq!(&sess.msgs, expect_message);

    let expr = FullExpression::from_str("1[]", 0);
    let mut sess = GenerationSession::new();
    let mut assigns = Vec::new();
    let _final_expr = simplize_expr(expr, &mut sess, &mut assigns);
    let _expect_message = &mut MessageEmitter::new();
    // assert_eq!(final_expr, None);
    // assert_eq!(&sess.msgs, expect_message);
    
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
    let _final_expr = simplize_expr(expr, &mut sess, &mut assigns);

    // assert_eq!(sess.vars.len(), 15);
    // perrorln!("assigns: \n{}", format_vector_debug(&assigns, "\n"));
    // perrorln!("final expr: \n{:?}", final_expr);
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
                    perrorln!("Messages: {:?}", sess.msgs);
                }
            }
        } else {
            break;
        }
    }
}

// Issues
// fn main() { a = 1; }  // not reporting ident not declared error
// fn main() { const string a = "helloworld"; writeln("a[2] = " + a[2].to_string()); } // OK
// fn main() { const [u32] a = [1u32, 2u32, 5u32]; a.set_index(2, 5u32); writeln((a[0] + a[1] + a[2] + 5 as u32).to_string()); } // not OK
// fn main() { const [u32] a = [1u32, 2u32, (1  + 1) as u32]; a.set_index(2, 5u32); writeln((a[0] + a[1] + a[2] + 5 as u32).to_string()); }
// fn main() { const i32 a = ("123", '1', 5, true).item2; writeln(a.to_string()); } // Ok
// fn main() { const i32 a = 1; ++a; }