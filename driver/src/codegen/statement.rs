
// Statement generation

use codepos::StringPosition;
use message::CodegenMessage;

use lexical::SeperatorKind;
use lexical::LitValue;
use lexical::NumLitValue;

use syntax::Block;
use syntax::Statement;
use syntax::Expression;
use syntax::LabelDef;

use syntax::VarDeclStatement;
use syntax::ReturnStatement;
use syntax::BreakStatement;
use syntax::ContinueStatement;
use syntax::ExprStatement;
use syntax::LoopStatement;
use syntax::WhileStatement;
use syntax::ForStatement;
use syntax::IfConditionBody;
use syntax::IfStatement;

use codegen::var_def::Var;
use codegen::session::GenerationSession;
use codegen::expression::gen_expr;
use codegen::expression::gen_expr_stmt;
use codegen::Operand;
use codegen::Code;
use codegen::vm_code::CodeCollection;
use codegen::ItemID;

// Just a static dispatcher
pub struct StatementGenerator{
}

// VarDeclaration
// pub struct VarDeclStatement {
//     pub is_const: bool,
//     pub ty: SMType,
//     pub name: String,
//     pub init_expr: Option<Expression>,
//     pub pos: [StringPosition; 4],     // position for 'const' or 'var', name, assign, and semicolon
fn gen_var_decl(var_decl: VarDeclStatement, sess: &mut GenerationSession) {

    let _ = var_decl;
    let _ = sess;
    // let pos_all = var_decl.pub_pos_all();
    // let typeid = sess.types.get_id_by_smtype(var_decl.ty, &mut sess.msgs, &mut sess.fns);

    // let varid = sess.vars.try_push(Var::new(var_decl.name.clone(), typeid, var_decl.is_const, pos_all), &mut sess.types, &mut sess.msgs);
    // // sess.codes.emit_silent(Code::DeclareVar(typeid));
    // if varid.is_invalid() { return; } // name collision, ignore the statement

    // if var_decl.init_expr.is_none() {
    //     if var_decl.is_const {
    //         sess.msgs.push(CodegenMessage::ConstVariableDeclareMissingInitializationExpression{ name: var_decl.name, pos: pos_all });
    //     }
    //     return;
    // }

    // // Has some init Expression
    // gen_expr_stmt(
    //     ExprStatement::new_assign(
    //         pos_all, SeperatorKind::Assign, var_decl.pos[2], 
    //         Expression::new_ident(var_decl.name, var_decl.pos[1]), 
    //         var_decl.init_expr),
    //     sess,
    //     true,
    // );
}

// pub struct IfConditionBody {
//     pub expr: Expression,
//     pub body: Block,
//     pub pos: [StringPosition; 2],   // position for if and else
// }
// pub struct IfStatement {
//     pub if_expr: Expression,
//     pub if_body: Block,
//     pub elseifs: Vec<IfConditionBody>,
//     pub else_body: Option<Block>,
//     pub pos: [StringPosition; 2],  // position for if and else
// }
/// Example
/// ``` 
/// if a { b; } else if c { d; } else if e { f; } else { g; }
/// ```
/// generate
/// a; brfalse; b; c; brfalse; d; e; brfalse; f; g;
///     to c            to e           to g 
fn gen_if(if_stmt: IfStatement, sess: &mut GenerationSession) {

    let _ = if_stmt;
    let _ = sess;
    // let if_expr_pos = if_stmt.get_if_expr().get_all_strpos();
    // let (if_expr, if_expr_typeid) = gen_expr(if_stmt.if_expr, sess);
    // if if_expr_typeid != ItemID::new(12) {
    //     sess.msgs.push(CodegenMessage::IfConditionNotBoolType {
    //         pos: if_expr_pos,
    //         actual: sess.types.fmt_by_id(if_expr_typeid),
    //     });
    //     // continue;
    // }
    // let if_goto_pos = sess.codes.emit(Code::GotoIf(if_expr, false, CodeCollection::dummy_id()));

    // gen_block(if_stmt.if_body, sess, true);
    // // sess.codes.emit_silent(Code::Goto(CodeCollection::dummy_id()));
    // let next_codeid = sess.codes.next_id();
    // sess.codes.refill_addr(if_goto_pos, next_codeid);

    // for elseif in if_stmt.elseifs {
    //     let elseif_expr_pos = elseif.expr.get_all_strpos();
    //     let (elseif_expr, elseif_expr_typeid) = gen_expr(elseif.expr, sess); 
    //     if if_expr_typeid != ItemID::new(12) {
    //         sess.msgs.push(CodegenMessage::IfConditionNotBoolType {
    //             pos: elseif_expr_pos,
    //             actual: sess.types.fmt_by_id(elseif_expr_typeid),
    //         });
    //         // continue;
    //     }
    //     let elseif_goto_pos = sess.codes.emit(Code::GotoIf(elseif_expr, false, CodeCollection::dummy_id()));
        
    //     gen_block(elseif.body, sess, true);
    //     let next_codeid = sess.codes.next_id();
    //     sess.codes.refill_addr(elseif_goto_pos, next_codeid);
    // }

    // match if_stmt.else_body {
    //     Some(else_body) => gen_block(else_body, sess, true),
    //     _ => (), // nothing
    // }   
}

fn gen_return(ret_stmt: ReturnStatement, sess: &mut GenerationSession) {

    let pos = ret_stmt.get_all_strpos();
    let (operand, typeid) = match ret_stmt.into_expr() {
        Some(expr) => gen_expr(expr, sess),
        None => (Operand::Lit(LitValue::Unit), ItemID::new(0))
    };

    if typeid != sess.loops.ret_type {
        sess.msgs.push(CodegenMessage::ReturnTypeMismatch{ 
            pos: pos,
            expect: sess.types.fmt_by_id(sess.loops.ret_type),
            actual: sess.types.fmt_by_id(typeid),
        });
    } else {
        sess.codes.emit(Code::Return(operand));
    }
}

fn gen_loop(loop_stmt: LoopStatement, sess: &mut GenerationSession) {

    let continue_addr = sess.codes.next_id();
    sess.loops.push_loop(loop_stmt.get_label().map(LabelDef::get_name).cloned(), continue_addr);

    gen_block(loop_stmt.into_body(), sess, true);
    sess.codes.emit(Code::Goto(continue_addr));
    
    let break_refill_addr = sess.codes.next_id();
    sess.loops.pop_and_refill(break_refill_addr, &mut sess.codes);
}

// pub struct WhileStatement {
//     pub expr: Expression,
//     pub body: Block,
//     pub pos: StringPosition, // while position
// }
fn gen_while(while_stmt: WhileStatement, sess: &mut GenerationSession) {

    let _ = while_stmt;
    let _ = sess;
    // let continue_addr = sess.codes.next_id();
    // let _ = gen_expr(while_stmt.expr, sess); // always reeval the expression
    // sess.loops.push_loop(None, continue_addr);

    // gen_block(while_stmt.body, sess, true);
    // sess.codes.emit(Code::Goto(continue_addr));

    // let while_refill_addr = sess.codes.next_id();
    // sess.loops.pop_and_refill(while_refill_addr, &mut sess.codes);
}

// pub struct ForStatement {
//     pub iter_name: String, 
//     pub expr_low: Expression,
//     pub expr_high: Expression,
//     pub body: Block,
//     pub pos: [StringPosition; 4]  // position for 'for', iter_name, 'in', range operator, 
// }
fn gen_for(for_stmt: ForStatement, sess: &mut GenerationSession) {

    let _ = for_stmt;
    let _ = sess;
    // // scope for iter var
    // sess.vars.push_scope();

    // let low_expr_pos = for_stmt.expr_low.get_all_strpos();
    // let (low_operand, low_typeid) = gen_expr(for_stmt.expr_low, sess);
    // if !sess.types.is_primitive_numeric(low_typeid) {
    //     sess.msgs.push(CodegenMessage::ForIteraterTypeMismatch{
    //         pos: low_expr_pos,
    //         actual: sess.types.fmt_by_id(low_typeid),
    //     });
    //     // continue, mainly for not ignoring block content
    // }
    // let iter_varid = sess.vars.try_push(Var::new(for_stmt.iter_name.clone(), low_typeid, false, for_stmt.pos[1]), &mut sess.types, &mut sess.msgs);
    // let iter_offset = sess.vars.get_offset(iter_varid);
    // sess.codes.emit_silent(Code::Store(iter_offset, low_operand));

    // let continue_addr = sess.codes.next_id(); // reveal every time
    // let high_expr_pos = for_stmt.expr_high.get_all_strpos();
    // let (high_operand, high_typeid) = gen_expr(for_stmt.expr_high, sess);
    // if high_typeid != low_typeid {
    //     sess.msgs.push(CodegenMessage::ForRangeTypeMismatch{
    //         pos: high_expr_pos,
    //         actual: sess.types.fmt_by_id(high_typeid),
    //         expect: sess.types.fmt_by_id(low_typeid),
    //     })
    // }
    // let compare_fnid = sess.fns.find_by_sign(SeperatorKind::Less, &vec![low_typeid, high_typeid]); // TODO FUTURE: currently primitive integral type has operator<, but future may not and needs check here
    // sess.codes.emit_silent(Code::Call(compare_fnid, vec![Operand::Stack(iter_offset), high_operand]));
    // sess.loops.push_loop(None, continue_addr);

    // let while_implicit_break_addr = sess.codes.emit(Code::GotoIf(Operand::Register, false, CodeCollection::dummy_id())); 
    // sess.loops.push_last_loop_break_addr(while_implicit_break_addr);

    // // TODOOOOOO: ++ is removed and I forget how to write the correct version of iter_var += 1
    // // gen_block(for_stmt.body, sess, false);
    // // let increase_fnid = sess.fns.find_by_sign(SeperatorKind::Increase, &vec![low_typeid]);
    // // sess.codes.emit_silent(Code::Call(increase_fnid, vec![Operand::Stack(iter_offset)]));
    // // sess.codes.emit(Code::Goto(continue_addr));

    // let for_refill_addr = sess.codes.next_id();
    // sess.loops.pop_and_refill(for_refill_addr, &mut sess.codes);
    // sess.vars.pop_scope();
}

fn gen_continue(continue_stmt: ContinueStatement, sess: &mut GenerationSession) {
    
    match continue_stmt.get_target() {
        Some(name) => match sess.loops.get_loop_continue_addr(&name) {
            Some(continue_addr) => sess.codes.emit_silent(Code::Goto(continue_addr)),
            None => sess.msgs.push(CodegenMessage::CannotFindLoopName{
                name: name.clone(), 
                pos: continue_stmt.get_all_strpos(),
            }),
        },
        None => match sess.loops.get_last_loop_continue_addr() {
            Some(continue_addr) => sess.codes.emit_silent(Code::Goto(continue_addr)),
            None => sess.msgs.push(CodegenMessage::JumpStatementNotInLoop{ 
                pos: continue_stmt.get_all_strpos(),
            }),
        },
    }
}
fn gen_break(break_stmt: BreakStatement, sess: &mut GenerationSession) {
    
    let break_addr = sess.codes.emit(Code::Goto(CodeCollection::dummy_id()));
    match break_stmt.get_target() {
        Some(name) => match sess.loops.push_loop_break_addr(&name, break_addr) {
            Some(()) => (),
            None => sess.msgs.push(CodegenMessage::CannotFindLoopName{
                name: name.clone(), 
                pos: break_stmt.get_all_strpos(),
            }),
        },
        None => match sess.loops.push_last_loop_break_addr(break_addr) {
            Some(()) => (),
            None => sess.msgs.push(CodegenMessage::JumpStatementNotInLoop{ 
                pos: break_stmt.get_all_strpos(),
            }),
        },
    }
}

// fn and for do not require scope enter on block
fn gen_block(block: Block, sess: &mut GenerationSession, emit_scope_barrier: bool) {

    let _ = block;
    let _ = sess;
    let _ = emit_scope_barrier;
    // if emit_scope_barrier { 
    //     sess.vars.push_scope(); 
    // }
    // for stmt in block.stmts {
    //     match stmt {
    //         Statement::VarDecl(var_decl) => gen_var_decl(var_decl, sess),
    //         Statement::Expression(expr_stmt) => gen_expr_stmt(expr_stmt, sess, false),
    //         Statement::If(if_stmt) => gen_if(if_stmt, sess),
    //         Statement::Return(ret_stmt) => gen_return(ret_stmt, sess),
    //         Statement::Break(break_stmt) => gen_break(break_stmt, sess),
    //         Statement::Continue(continue_stmt) => gen_continue(continue_stmt, sess),
    //         Statement::For(for_stmt) => gen_for(for_stmt, sess),
    //         Statement::Loop(loop_stmt) => gen_loop(loop_stmt, sess),
    //         Statement::While(while_stmt) => gen_while(while_stmt, sess),
    //         Statement::Block(_block_stmt) => (), // gen_block(block.get_body(), sess, true),   // ignore to pass compile
    //     }
    // }
    // if emit_scope_barrier { 
    //     sess.vars.pop_scope(); 
    // }
}

// Block
impl StatementGenerator {

    pub fn generate(block: Block, sess: &mut GenerationSession) {
        gen_block(block, sess, false);
        sess.codes.emit(Code::Return(Operand::Lit(LitValue::Unit))); // a `return ();` at fn end if no return is provided
    }
}