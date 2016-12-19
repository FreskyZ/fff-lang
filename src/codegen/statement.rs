
// Statement generation

use common::From2;
use common::StringPosition;
use message::CodegenMessage;

use lexical::SeperatorKind;
use lexical::LitValue;
use lexical::NumLitValue;

use syntax::Block;
use syntax::Statement;
use syntax::Expression;
use syntax::ExpressionBase;

use syntax::VarDeclStatement;
use syntax::ReturnStatement;
use syntax::BreakStatement;
use syntax::ContinueStatement;
use syntax::ExpressionStatement;
use syntax::LoopStatement;
use syntax::WhileStatement;
use syntax::ForStatement;
use syntax::ElseIfBranch;
use syntax::IfStatement;

use codegen::var_def::Var;
use codegen::session::GenerationSession;
use codegen::expression::gen_expr;
use codegen::expression::gen_expr_stmt;
use codegen::Operand;
use codegen::Code;
use codegen::vm_code::CodeCollection;
use codegen::ItemID;
use codegen::AssignOperator;
use codegen::BinaryOperator;

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

    let pos_all = var_decl.pub_pos_all();
    let typeid = sess.types.get_id_by_smtype(var_decl.ty, &mut sess.msgs, &mut sess.fns);

    let varid = sess.vars.try_push(Var::new(var_decl.name.clone(), typeid, var_decl.is_const, pos_all), &mut sess.types, &mut sess.msgs);
    sess.codes.emit_silent(Code::DeclareVar(typeid));
    if varid.is_invalid() { return; } // name collision, ignore the statement

    if var_decl.init_expr.is_none() {
        if var_decl.is_const {
            sess.msgs.push(CodegenMessage::ConstVariableDeclareMissingInitializationExpression{ name: var_decl.name, pos: pos_all });
        }
        return;
    }

    // Has some init Expression
    gen_expr_stmt(
        ExpressionStatement{
            left_expr: Expression::new(
                ExpressionBase::Ident(var_decl.name, var_decl.pos[1]),
                Vec::new(),
                var_decl.pos[1],
            ),
            op: Some(SeperatorKind::Assign),
            right_expr: var_decl.init_expr,
            pos: [var_decl.pos[2], var_decl.pos[3]],
        },
        sess,
        true,
    );
}

// pub struct ElseIfBranch {
//     pub expr: Expression,
//     pub body: Block,
//     pub pos: [StringPosition; 2],   // position for if and else
// }
// pub struct IfStatement {
//     pub if_expr: Expression,
//     pub if_body: Block,
//     pub elseifs: Vec<ElseIfBranch>,
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

    let if_expr = gen_expr(if_stmt.if_expr, sess); 
    let if_goto_pos = sess.codes.emit(Code::GotoIf(if_expr, false, CodeCollection::dummy_id()));

    gen_block(if_stmt.if_body, sess, true);
    let next_codeid = sess.codes.next_id();
    sess.codes.refill_addr(if_goto_pos, next_codeid);

    for elseif in if_stmt.elseifs {
        let elseif_expr = gen_expr(elseif.expr, sess);
        let elseif_goto_pos = sess.codes.emit(Code::GotoIf(elseif_expr, false, CodeCollection::dummy_id()));
        gen_block(elseif.body, sess, true);
        let next_codeid = sess.codes.next_id();
        sess.codes.refill_addr(elseif_goto_pos, next_codeid);
    }

    match if_stmt.else_body {
        Some(else_body) => gen_block(else_body, sess, true),
        _ => (), // nothing
    }   
}

// pub struct ReturnStatement {
//     pub expr: Option<Expression>,
//     pub pos: [StringPosition; 2], // position for return and semicolon
// }
fn gen_return(ret_stmt: ReturnStatement, sess: &mut GenerationSession) {

    let operand = match ret_stmt.expr {
        Some(expr) => gen_expr(expr, sess),
        None => Operand::Lit(LitValue::Unit),
    };

    sess.codes.emit(Code::Return(operand));
}

// pub struct LoopStatement {
//     pub name: Option<String>,
//     pub body: Block,
//     pub pos: [StringPosition; 2],   // position for 'loop' and loop name
// }
fn gen_loop(loop_stmt: LoopStatement, sess: &mut GenerationSession) {

    let continue_addr = sess.codes.next_id();
    sess.loops.push_loop(loop_stmt.name, continue_addr);

    gen_block(loop_stmt.body, sess, true);
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

    let continue_addr = sess.codes.next_id();
    let _ = gen_expr(while_stmt.expr, sess); // always reeval the expression
    sess.loops.push_loop(None, continue_addr);

    gen_block(while_stmt.body, sess, true);
    sess.codes.emit(Code::Goto(continue_addr));

    let while_refill_addr = sess.codes.next_id();
    sess.loops.pop_and_refill(while_refill_addr, &mut sess.codes);
}

// pub struct ForStatement {
//     pub iter_name: String, 
//     pub expr_low: Expression,
//     pub expr_high: Expression,
//     pub body: Block,
//     pub pos: [StringPosition; 4]  // position for 'for', iter_name, 'in', range operator, 
// }
fn gen_for(for_stmt: ForStatement, sess: &mut GenerationSession) {

    // scope for iter var
    sess.vars.push_scope();

    let iter_varid = sess.vars.try_push(Var::new(for_stmt.iter_name.clone(), ItemID::new(14), false, for_stmt.pos[1]), &mut sess.types, &mut sess.msgs);
    let iter_offset = sess.vars.get_offset(iter_varid);
    sess.codes.emit_silent(Code::DeclareVar(ItemID::new(14)));
    let operand = gen_expr(for_stmt.expr_low, sess);
    sess.codes.emit_silent(Code::Assign(Operand::Stack(iter_offset), AssignOperator::Assign, operand));

    let continue_addr = sess.codes.next_id(); // reeval every time
    let high_operand = gen_expr(for_stmt.expr_high, sess);
    sess.codes.emit_silent(Code::Binary(Operand::Stack(iter_offset), BinaryOperator::Less, high_operand));
    sess.loops.push_loop(None, continue_addr);

    let while_implicit_break_addr = sess.codes.emit(Code::GotoIf(Operand::Register, false, CodeCollection::dummy_id())); 
    sess.loops.push_last_loop_break_addr(while_implicit_break_addr);

    gen_block(for_stmt.body, sess, false);
    sess.codes.emit_silent(Code::Binary(Operand::Stack(iter_offset), BinaryOperator::Add, Operand::Lit(LitValue::from(1))));
    sess.codes.emit(Code::Goto(continue_addr));

    let for_refill_addr = sess.codes.next_id();
    sess.loops.pop_and_refill(for_refill_addr, &mut sess.codes);
    sess.vars.pop_scope();
}

// pub struct ContinueStatement {
//     pub name: Option<String>,
//     pub pos: [StringPosition; 3], // position for continue, name and semicolon
// }
fn gen_continue(continue_stmt: ContinueStatement, sess: &mut GenerationSession) {
    
    match continue_stmt.name {
        Some(name) => match sess.loops.get_loop_continue_addr(&name) {
            Some(continue_addr) => sess.codes.emit_silent(Code::Goto(continue_addr)),
            None => sess.msgs.push(CodegenMessage::CannotFindLoopName{
                name: name, 
                pos: StringPosition::from2(continue_stmt.pos[0].start_pos, continue_stmt.pos[2].end_pos) 
            }),
        },
        None => match sess.loops.get_last_loop_continue_addr() {
            Some(continue_addr) => sess.codes.emit_silent(Code::Goto(continue_addr)),
            None => sess.msgs.push(CodegenMessage::JumpStatementNotInLoop{ 
                pos: StringPosition::from2(continue_stmt.pos[0].start_pos, continue_stmt.pos[2].end_pos) 
            }),
        },
    }
}
// pub struct BreakStatement {
//     pub name: Option<String>,
//     pub pos: [StringPosition; 3], // position for break, name and semicolon
// }
fn gen_break(break_stmt: BreakStatement, sess: &mut GenerationSession) {
    
    let break_addr = sess.codes.emit(Code::Goto(CodeCollection::dummy_id()));
    match break_stmt.name {
        Some(name) => match sess.loops.push_loop_break_addr(&name, break_addr) {
            Some(()) => (),
            None => sess.msgs.push(CodegenMessage::CannotFindLoopName{
                name: name, 
                pos: StringPosition::from2(break_stmt.pos[0].start_pos, break_stmt.pos[2].end_pos) 
            }),
        },
        None => match sess.loops.push_last_loop_break_addr(break_addr) {
            Some(()) => (),
            None => sess.msgs.push(CodegenMessage::JumpStatementNotInLoop{ 
                pos: StringPosition::from2(break_stmt.pos[0].start_pos, break_stmt.pos[2].end_pos) 
            }),
        },
    }
}

// fn and for do not require scope enter on block
fn gen_block(block: Block, sess: &mut GenerationSession, emit_scope_barrier: bool) {

    if emit_scope_barrier { 
        sess.vars.push_scope(); 
    }
    for stmt in block.stmts {
        match stmt {
            Statement::VarDecl(var_decl) => gen_var_decl(var_decl, sess),
            Statement::Expression(expr_stmt) => gen_expr_stmt(expr_stmt, sess, false),
            Statement::If(if_stmt) => gen_if(if_stmt, sess),
            Statement::Return(ret_stmt) => gen_return(ret_stmt, sess),
            Statement::Break(break_stmt) => gen_break(break_stmt, sess),
            Statement::Continue(continue_stmt) => gen_continue(continue_stmt, sess),
            Statement::For(for_stmt) => gen_for(for_stmt, sess),
            Statement::Loop(loop_stmt) => gen_loop(loop_stmt, sess),
            Statement::While(while_stmt) => gen_while(while_stmt, sess),
            Statement::Block(block) => gen_block(block, sess, true),        
        }
    }
    if emit_scope_barrier { 
        sess.vars.pop_scope(); 
    }
}

// Block
impl StatementGenerator {

    pub fn generate(block: Block, sess: &mut GenerationSession) {
        gen_block(block, sess, false);
        sess.codes.emit(Code::Return(Operand::Lit(LitValue::Unit))); // a `return ();` at fn end if no return is provided;
    }
}