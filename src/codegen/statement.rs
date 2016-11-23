
// Statement generation

use common::From2;
use common::StringPosition;
use message::CodegenMessage;

use lexical::SeperatorKind;

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

use codegen::Var;
use codegen::VarID;
// use codegen::TypeDeclCollection;
// use codegen::FnCollection;
// use codegen::VarCollection;
// use codegen::CodeCollection;
use codegen::session::GenerationSession;
use codegen::expression::gen_expr;

// Just a static dispatcher
pub struct StatementGenerator{
}


// Expression
// A special ignore left const for const var declaration generation
fn generate_expr_stmt(_expr_stmt: ExpressionStatement, _sess: &mut GenerationSession, _ignore_left_const: bool) {
    
}

// VarDeclaration
// pub struct VarDeclStatement {
//     pub is_const: bool,
//     pub ty: SMType,
//     pub name: String,
//     pub init_expr: Option<Expression>,
//     pub pos: [StringPosition; 4],     // position for 'const' or 'var', name, assign, and semicolon
fn generate_var_decl(var_decl: VarDeclStatement, sess: &mut GenerationSession) {

    let pos_all = var_decl.pub_pos_all();
    let typeid = sess.types.try_get_id(var_decl.ty, &mut sess.msgs);

    let varid = sess.vars.try_push(Var::new(var_decl.name.clone(), typeid, var_decl.is_const, pos_all), &mut sess.types, &mut sess.msgs);
    if varid.is_invalid() { return; } // name collision, ignore the statement

    if var_decl.init_expr.is_none() {
        if var_decl.is_const {
            sess.msgs.push(CodegenMessage::ConstVariableDeclareMissingInitializationExpression{ name: var_decl.name, pos: pos_all });
        }
        return;
    }

    // Has some init Expression
    generate_expr_stmt(
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
#[cfg(test)] #[test]
fn gen_stmt_var_decl() {
    perrorln!("but it passes compile");
}

// Block
impl StatementGenerator {

    pub fn generate(block: Block, sess: &mut GenerationSession) {

        for stmt in block.stmts {
            match stmt {
                Statement::VarDecl(var_decl) => generate_var_decl(var_decl, sess),
                _ => (),        
            }
        }
    }
}