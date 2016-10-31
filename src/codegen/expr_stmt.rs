
// Expand expr stmt to vmcodes 
// if and while's expression are first change to 'somelocal = the expr; while ...' and come to here

use syntax::Expression;
use syntax::ExpressionStatement;

use codegen::vm_code::VMCode;
use codegen::gener::CodeGenerater;

fn expr_expand(_gener: &mut CodeGenerater, _expr: Expression) -> Vec<VMCode> {
    Vec::new()
}

pub fn expr_stmt_expand(_gener: &mut CodeGenerater, _expr_stmt: &ExpressionStatement) -> Vec<VMCode> {
    
    // match expr_stmt.op {
    //     (Some(assign_op)) => {

    //     }
    // }

    Vec::new()
}

#[cfg(test)]
#[test]
fn gen_expr_stmt() {
    // use syntax::FunctionDef;
    // use syntax::Statement;

    // let syn_func = &FunctionDef::from_str("fn main(i32 arg1) { var a = 1; arg1 = a * 2; }", 0);
    // let func_gener = &mut FunctionGenerater::new(syn_func);
    // if let &Statement::Expression(ref input_stmt) = &func_gener.get_syn_func().body.stmts[1] {
    //     let codes = expr_stmt_expand(func_gener, input_stmt);
    //     perrorln!("codes: {:?}", codes);
    // } else {
    //     panic!("first stmt is not expr stmt");
    // }
}