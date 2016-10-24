
// Validate AST
// combine to get_index, set_index, op_set_index as Operations
// deny incorrect expression statements
// TODO far: insert warnings on dead code, because control flow dead code are more convenient to find in AST

use message::MessageEmitter;

use syntax::Program;



pub fn validate_ast(program: &Program, messages: &mut MessageEmitter) -> bool {
    true
}