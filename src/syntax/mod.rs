
// Syntax, form abstract syntax tree
// TODO far: Add error recovery, which need new structure of message emitter and symbol length
// TODO far: Add support for semantic error, which need postition information in ast items
// TODO: to support name resolve, statements in block should have i32 id, because same statement at different position is not same

mod ast_item;
mod scope;

use lexical::Lexer;

use syntax::ast_item::IASTItem;
pub use syntax::ast_item::program::Program;
pub use syntax::ast_item::function_def::FunctionDef;
pub use syntax::ast_item::expression::Expression;
pub use syntax::ast_item::statement::Statement;
pub use syntax::ast_item::smtype::SMTypeBase;
pub use syntax::ast_item::smtype::SMType;
pub use syntax::ast_item::block::Block;

pub fn get_ast(lexer: &mut Lexer) -> Option<Program> {

    Program::parse(lexer, 0).0
}

#[cfg(test)]
mod tests {

    #[test]
    fn program_get_main() {
        // use super::Function;
        // use super::Program;

        // let mut program = Program{ functions: Vec::new() };
        // program.functions.push(Function{ name: "123".to_owned() });
        // program.functions.push(Function{ name: "main".to_owned() });
        // program.functions.push(Function{ name: "456".to_owned() });

        // match program.get_main() {
        //     Some(main_function) => assert_eq!(main_function.name, "main"),
        //     None => panic!("Not got main function"),
        // }
    }

    #[test]
    fn ast_hello_world() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use super::get_ast;

        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test(r#"fn main() { println("helloworld"); }"#, messages);
        let program = get_ast(lexer);

        perrorln!("program: {:?}", program);
        perrorln!("messages: {:?}", lexer.messages())
    }
}

// Designment
// First there is syntax definition, then there is syntax diagram and confirm there is no collision, then
// syntax diagram has init nodes and end nodes, in practice, end nodes are always EOF
// 3 basic structures of syntax diagram
// Normal:   A ---->----- B    , where A and B is lexical token
// Branch:   A ------>--- B  
//                \___>__ C
//                 \__>__ D
// Revert/Recursion:   /------<-----\
//                     A ----->----- B
// Then new a state machine, 
// init state expecting the start node of the syntax diagram
// for normal syntax diagram edges, just expecting next token, if not, error, unrecoverable error
// for branch syntax diagram edges, the first tokens of B, C, D is not same, expecting them, if not expected, error, unrecoverable error
// for recursion, it is normal

// In this implementation, the syntax diagram is devide into sub diagrams
// and previous tokens may be non final tokens
// if meet first token of the subtype, just go into the subtype, if unexpected in the subtype, then it is unrecoverable error

// recoverable, recoverable is actually another syntax rule with emitting messages
