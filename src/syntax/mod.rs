
// Syntax, form abstract syntax tree
// TODO: Add error recovery, which need new structure of message emitter and symbol length

#[macro_use]
mod ast_item;
mod program;
mod function_def;
mod smtype;
mod statement;
mod block;
mod expression;

pub use crate::syntax::program::Program;
pub use crate::syntax::function_def::Argument;
pub use crate::syntax::function_def::FunctionDef;
pub use crate::syntax::expression::Expression;
pub use crate::syntax::expression::ExpressionBase;
pub use crate::syntax::expression::ExpressionOperator;
pub use crate::syntax::statement::Statement;
pub use crate::syntax::statement::VarDeclStatement;
pub use crate::syntax::statement::ReturnStatement;
pub use crate::syntax::statement::BreakStatement;
pub use crate::syntax::statement::ContinueStatement;
pub use crate::syntax::statement::ExpressionStatement;
pub use crate::syntax::statement::LoopStatement;
pub use crate::syntax::statement::WhileStatement;
pub use crate::syntax::statement::ForStatement;
pub use crate::syntax::statement::ElseIfBranch;
pub use crate::syntax::statement::IfStatement;
pub use crate::syntax::smtype::SMType;
pub use crate::syntax::block::Block;

use crate::lexical::Lexer;
pub fn parse(lexer: &mut Lexer) -> Option<Program> {
    use crate::syntax::ast_item::IASTItem;

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
        use crate::lexical::Lexer;
        use super::parse;

        let lexer = &mut Lexer::new(r#"fn main() { println("helloworld"); }"#);
        let program = parse(lexer);

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
