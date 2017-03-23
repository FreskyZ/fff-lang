#![allow(dead_code)]

///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

// TODO: Add error recovery, which need new structure of message emitter and symbol length

#[macro_use] extern crate util;
#[macro_use] extern crate codepos;
extern crate messages as message;
extern crate codemap;
extern crate lexical;

#[macro_use]
mod ast_item;
mod program;
mod function_def;
mod smtype;
mod statement;
mod block;
mod expression;

pub use self::program::Program;
pub use self::function_def::Argument;
pub use self::function_def::FunctionDef;
pub use self::expression::Expression;
pub use self::expression::ExpressionBase;
pub use self::expression::ExpressionOperator;
pub use self::statement::Statement;
pub use self::statement::VarDeclStatement;
pub use self::statement::ReturnStatement;
pub use self::statement::BreakStatement;
pub use self::statement::ContinueStatement;
pub use self::statement::ExpressionStatement;
pub use self::statement::LoopStatement;
pub use self::statement::WhileStatement;
pub use self::statement::ForStatement;
pub use self::statement::ElseIfBranch;
pub use self::statement::IfStatement;
pub use self::smtype::SMType;
pub use self::block::Block;

use lexical::Lexer;
pub fn parse(lexer: &mut Lexer) -> Option<Program> {
    use self::ast_item::IASTItem;

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
        use lexical::Lexer;
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
