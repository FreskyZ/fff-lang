
// Syntax, form abstract syntax tree

mod ast_item;
mod scope;

use message::MessageEmitter;
use lexical::BufLexer as Lexer;

use syntax::ast_item::ASTParser;
pub use syntax::ast_item::argument::Argument;
pub use syntax::ast_item::program::Program;
pub use syntax::ast_item::function_def::FunctionDef;
pub use syntax::ast_item::function_call::FunctionCall;
pub use syntax::ast_item::expression::Expression;
pub use syntax::ast_item::statement::Statement;
pub use syntax::ast_item::smtype::PrimitiveType;
pub use syntax::ast_item::smtype::Type;
pub use syntax::ast_item::variable::Variable;

pub fn get_ast(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Program> {

    Program::parse(lexer, messages)
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
        use lexical::BufLexer as Lexer;
        use super::get_ast;

        let messages = &mut MessageEmitter::new();
        let lexer = &mut Lexer::from(r#"fn main() { println("helloworld"); }"#.to_owned());
        let program = get_ast(lexer, messages);

        perrorln!("{:?}", program);
    }
}