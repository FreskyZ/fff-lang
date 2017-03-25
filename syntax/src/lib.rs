#![allow(dead_code)]

///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

// TODO: Add error recovery, which need new structure of message emitter and symbol length

#[macro_use] extern crate util;
#[macro_use] extern crate codepos;
#[macro_use] extern crate messages as message;
extern crate lexical;

macro_rules! push_unexpect {
    ($lexer: expr, $messages: expr, [$($final_tokens: expr, )+], $index: expr, $ret_size: expr) => ({
        use util::format_vector_display;

        let desc = format!("Expect {}", format_vector_display(&vec![$($final_tokens, )+], ", "));
        let actual_token_desc = format!("Meet {:?}", $lexer.nth($index));
        let strpos = $lexer.pos($index);

        $messages.push(Message::with_help("Unexpect symbol".to_owned(), 
            vec![(strpos, actual_token_desc)],
            vec![desc]
        ));

        (None, $ret_size)
    });
    ($lexer: expr, $messages: expr, $final_token: expr, $index: expr, $ret_size: expr) => ({
        push_unexpect!($lexer, $messages, [$final_token, ], $index, $ret_size)
    })
}

#[macro_use]
mod ast_item;
mod syntax_tree;
mod function_def;
mod smtype;
mod statement;
mod block;
mod expression;

pub use self::ast_item::ISyntaxItem;
pub use self::syntax_tree::SyntaxTree;
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
        // use lexical::parse_test_str;
        // use super::parse;

        // let lexer = &mut parse_test_str(r#"fn main() { println("helloworld"); }"#);
        // let program = parse(lexer);

        // perrorln!("program: {:?}", program);
        // perrorln!("messages: {:?}", lexer.messages())
    }
}

// recoverable, recoverable is actually another syntax rule with emitting messages

// TODO: 
// Change lexer.push_expect(s) to messages.push(Message::new...)
// Move MessageCollection out of lexer and add messages to ISyntaxItem, change lexer driver to TokenStream::parse(codechars, messages)
// Rename ISyntaxItem to ISyntaxItem, change syntax driver to SyntaxTree::parse(tokens, messages)