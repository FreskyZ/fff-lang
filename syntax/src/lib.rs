#![allow(dead_code)]
///! fff-lang
///!
///! syntax, abstract syntax tree types and generation

// TODO: Add error recovery, which need new structure of message emitter and symbol length

#[macro_use] extern crate messages as message;
#[cfg_attr(test, macro_use)] extern crate util;
#[cfg_attr(test, macro_use)] extern crate codepos;
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
mod traits;
mod syntax_tree;
mod function_def;
mod type_use;
mod statement;
mod block;
mod expr;

pub use self::syntax_tree::SyntaxTree;
pub use self::function_def::Argument;
pub use self::function_def::FunctionDef;
pub use self::expr::Expression;
pub use self::expr::ExpressionBase;
pub use self::expr::ExpressionOperator;
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
pub use self::type_use::TypeUse;
pub use self::block::Block;

use self::traits::ISyntaxItem;
use self::traits::ISyntaxItemFormat;
pub use self::traits::ISyntaxItemWithStr;

#[cfg(test)] use self::traits::TestCase;

// TODO: recoverable, recoverable is actually another syntax rule with emitting messages