
// Variable -> Var Type Identifier [ = Expression ] | Const Type Identifier [ = Expression ]

use message::MessageEmitter;
use lexical::BufLexer as Lexer;
use syntax::ast_item::ASTParser;
use syntax::Type;
use syntax::scope::VariableScope;

#[derive(Debug, Eq, PartialEq)]
pub struct Variable {
    pub is_const: bool,
    pub var_type: Type,
    pub name: String,
    pub scope: VariableScope,
}

impl ASTParser for Variable {

    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Variable> {
        None
    }
}