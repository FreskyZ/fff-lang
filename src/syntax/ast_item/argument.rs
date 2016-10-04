
// Argument -> Type Identifier

use message::MessageEmitter;
use lexical::Lexer;
use syntax::ast_item::ASTParser;
use syntax::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Argument {
    pub arg_type: Type,
    pub name: String,
}

impl ASTParser for Argument {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Argument> {
        None
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_argument_parse() {
        
    }
}