
// Statement -> LeftBrace [Expression]* RightBrace
 
use message::MessageEmitter;
use lexical::Lexer;
use syntax::ast_item::ASTParser;
use syntax::Expression;

#[derive(Debug, Eq, PartialEq)]
pub struct Statement {
    pub exprs: Vec<Expression>,
}

impl ASTParser for Statement {
    
    fn parse(lexer: &mut Lexer, messages: &mut MessageEmitter) -> Option<Statement> {
        
        None
    }
}