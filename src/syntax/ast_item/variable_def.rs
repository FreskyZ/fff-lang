
// Variable -> Var Type Identifier [ = Expression ] | Const Type Identifier [ = Expression ]

use lexical::Lexer;
use syntax::ast_item::IASTItem;
use syntax::Type;
use syntax::Expression;
use syntax::scope::VariableScope;

#[derive(Debug, Eq, PartialEq)]
pub struct VariableDef {
    pub is_const: bool,
    pub var_type: Type,
    pub name: String,
    pub scope: VariableScope,
    pub assign_expr: Option<Expression>,
}

impl IASTItem for VariableDef {

    fn symbol_len(&self) -> usize {
        0
    }

    fn parse(lexer: &mut Lexer, index: usize) -> Option<VariableDef> {
        None
    }
}