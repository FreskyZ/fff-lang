
// Argument -> Type Identifier

use message::MessageEmitter;
use lexical::Lexer;
use syntax::ast_item::ASTParser;
use syntax::ast_item::ASTItem;
use syntax::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Argument {
    pub arg_type: Type,
    pub name: String,
}

impl ASTItem for Argument {
    fn symbol_len(&self) -> usize {
        self.arg_type.symbol_len() + 1
    }
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
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::ASTParser;
        use syntax::Type;
        use syntax::PrimitiveType;
        use super::Argument;

        macro_rules! test_case {
            ($program_slice: expr, $expect_type: expr, $expect_name: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from_test($program_slice, messages);
                assert_eq!(Argument::parse(lexer, messages), Some(Argument{ arg_type: $expect_type, name: $expect_name.to_owned() }));
            });
            ($program_slice: expr) => ({

                let messages = &mut MessageEmitter::new();
                let lexer = &mut Lexer::from_test($program_slice, messages);
                assert_eq!(Argument::parse(lexer, messages), None);
            })
        }

        test_case!("");
        test_case!("i32 a", Type::Primitive(PrimitiveType::I32), "a");
        test_case!("[string] args", Type::Array(PrimitiveType::SMString), "args");
    }
}