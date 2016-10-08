
// Argument -> Type Identifier

use lexical::Lexer;
use lexical::IToken;
use syntax::ast_item::IASTItem;
use syntax::Type;

#[derive(Debug, Eq, PartialEq)]
pub struct Argument {
    pub arg_type: Type,
    pub name: String,
}

impl IASTItem for Argument {

    fn symbol_len(&self) -> usize {
        self.arg_type.symbol_len() + 1
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<Argument> {
        
        match Type::parse(lexer, index) {
            Some(smtype) => {
                match lexer.nth(index + smtype.symbol_len()).get_identifier() {
                    Some(name) => Some(Argument{ arg_type: smtype, name: name.clone(), }),
                    None => lexer.push_expect_symbol("identifier", index + smtype.symbol_len()),
                }
            }
            None => None, // message emitted
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_argument_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::ast_item::IASTItem;
        use syntax::Type;
        use syntax::PrimitiveType;
        use super::Argument;

        macro_rules! test_case {
            ($program_slice: expr, $expect_type: expr, $expect_name: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                assert_eq!(Argument::parse(lexer, 0), Some(Argument{ arg_type: $expect_type, name: $expect_name.to_owned() }));
            });
            ($program_slice: expr) => ({

                let messages = MessageEmitter::new();
                let lexer = &mut Lexer::new_test($program_slice, messages);
                assert_eq!(Argument::parse(lexer, 0), None);
            })
        }

        test_case!("");
        test_case!("i32 a", Type::Primitive(PrimitiveType::I32), "a");
        test_case!("[string] args", Type::Array(PrimitiveType::SMString), "args");
    }
}