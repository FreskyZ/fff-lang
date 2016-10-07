
// FunctionDef -> 
//     FnDef Identifier LeftParen [Argument [, Argument]*] RightParen [NarrowRightArrow Type] Statement

use message::Message;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Argument;
use syntax::Type;
use syntax::Statement;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub arguments: Vec<Argument>,
    pub return_type: Type,
    pub body: Statement,
}

impl IASTItem for FunctionDef {

    fn symbol_len(&self) -> usize {
        // fn, name, argument * 2, 
        2
        + self.arguments.iter().fold(-1i32, |i, ref arg| i + arg.symbol_len() as i32 + 1) as usize
        + if self.return_type == Type::unit_type() { 0 } else { 2 }
        + self.body.symbol_len()
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<FunctionDef> {

        if !lexer.nth(index).is_keyword(KeywordKind::FnDef) {
            return lexer.push_expect_symbol("keyword `fn`", index);
        }

        let fn_name = match (lexer.nth(index + 1).get_identifier(), lexer.sym_pos(index + 1).start_pos) {
            (Some(name), _pos) => name.clone(),
            (None, pos) => return lexer.push_ret_none(Message::ExpectSymbol{ desc: "identifier".to_owned(), pos: pos }),
        };

        if !lexer.nth(index + 2).is_seperator(SeperatorKind::LeftParenthenes) {
            return lexer.push_expect_symbol("left parenthenes", index + 2);
        }

        let mut args = Vec::<Argument>::new();
        let mut has_args = false;
        let mut args_sym_len = 0_usize;
        loop {
            if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::RightParenthenes) {
                break; 
            }
            if has_args && lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::Comma) {
                continue;
            }
            match Argument::parse(lexer, index + 3 + args_sym_len) {
                Some(arg) => {
                    args_sym_len += arg.symbol_len();
                    has_args = true;
                    if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::RightParenthenes) {
                        args.push(arg);
                        break; 
                    } else if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::Comma) {
                        args.push(arg);
                        args_sym_len += 1;
                        continue;
                    } else {
                        return lexer.push_expect_symbol("comma or right parenthenes", index + 3 + args_sym_len);
                    }
                }
                None => {
                    return lexer.push_expect_symbol("argument", index + 3 + args_sym_len);
                }
            }
        }

        let mut return_type = Type::unit_type();
        let mut ret_type_sym_len = 0_usize;
        if lexer.nth(index + 4 + args_sym_len).is_seperator(SeperatorKind::NarrowRightArrow) {
            match Type::parse(lexer, index + 5 + args_sym_len) {
                Some(ret_type) => {
                    return_type = ret_type;
                    ret_type_sym_len = 1 + return_type.symbol_len();
                }
                None => {
                    return lexer.push_expect_symbol("typedef", index + 5 + args_sym_len);
                }
            }
        }

        match Statement::parse(lexer, index + 4 + args_sym_len + ret_type_sym_len) {
            Some(statement) => Some(FunctionDef{ name: fn_name.clone(), arguments: args, return_type: return_type, body: statement }),
            None => lexer.push_expect_symbol("statement", index + 4 + args_sym_len + ret_type_sym_len),
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_function_def_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::Argument;
        use syntax::Type;
        use syntax::PrimitiveType;
        use syntax::Statement;
        use syntax::ast_item::IASTItem;
        use super::FunctionDef;

        perrorln!("Case 1:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::from_test("fn main() {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: Vec::new(), 
                return_type: Type::unit_type(), 
                body: Statement{ exprs: Vec::new() } 
            })
        );

        perrorln!("Case 2:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::from_test("fn main(i32 abc) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![Argument{ name: "abc".to_owned(), arg_type: Type::Primitive(PrimitiveType::I32) }], 
                return_type: Type::unit_type(), 
                body: Statement{ exprs: Vec::new() } 
            })
        );

        perrorln!("Case 3:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::from_test("fn main([string] argv, i32 argc, char some_other) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![
                    Argument{ arg_type: Type::Array(PrimitiveType::SMString), name: "argv".to_owned() },
                    Argument{ arg_type: Type::Primitive(PrimitiveType::I32), name: "argc".to_owned() },
                    Argument{ arg_type: Type::Primitive(PrimitiveType::Char), name: "some_other".to_owned() },
                ],
                return_type: Type::unit_type(), 
                body: Statement{ exprs: Vec::new() } 
            })
        );
        
        perrorln!("Case 4:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::from_test("fn main() -> i32 {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: Vec::new(), 
                return_type: Type::Primitive(PrimitiveType::I32), 
                body: Statement{ exprs: Vec::new() } 
            })
        );
        
        perrorln!("Case 5:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::from_test("fn main([string] argv, i32 argc, char some_other) -> [string] {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![
                    Argument{ arg_type: Type::Array(PrimitiveType::SMString), name: "argv".to_owned() },
                    Argument{ arg_type: Type::Primitive(PrimitiveType::I32), name: "argc".to_owned() },
                    Argument{ arg_type: Type::Primitive(PrimitiveType::Char), name: "some_other".to_owned() },
                ],
                return_type: Type::Array(PrimitiveType::SMString), 
                body: Statement{ exprs: Vec::new() } 
            })
        );
    }
}