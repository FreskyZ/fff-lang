
// FunctionDef = fFn fIdentifier fLeftParen [Type Identifier [fComma Type Identifier]*] fRightParen [fNarrowRightArrow Type] Block

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::SMType;
use syntax::Block;

#[derive(Debug, Eq, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub arguments: Vec<(SMType, String)>,
    pub return_type: SMType,
    pub body: Block,
}

impl IASTItem for FunctionDef {

    fn symbol_len(&self) -> usize {
        // fn, name, argument * 2, 
        2
        + self.arguments.iter().fold(-1i32, |i, ref arg| i + arg.0.symbol_len() as i32 + 2) as usize
        + if self.return_type == SMType::Unit { 0 } else { 2 }
        + self.body.symbol_len()
    }
    
    fn parse(lexer: &mut Lexer, index: usize) -> Option<FunctionDef> {

        if !lexer.nth(index).is_keyword(KeywordKind::FnDef) {
            return lexer.push_expect_symbol("keyword `fn`", index);
        }

        let fn_name = match lexer.nth(index + 1).get_identifier() {
            Some(name) => name.clone(),
            None => return lexer.push_expect_symbol("identifier", index + 1),
        };

        if !lexer.nth(index + 2).is_seperator(SeperatorKind::LeftParenthenes) {
            return lexer.push_expect_symbol("left parenthenes", index + 2);
        }

        let mut args = Vec::<(SMType, String)>::new();
        let mut has_args = false;
        let mut args_sym_len = 0_usize;
        loop {
            if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::RightParenthenes) {
                break; 
            }
            if has_args && lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::Comma) {
                continue;
            }
            match SMType::parse(lexer, index + 3 + args_sym_len) {
                Some(arg_type) => {
                    match lexer.nth(index + 3 + args_sym_len + arg_type.symbol_len()).get_identifier() {
                        Some(ident) => {
                            args_sym_len += arg_type.symbol_len() + 1;
                            has_args = true;
                            if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::RightParenthenes) {
                                args.push((arg_type, ident.clone()));
                                break; 
                            } else if lexer.nth(index + 3 + args_sym_len).is_seperator(SeperatorKind::Comma) {
                                args.push((arg_type, ident.clone()));
                                args_sym_len += 1;
                                continue;
                            } else {
                                return lexer.push_expect_symbol("comma or right parenthenes", index + 3 + args_sym_len);
                            }
                        }
                        None => {
                            return lexer.push_expect_symbol("argument name", index + 3 + args_sym_len + arg_type.symbol_len());
                        }
                    }
                }
                None => {
                    return lexer.push_expect_symbol("typedef", index + 3 + args_sym_len);
                }
            }
        }

        let mut return_type = SMType::Unit;
        let mut ret_type_sym_len = 0_usize;
        if lexer.nth(index + 4 + args_sym_len).is_seperator(SeperatorKind::NarrowRightArrow) {
            match SMType::parse(lexer, index + 5 + args_sym_len) {
                Some(ret_type) => {
                    return_type = ret_type;
                    ret_type_sym_len = 1 + return_type.symbol_len();
                }
                None => {
                    return lexer.push_expect_symbol("typedef", index + 5 + args_sym_len);
                }
            }
        }

        match Block::parse(lexer, index + 4 + args_sym_len + ret_type_sym_len) {
            Some(block) => Some(FunctionDef{ name: fn_name.clone(), arguments: args, return_type: return_type, body: block }),
            None => lexer.push_expect_symbol("block", index + 4 + args_sym_len + ret_type_sym_len),
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_function_def_parse() {
        use message::MessageEmitter;
        use lexical::Lexer;
        use syntax::SMType;
        use syntax::Block;
        use syntax::ast_item::IASTItem;
        use super::FunctionDef;

        perrorln!("Case 1:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main() {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: Vec::new(), 
                return_type: SMType::Unit, 
                body: Block{ stmts: Vec::new() } 
            })
        );

        perrorln!("Case 2:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main(i32 abc) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![(SMType::I32, "abc".to_owned())], 
                return_type: SMType::Unit, 
                body: Block{ stmts: Vec::new() } 
            })
        );

        perrorln!("Case 3:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main([[string]] argv, i32 argc, char some_other) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![
                    (SMType::make_array(SMType::make_array(SMType::SMString)), "argv".to_owned()),
                    (SMType::I32, "argc".to_owned()),
                    (SMType::Char, "some_other".to_owned()),
                ],
                return_type: SMType::Unit, 
                body: Block{ stmts: Vec::new() } 
            })
        );
        
        perrorln!("Case 4:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main() -> i32 {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: Vec::new(), 
                return_type: SMType::I32, 
                body: Block{ stmts: Vec::new() } 
            })
        );
        
        perrorln!("Case 5:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main([string] argv, i32 argc, char some_other) -> [[string]] {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.emitter());
        assert_eq!(
            result,
            Some(FunctionDef{ 
                name: "main".to_owned(), 
                arguments: vec![
                    (SMType::make_array(SMType::SMString), "argv".to_owned()),
                    (SMType::I32, "argc".to_owned()),
                    (SMType::Char, "some_other".to_owned()),
                ],
                return_type: SMType::make_array(SMType::make_array(SMType::SMString)), 
                body: Block{ stmts: Vec::new() } 
            })
        );
    }
}