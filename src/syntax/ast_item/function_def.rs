
// FunctionDef = fFn fIdentifier fLeftParen [Type Identifier [fComma Type Identifier]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

use common::From2;
use common::StringPosition;
use message::Message;

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
    pub args: Vec<(SMType, String)>,
    pub ret_type: SMType,
    pub body: Block,
    poss: Vec<StringPosition>, // fn pos, name pos, args pos*
}

impl FunctionDef {

    pub fn pos_fn(&self) -> StringPosition { self.poss[0] }
    pub fn pos_name(&self) -> StringPosition { self.poss[1] }
    pub fn pos_args(&self) -> &[StringPosition] { &self.poss[2..(2 + self.args.len())] }
    pub fn pos_arg(&self, index: usize) -> StringPosition { self.poss[2 + index] }
    pub fn pos_ret_type(&self) -> StringPosition { self.ret_type.pos_all() }
    pub fn pos_body(&self) -> StringPosition { self.body.pos_all() }
    pub fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos_fn().start_pos, self.pos_body().end_pos) }
}

impl IASTItem for FunctionDef {

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<FunctionDef>, usize) {

        let mut current_len = 0;

        if !lexer.nth(index).is_keyword(KeywordKind::FnDef) {
            return lexer.push_expect("keyword fn", index, 0);
        }
        let mut poss = vec![lexer.pos(index)];
        current_len = 1;

        let fn_name = match lexer.nth(index + current_len).get_identifier() {
            Some(name) => name.clone(),
            None => return lexer.push_expect("identifier", index + 1, current_len),
        };
        poss.push(lexer.pos(index + current_len));
        current_len = 2;

        if !lexer.nth(index + 2).is_seperator(SeperatorKind::LeftParenthenes) {
            return lexer.push_expect("left parenthenes", index + 2, current_len);
        }
        current_len = 3;

        let mut args = Vec::<(SMType, String)>::new();
        loop {
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                current_len += 1;
                break; 
            }
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma)   // accept `fn main(i32 a,) {}`
                && lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                if args.is_empty() {                // recoverable error on fn main(, ) {}
                    let pos1 = lexer.pos(index).start_pos;
                    let pos2 = lexer.pos(index + current_len - 2).start_pos;
                    lexer.push(Message::SingleCommaInNonArgumentFunctionDef{ fn_pos: pos1, comma_pos: pos2 });
                }   
                current_len += 2;
                break;
            }
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                current_len += 1;
                continue;
            }
            match SMType::parse(lexer, index + current_len) {
                (None, arg_type_len) => return lexer.push_expects(vec!["typedef", "right parenthenese"], index + current_len, current_len + arg_type_len),
                (Some(arg_type), arg_type_len) => {
                    current_len += arg_type_len;
                    match lexer.nth(index + current_len).get_identifier() {
                        None => return lexer.push_expect("argument identifier", index + current_len, current_len),
                        Some(ident) => {
                            poss.push(lexer.pos(index + current_len));
                            args.push((arg_type, ident.clone()));
                            current_len += 1;
                        }
                    }
                }
            }
        }

        let mut return_type = SMType::default();
        if lexer.nth(index + current_len).is_seperator(SeperatorKind::NarrowRightArrow) {
            current_len += 1;
            match SMType::parse(lexer, index + current_len) {
                (Some(ret_type), ret_type_len) => {
                    return_type = ret_type;
                    current_len += ret_type_len;
                }
                (None, _length) => { // other things expect Type, find next left brace to continue
                    let _ = lexer.push_expect::<i32>("typedef", index + current_len, current_len);
                    for i in (index + current_len)..lexer.len() {
                        if lexer.nth(i).is_seperator(SeperatorKind::LeftBrace) {
                            current_len = i - index;
                        }
                    }
                }
            }
        }

        match Block::parse(lexer, index + current_len) {
            (Some(block), block_len) => 
                (Some(FunctionDef{ name: fn_name.clone(), args: args, ret_type: return_type, body: block, poss: poss }), current_len + block_len),
            (None, length) => (None, current_len + length),
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
        use syntax::SMTypeBase;
        use syntax::Block;
        use syntax::ast_item::IASTItem;
        use super::FunctionDef;
        use common::StringPosition;

        perrorln!("Case 1:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main() {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.messages());
        assert_eq!(
            result,
            (Some(FunctionDef{ 
                name: "main".to_owned(), 
                args: Vec::new(), 
                ret_type: SMType(SMTypeBase::Unit, make_str_pos!(1, 1, 1, 1)), 
                body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 1) },
                poss: Vec::new(), 
            }), 0)
        );

        perrorln!("Case 2:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main(i32 abc) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.messages());
        assert_eq!(
            result,
            (Some(FunctionDef{ 
                name: "main".to_owned(), 
                args: vec![(SMType(SMTypeBase::I32, make_str_pos!(1, 1, 1, 1)), "abc".to_owned())], 
                ret_type: SMType(SMTypeBase::Unit, make_str_pos!(1, 1, 1, 1)), 
                body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 1) },
                poss: Vec::new(), 
            }), 0)
        );

        perrorln!("Case 3:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main([[string]] argv, i32 argc, char some_other) {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.messages());
        assert_eq!(
            result,
            (Some(FunctionDef{ 
                name: "main".to_owned(), 
                args: vec![
                    (SMType::make_array(SMType::make_array(SMType(SMTypeBase::SMString, make_str_pos!(1, 1, 1, 1)), make_str_pos!(1, 1, 1, 1)), make_str_pos!(1, 1, 1, 1)), "argv".to_owned()),
                    (SMType(SMTypeBase::I32, make_str_pos!(1, 1, 1, 1)), "argc".to_owned()),
                    (SMType(SMTypeBase::Char, make_str_pos!(1, 1, 1, 1)), "some_other".to_owned()),
                ],
                ret_type: SMType(SMTypeBase::Unit, make_str_pos!(1, 1, 1, 1)), 
                body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 1) },
                poss: Vec::new(),  
            }), 0)
        );
        
        perrorln!("Case 4:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main() -> i32 {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.messages());
        assert_eq!(
            result,
            (Some(FunctionDef{ 
                name: "main".to_owned(), 
                args: Vec::new(), 
                ret_type: SMType(SMTypeBase::I32, make_str_pos!(1, 1, 1, 1)), 
                body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 1) },
                poss: Vec::new(), 
            }), 0)
        );
        
        perrorln!("Case 5:");
        let messages = MessageEmitter::new();
        let lexer = &mut Lexer::new_test("fn main([string] argv, i32 argc, char some_other) -> [[string]] {}", messages);

        let result = FunctionDef::parse(lexer, 0);
        perrorln!("messages: {:?}", lexer.messages());
        assert_eq!(
            result,
            (Some(FunctionDef{ 
                name: "main".to_owned(), 
                args: vec![
                    (SMType::make_array(SMType(SMTypeBase::SMString, make_str_pos!(1, 1, 1, 1)), make_str_pos!(1, 1, 1, 1)), "argv".to_owned()),
                    (SMType(SMTypeBase::I32, make_str_pos!(1, 1, 1, 1)), "argc".to_owned()),
                    (SMType(SMTypeBase::Char, make_str_pos!(1, 1, 1, 1)), "some_other".to_owned()),
                ],
                ret_type: SMType::make_array(SMType::make_array(SMType(SMTypeBase::SMString, make_str_pos!(1, 1, 1, 1)), make_str_pos!(1, 1, 1, 1)), make_str_pos!(1, 1, 1, 1)), 
                body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 1, 1, 1) },
                poss: Vec::new(), 
            }), 0)
        );
    }
}