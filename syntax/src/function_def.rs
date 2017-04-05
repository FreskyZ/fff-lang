
// FunctionDef = fFn fIdentifier fLeftParen [Type Identifier [fComma Type Identifier]* [fComma] ] fRightParen [fNarrowRightArrow Type] Block

use std::fmt;

use codepos::StringPosition;
use util::format_vector_debug;
use message::SyntaxMessage;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use super::ISyntaxItem;
use super::TypeUse;
use super::Block;

#[derive(Eq, PartialEq)]
pub struct Argument {
    pub ty: TypeUse,
    pub name: String,
    pub pos_name: StringPosition,
}

impl fmt::Debug for Argument {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} {:?} @ {:?}", self.ty, self.name, self.pos_name)
    }
}
impl ISyntaxItem for Argument {

    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.ty.pos_all(), self.pos_name) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool { 
        TypeUse::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<Argument>, usize) {

        let (ty, ty_len) = match TypeUse::parse(lexer, messages, index) {
            (Some(ty), ty_len) => (ty, ty_len),
            (None, len) => return (None, len), 
        };

        let name = if lexer.nth(index + ty_len).is_keyword(KeywordKind::This) {
            "this".to_owned()
        } else { 
            match lexer.nth(index + ty_len).get_identifier() {
                Some(ident) => ident,
                None => return push_unexpect!(lexer, messages, ["identifier", ], index + ty_len, ty_len),
            }
        };

        (Some(Argument{ ty: ty, name: name, pos_name: lexer.pos(index + ty_len) }), ty_len + 1)
    }
}
impl Argument {
    
    pub fn pub_pos_all(&self) -> StringPosition { self.pos_all() }
}

#[derive(Eq, PartialEq)]
pub struct FunctionDef {
    pub name: String,
    pub args: Vec<Argument>,
    pub ret_type: TypeUse,           // if not specified, position is decided at exactly after right paren
    pub body: Block,
    pub pos2: [StringPosition; 2],  // pos_fn and pos_name
}
impl fmt::Debug for FunctionDef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn @ {:?} {:?} @ {:?} ({:?}) -> {:?} {:?}",
            self.pos2[0], 
            self.name, self.pos2[1],
            format_vector_debug(&self.args, ", "),
            self.ret_type,
            self.body,
        )
    }
}
impl FunctionDef {

    pub fn pos_fn(&self) -> StringPosition { self.pos2[0] }
    pub fn pos_name(&self) -> StringPosition { self.pos2[1] }
}
impl ISyntaxItem for FunctionDef {
    
    fn pos_all(&self) -> StringPosition { StringPosition::merge(self.pos2[0], self.body.pos_all()) }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_keyword(KeywordKind::FnDef)
    }
    
    #[allow(unused_assignments)]
    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<FunctionDef>, usize) {

        let mut current_len = 0;

        if !lexer.nth(index).is_keyword(KeywordKind::FnDef) {
            return push_unexpect!(lexer, messages, "keyword fn", index, 0);
        }
        current_len = 1;

        let fn_name = match lexer.nth(index + current_len).get_identifier() {
            Some(name) => name.clone(),
            None => return push_unexpect!(lexer, messages, "identifier", index + 1, current_len),
        };
        current_len = 2;

        if !lexer.nth(index + 2).is_seperator(SeperatorKind::LeftParenthenes) {
            return push_unexpect!(lexer, messages, "left parenthenes", index + 2, current_len);
        }
        current_len = 3;

        let mut args = Vec::new();
        loop {
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                current_len += 1;
                break; 
            }
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma)   // accept `fn main(i32 a,) {}`
                && lexer.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                if args.is_empty() {                // recoverable error on fn main(, ) {}
                    let pos1 = StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len + 1));
                    let pos2 = lexer.pos(index + current_len - 2).start_pos();
                    messages.push(SyntaxMessage::SingleCommaInNonArgumentFunctionDef{ fn_pos: pos1, comma_pos: pos2 });
                }   
                current_len += 2;
                break;
            }
            if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                current_len += 1;
                continue;
            }
            match Argument::parse(lexer, messages, index + current_len) {
                (None, arg_len) => return (None, current_len + arg_len),
                (Some(arg), arg_len) => {
                    current_len += arg_len;
                    args.push(arg);
                }
            }
        }

        let may_be_ret_type_pos = lexer.pos(index + current_len - 1).start_pos().next_col();
        let mut return_type = TypeUse::Unit(StringPosition::from2(may_be_ret_type_pos, may_be_ret_type_pos));
        if lexer.nth(index + current_len).is_seperator(SeperatorKind::NarrowRightArrow) {
            current_len += 1;
            match TypeUse::parse(lexer, messages, index + current_len) {
                (Some(ret_type), ret_type_len) => {
                    return_type = ret_type;
                    current_len += ret_type_len;
                }
                (None, _length) => { // other things expect Type, find next left brace to continue
                    let _: (Option<i32>, _) = push_unexpect!(lexer, messages, "typedef", index + current_len, current_len);
                    for i in (index + current_len)..lexer.len() {
                        if lexer.nth(i).is_seperator(SeperatorKind::LeftBrace) {
                            current_len = i - index;
                        }
                    }
                }
            }
        }

        match Block::parse(lexer, messages, index + current_len) {
            (Some(block), block_len) => {
                let pos1 = lexer.pos(index);
                let pos2 = lexer.pos(index + 1);
                (Some(FunctionDef{ name: fn_name.clone(), args: args, ret_type: return_type, body: block, pos2: [pos1, pos2] }), current_len + block_len)
            }
            (None, length) => (None, current_len + length),
        }
    }
}

#[cfg(test)] #[test]
fn ast_argument_parse() {
    use super::ISyntaxItemWithStr;

    assert_eq!(
        Argument::with_test_str_ret_size("i32 a"), 
        (Some(Argument{ 
            ty: TypeUse::Base("i32".to_owned(), make_str_pos!(1, 1, 1, 3)), 
            name: "a".to_owned(),
            pos_name: make_str_pos!(1, 5, 1, 5), 
        }), 2)
    );
    
    assert_eq!(
        Argument::with_test_str_ret_size("[u8] buffer"), 
        (Some(Argument{ 
            ty: TypeUse::Array(Box::new(TypeUse::Base("u8".to_owned(), make_str_pos!(1, 2, 1, 3))), make_str_pos!(1, 1, 1, 4)), 
            name: "buffer".to_owned(),
            pos_name: make_str_pos!(1, 6, 1, 11), 
        }), 4)
    );
}
#[cfg(test)] #[test]
fn ast_function_def_parse() {
    use super::ISyntaxItemWithStr;
    // TODO: there was println!(messages) to manually check messages, update them to auto check when refactoring this

    perrorln!("Case 1:"); //                 123456789ABC
    let result = FunctionDef::with_test_str_ret_size("fn main() {}");
    assert_eq!(
        result,
        (Some(FunctionDef{ 
            name: "main".to_owned(), 
            pos2: [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7)], 
            args: Vec::new(), 
            ret_type: TypeUse::Unit(make_str_pos!(1, 10, 1, 10)), 
            body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 11, 1, 12) },
        }), 6)
    );

    perrorln!("Case 2:"); //                 123456789ABCDEFGHI
    let result = FunctionDef::with_test_str_ret_size("fn main(i32 abc) {}");
    assert_eq!(
        result,
        (Some(FunctionDef{  
            name: "main".to_owned(), 
            pos2: [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7)], 
            args: vec![
                Argument{
                    ty: TypeUse::Base("i32".to_owned(), make_str_pos!(1, 9, 1, 11)), 
                    name: "abc".to_owned(),
                    pos_name: make_str_pos!(1, 13, 1, 15),
                }
            ], 
            ret_type: TypeUse::Unit(make_str_pos!(1, 17, 1, 17)), 
            body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 18, 1, 19) },
        }), 8)
    );
                            //                 0        1         2         3         4         5         6
    perrorln!("Case 3:"); //                 12345678901234567890123456789012345678901234567890123456789012
    let result = FunctionDef::with_test_str_ret_size(" fn mainxxx([[string] ] argv  ,i32 this, char some_other, )  {}");
    assert_eq!(
        result,
        (Some(FunctionDef{  
            name: "mainxxx".to_owned(), 
            pos2: [make_str_pos!(1, 2, 1, 3), make_str_pos!(1, 5, 1, 11)], 
            args: vec![
                Argument{
                    ty: TypeUse::Array(Box::new(
                            TypeUse::Array(Box::new(
                                TypeUse::Base("string".to_owned(), make_str_pos!(1, 15, 1, 20))
                            ), make_str_pos!(1, 14, 1, 21))
                        ), make_str_pos!(1, 13, 1, 23)), 
                    name: "argv".to_owned(),
                    pos_name: make_str_pos!(1, 25, 1, 28),
                },
                Argument{
                    ty: TypeUse::Base("i32".to_owned(), make_str_pos!(1, 32, 1, 34)), 
                    name: "this".to_owned(),
                    pos_name: make_str_pos!(1, 36, 1, 39),
                },
                Argument{
                    ty: TypeUse::Base("char".to_owned(), make_str_pos!(1, 42, 1, 45)), 
                    name: "some_other".to_owned(),
                    pos_name: make_str_pos!(1, 47, 1, 56),
                },
            ],
            ret_type: TypeUse::Unit(make_str_pos!(1, 60, 1, 60)), 
            body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 62, 1, 63) },
        }), 19)
    );
                            //                 0        1         2        
    perrorln!("Case 4:"); //                 123456789012345678901
    let result = FunctionDef::with_test_str_ret_size("fn main(, ) -> i32 {}");
    assert_eq!(
        result,
        (Some(FunctionDef{  
            name: "main".to_owned(), 
            pos2: [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7)], 
            args: Vec::new(), 
            ret_type: TypeUse::Base("i32".to_owned(), make_str_pos!(1, 16, 1, 18)), 
            body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 20, 1, 21) },
        }), 9)
    );
                            //                 0        1         2         3         4         5         6
    perrorln!("Case 5:"); //                 1234567890123456789012345678901234567890123456789012345678901234567
    let result = FunctionDef::with_test_str_ret_size("fn main([string] argv, i32 argc, char some_other,) -> [[string]] {}");
    assert_eq!(
        result,
        (Some(FunctionDef{  
            name: "main".to_owned(), 
            pos2: [make_str_pos!(1, 1, 1, 2), make_str_pos!(1, 4, 1, 7)], 
            args: vec![
                Argument{
                    ty: TypeUse::Array(Box::new(TypeUse::Base("string".to_owned(), make_str_pos!(1, 10, 1, 15))), make_str_pos!(1, 9, 1, 16)), 
                    name: "argv".to_owned(),
                    pos_name: make_str_pos!(1, 18, 1, 21),
                },
                Argument{
                    ty: TypeUse::Base("i32".to_owned(), make_str_pos!(1, 24, 1, 26)), 
                    name: "argc".to_owned(),
                    pos_name: make_str_pos!(1, 28, 1, 31),
                },
                Argument{
                    ty: TypeUse::Base("char".to_owned(), make_str_pos!(1, 34, 1, 37)), 
                    name: "some_other".to_owned(),
                    pos_name: make_str_pos!(1, 39, 1, 48),
                },
            ],
            ret_type: TypeUse::Array(Box::new(
                        TypeUse::Array(Box::new(
                            TypeUse::Base("string".to_owned(), make_str_pos!(1, 57, 1, 62))
                        ), make_str_pos!(1, 56, 1, 63))
                    ), make_str_pos!(1, 55, 1, 64)), 
            body: Block{ stmts: Vec::new(), pos: make_str_pos!(1, 66, 1, 67) },
        }), 23)
    );
}