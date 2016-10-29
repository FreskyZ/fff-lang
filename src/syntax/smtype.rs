
// Type = fPrimitiveType | fIdentifier | fLeftBracket Type fRightBracket | fLeftParen Type [fComma Type]* [fComma] fRightParen 

// Some history, just for complain...
// First, only primitive types and one level of array are supported, 
//     recursive types are not supported, so SMTypeBase are primitive types, SMType are enum of them or an array        // SMTypeBase + SMType
// Then recursive array are accepted and boxed value are accepted so there is only SMType 
//     and primitive types as enum member, including the recursive array                                                // SMType
// Then position info is add and SMType is then devided into SMTypeBase and its position                                // SMTypeBase + SMType
// Then more primitive types added and SMTypeBase become large, nearly twenty members                                   // SMTypeBase + SMType
// Then tuple type and identifier as user define types are accepted and SMTypeBase is managed to 6 enum members 
//      and position info are moved back, so there is no more SMTypeBase again                                          // SMType
// Then a Primtype enum is added instead of the keywordkind, for unit is not keyword but prim type                      // SMType + PrimitiveType

use std::fmt;

use common::From2;
use common::StringPosition;
use common::format_vector_debug;
use common::format_vector_display;
use message::SyntaxMessage;

use lexical::Lexer;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;

#[derive(Eq, PartialEq, Clone)]
pub enum PrimitiveType {
    Unit,
    U8, I8,
    U16, I16,
    U32, I32,
    U64, I64,
    F32,
    F64,
    Char, 
    Bool,
    SMString,
}
impl fmt::Debug for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::PrimitiveType::*;
        match *self {
            Unit => write!(f, "()"),
            U8 => write!(f, "u8"), I8 => write!(f, "i8"),
            U16 => write!(f, "u16"), I16 => write!(f, "i16"),
            U32 => write!(f, "u32"), I32 => write!(f, "i32"),
            U64 => write!(f, "u64"), I64 => write!(f, "i64"),
            F32 => write!(f, "f32"), 
            F64 => write!(f, "f64"),
            Char => write!(f, "char"), 
            Bool => write!(f, "bool"),
            SMString => write!(f, "string"),
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum SMType {
    User(String, StringPosition),           // position for the identifier, although User define type is accepted here, user define type syntax is not added
    Prim(PrimitiveType, StringPosition),    // position for the primitive type
    Tuple(Vec<SMType>, StringPosition),     // position for ()
    Array(Box<SMType>, StringPosition),     // position for []
}

impl fmt::Debug for SMType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SMType::User(ref name, ref pos) => write!(f, "{} @ {:?}", name, pos),
            SMType::Prim(ref ty, ref pos) => write!(f, "{:?} @ {:?}", ty, pos),
            SMType::Tuple(ref types, ref pos) => write!(f, "({}) @ {:?}", format_vector_debug(types, ", "), pos),
            SMType::Array(ref inner, ref pos) => write!(f, "[{:?}] @ {:?}", inner.as_ref(), pos),
        }
    }
}
impl fmt::Display for SMType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SMType::User(ref name, ref _pos) => write!(f, "{}", name),
            SMType::Prim(ref ty, ref _pos) => write!(f, "{:?}", ty),
            SMType::Tuple(ref types, ref _pos) => write!(f, "({})", format_vector_display(types, ", ")),
            SMType::Array(ref inner, ref _pos) => write!(f, "[{:?}]", inner.as_ref()),
        }
    }
}

impl SMType {

    pub fn is_spec_prim(&self, expect: PrimitiveType) -> bool {
        match *self {
            SMType::Prim(ref prim, ref _pos) => *prim == expect,
            _ => false,
        }
    }
}

fn keyword_to_primitive_type(keyword: KeywordKind) -> Option<PrimitiveType> {
    match keyword {
        KeywordKind::PrimTypeU8 => Some(PrimitiveType::U8),
        KeywordKind::PrimTypeI8 => Some(PrimitiveType::I8),
        KeywordKind::PrimTypeU16 => Some(PrimitiveType::U16),
        KeywordKind::PrimTypeI16 => Some(PrimitiveType::I16),
        KeywordKind::PrimTypeU32 => Some(PrimitiveType::U32),
        KeywordKind::PrimTypeI32 => Some(PrimitiveType::I32),
        KeywordKind::PrimTypeU64 => Some(PrimitiveType::U64),
        KeywordKind::PrimTypeI64 => Some(PrimitiveType::I64),
        KeywordKind::PrimTypeF32 => Some(PrimitiveType::F32),
        KeywordKind::PrimTypeF64 => Some(PrimitiveType::F64),
        KeywordKind::PrimTypeChar => Some(PrimitiveType::Char),
        KeywordKind::PrimTypeBool => Some(PrimitiveType::Bool),
        KeywordKind::PrimTypeString => Some(PrimitiveType::SMString),
        _ => None,
    }
}

impl IASTItem for SMType {

    fn pos_all(&self) -> StringPosition { 
        match *self {
            SMType::User(ref _name, ref pos) => *pos,
            SMType::Prim(ref _keyword, ref pos) => *pos,
            SMType::Tuple(ref _types, ref pos) => *pos,
            SMType::Array(ref _inner, ref pos) => *pos,
        }
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        lexer.nth(index).is_ident()
        || lexer.nth(index).is_seperator(SeperatorKind::LeftBracket)
        || lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes)
        || match lexer.nth(index).get_keyword() {
            Some(keyword) => keyword.is_prim_type(),
            None => false,    
        }
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<SMType>, usize) {
        let log_enable = false;

        if lexer.nth(index).is_ident() {
            test_condition_perrorln!{ log_enable, "is ident, return" }
            return (Some(SMType::User(lexer.nth(index).get_identifier().unwrap(), lexer.pos(index))), 1);
        }
        if let Some(keyword) = lexer.nth(index).get_keyword() {
            test_condition_perrorln!{ log_enable, "is some keyword" }
            if keyword.is_prim_type() {
                test_condition_perrorln!{ log_enable, "is primitive type keyword, return" }
                return (Some(SMType::Prim(keyword_to_primitive_type(keyword).unwrap(), lexer.pos(index))), 1);
            } 
        }

        let mut current_len = 0;
        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            test_condition_perrorln!{ log_enable, "is left bracket, try get array inner type" }
            current_len += 1;
            match SMType::parse(lexer, index + current_len) {
                (None, length) => { // TODO: recover by find paired right bracket
                    test_condition_perrorln!{ log_enable, "parse array inner type failed, return none" }
                    return (None, current_len + length);
                }  
                (Some(inner), inner_length) => {
                    test_condition_perrorln!{ log_enable, "parse array inner type succeed" }
                    current_len += inner_length;
                    if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                        test_condition_perrorln!{ log_enable, "parse array inner type succeed, expect right bracket" }
                        current_len += 1;
                        return (
                            Some(SMType::Array(
                                Box::new(inner), 
                                StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + current_len - 1).end_pos)
                            )), 
                            inner_length + 2    
                        );
                    } else {
                        test_condition_perrorln!{ log_enable, "parse array failed, not right bracket" }
                        return lexer.push_expect("right bracket", index + current_len, current_len);
                    }
                } 
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            test_condition_perrorln!{ log_enable, "meet left paren, start tuple" }
            if lexer.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {  // still, '(, )' is not allowed here
                test_condition_perrorln!{ log_enable, "is left paren and right paren, it's unit" }
                return (Some(SMType::Prim(
                    PrimitiveType::Unit, 
                    StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + 1).end_pos),
                )), 2)
            }
            
            current_len += 1;
            let mut types = Vec::new();
            match SMType::parse(lexer, index + current_len) {
                (Some(ty), ty_len) => {
                    test_condition_perrorln!{ log_enable, "parse first tuple element succeed" }
                    types.push(ty);
                    current_len += ty_len;
                }
                (None, length) => {
                    test_condition_perrorln!{ log_enable, "parse first tuple element failed, return" }
                    return (None, length);
                }
            }

            loop {
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    test_condition_perrorln!{ log_enable, "parse tuple elements finished" }
                    current_len += 1;
                    break;
                }
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                        current_len += 1;
                        break;
                    }
                    match SMType::parse(lexer, index + current_len) {
                        (Some(ty), ty_len) => {
                            test_condition_perrorln!{ log_enable, "parse tuple elements succeed" }
                            types.push(ty);
                            current_len += ty_len;
                        }
                        (None, length) => {
                            return (None, current_len + length);
                        }
                    }
                }
            }
            
            let pos = StringPosition::from2(lexer.pos(index).start_pos, lexer.pos(index + current_len - 1).end_pos);
            if types.len() == 0 {
                unreachable!()
            } else if types.len() == 1 {
                lexer.push(SyntaxMessage::SingleItemTupleType{ pos: pos });
                return (Some(SMType::Tuple(types, pos)), current_len);
            } else if types.len() >= 2 {
                return (Some(SMType::Tuple(types, pos)), current_len);
            }
        }

        test_condition_perrorln!{ log_enable, "pass every check and return None at end" }
        let _log_enable = log_enable;
        return lexer.push_expects(vec!["primitive type keyword", "left bracket", "left parenthenes", "identifier"], index, 0);
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::Message;
        use message::SyntaxMessage;
        use super::SMType;
        use super::PrimitiveType;
        use common::StringPosition;
        use syntax::ast_item::TestCase;

        // Primitive
        ast_test_case!{ "u8", 1, make_str_pos!(1, 1, 1, 2),
            SMType::Prim(PrimitiveType::U8, make_str_pos!(1, 1, 1, 2))
        }
        ast_test_case!{ "i16", 1, make_str_pos!(1, 1, 1, 3),
            SMType::Prim(PrimitiveType::I16, make_str_pos!(1, 1, 1, 3))
        }
        ast_test_case!{ "char", 1, make_str_pos!(1, 1, 1, 4),
            SMType::Prim(PrimitiveType::Char, make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "f32", 1, make_str_pos!(1, 1, 1, 3),
            SMType::Prim(PrimitiveType::F32, make_str_pos!(1, 1, 1, 3))
        }
        ast_test_case!{ "bool", 1, make_str_pos!(1, 1, 1, 4),
            SMType::Prim(PrimitiveType::Bool, make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "string", 1, make_str_pos!(1, 1, 1, 6),
            SMType::Prim(PrimitiveType::SMString, make_str_pos!(1, 1, 1, 6))
        }

        // Simple user define
        ast_test_case!{ "helloworld_t", 1, make_str_pos!(1, 1, 1, 12),
            SMType::User("helloworld_t".to_owned(), make_str_pos!(1, 1, 1, 12))
        }

        // Array
        ast_test_case!{ "[u8]", 3, make_str_pos!(1, 1, 1, 4),
            SMType::Array(Box::new(
                SMType::Prim(PrimitiveType::U8, make_str_pos!(1, 2, 1, 3)),
            ), make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "[[helloworld_t]]", 5, make_str_pos!(1, 1, 1, 16),
            SMType::Array(Box::new(
                SMType::Array(Box::new( 
                    SMType::User("helloworld_t".to_owned(), make_str_pos!(1, 3, 1, 14))
                ), make_str_pos!(1, 2, 1, 15))
            ), make_str_pos!(1, 1, 1, 16))
        }

        // Unit
        ast_test_case!{ "()", 2, make_str_pos!(1, 1, 1, 2),
            SMType::Prim(PrimitiveType::Unit, make_str_pos!(1, 1, 1, 2))
        }

        // Tuple
        //           1234567890123
        ast_test_case!{ "(i32, string)", 5, make_str_pos!(1, 1, 1, 13),
            SMType::Tuple(
                vec![
                    SMType::Prim(PrimitiveType::I32, make_str_pos!(1, 2, 1, 4)),
                    SMType::Prim(PrimitiveType::SMString, make_str_pos!(1, 7, 1, 12)),
                ],
                make_str_pos!(1, 1, 1, 13)
            )
        }        //  12345678901234
        ast_test_case!{ "(char, hw_t, )", 6, make_str_pos!(1, 1, 1, 14),
            SMType::Tuple(
                vec![
                    SMType::Prim(PrimitiveType::Char, make_str_pos!(1, 2, 1, 5)),
                    SMType::User("hw_t".to_owned(), make_str_pos!(1, 8, 1, 11)),
                ],
                make_str_pos!(1, 1, 1, 14),
            )    //  0        1         2         3
        }        //  123456789012345678901234567890123456
        ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, make_str_pos!(1, 1, 1, 36), 
            SMType::Tuple(
                vec![
                    SMType::Array(Box::new(
                        SMType::Prim(PrimitiveType::Char, make_str_pos!(1, 3, 1, 6))
                    ), make_str_pos!(1, 2, 1, 7)),
                    SMType::Prim(PrimitiveType::I32, make_str_pos!(1, 10, 1, 12)),
                    SMType::User("u17".to_owned(), make_str_pos!(1, 15, 1, 17)),
                    SMType::Array(Box::new(
                        SMType::Tuple(
                            vec![
                                SMType::Prim(PrimitiveType::Unit, make_str_pos!(1, 22, 1, 23)),
                                SMType::Prim(PrimitiveType::U8, make_str_pos!(1, 26, 1, 27)),
                                SMType::User("f128".to_owned(), make_str_pos!(1, 30, 1, 33)),
                            ],
                            make_str_pos!(1, 21, 1, 34)
                        )
                    ), make_str_pos!(1, 20, 1, 35))
                ],
                make_str_pos!(1, 1, 1, 36)
            )
        }

        // Not tuple
        ast_test_case!{ "(i32)", 3, make_str_pos!(1, 1, 1, 5),
            SMType::Tuple(
                vec![
                    SMType::Prim(PrimitiveType::I32, make_str_pos!(1, 2, 1, 4)),
                ],
                make_str_pos!(1, 1, 1, 5),
            ),
            [
                Message::Syntax(SyntaxMessage::SingleItemTupleType{ pos: make_str_pos!(1, 1, 1, 5) })
            ]
        }
    }
}