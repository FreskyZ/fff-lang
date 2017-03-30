
// Type = fPrimitiveType | fIdentifier | fLeftBracket Type fRightBracket | fLeftParen Type [fComma Type]* [fComma] fRightParen 

// Some history, just for complain...
// First, only primitive types and one level of array are supported, 
//     recursive types are not supported, so TypeUseBase are primitive types, TypeUse are enum of them or an array        // TypeUseBase + TypeUse
// Then recursive array are accepted and boxed value are accepted so there is only TypeUse 
//     and primitive types as enum member, including the recursive array                                                // TypeUse
// Then position info is add and TypeUse is then devided into TypeUseBase and its position                                // TypeUseBase + TypeUse
// Then more primitive types added and TypeUseBase become large, nearly twenty members                                   // TypeUseBase + TypeUse
// Then tuple type and identifier as user define types are accepted and TypeUseBase is managed to 6 enum members 
//      and position info are moved back, so there is no more TypeUseBase again                                          // TypeUse
// Then a Primtype enum is added instead of the keywordkind, for unit is not keyword but prim type                      // TypeUse + PrimitiveType
// Then primitive type are removed from sytnax parse                                                                    // TypeUse

use std::fmt;

use codepos::StringPosition;
use message::SyntaxMessage;
use message::Message;
use message::MessageCollection;

use lexical::Lexer;
use lexical::SeperatorKind;

use super::ISyntaxItem;
use super::ISyntaxItemFormat;

#[derive(Eq, PartialEq, Clone)]
pub enum TypeUse {
    Unit(StringPosition),
    Base(String, StringPosition),           // position for the identifier, primitive type not aware here
    Tuple(Vec<TypeUse>, StringPosition),     // position for ()
    Array(Box<TypeUse>, StringPosition),     // position for []
}
impl ISyntaxItemFormat for TypeUse {
    fn format(&self, indent: u32) -> String {
        match self {
            &TypeUse::Unit(ref strpos) => format!("{}UnitTypeUse <{:?}>", TypeUse::indent_str(indent), strpos),
            &TypeUse::Base(ref name, ref strpos) => format!("{}SimpleTypeUse {} <{:?}>", TypeUse::indent_str(indent), name, strpos),
            &TypeUse::Tuple(ref types, ref strpos) => 
                format!("{}TupleTypeUse\n{}Paren at <{:?}>{}",
                    TypeUse::indent_str(indent), TypeUse::indent_str(indent + 1),
                    strpos, types.iter().fold(String::new(), |mut buf, fftype| { buf.push_str("\n"); buf.push_str(&fftype.format(indent + 1)); buf })
                ),
            &TypeUse::Array(ref inner, ref strpos) => 
                format!("{}ArrayTypeUse\n{}Bracket at <{:?}>\n{}", 
                    TypeUse::indent_str(indent), TypeUse::indent_str(indent + 1), strpos, inner.format(indent + 1)
                ),
        }
    }
}
impl fmt::Debug for TypeUse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}

impl TypeUse {

    pub fn is_unit(&self) -> bool {
        match *self {
            TypeUse::Unit(_) => true,
            _ => false,
        }
    }
    pub fn pos(&self) -> StringPosition { self.pos_all() } // for outter use
}

impl ISyntaxItem for TypeUse {

    fn pos_all(&self) -> StringPosition { 
        match *self {
            TypeUse::Unit(ref pos) => *pos,
            TypeUse::Base(ref _name, ref pos) => *pos,
            TypeUse::Tuple(ref _types, ref pos) => *pos,
            TypeUse::Array(ref _inner, ref pos) => *pos,
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

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<TypeUse>, usize) {
        #[cfg(feature = "trace_type_use_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[TypeUse]"); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_type_use_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        if lexer.nth(index).is_ident() {
            trace!{ "is ident, return" }
            return (Some(TypeUse::Base(lexer.nth(index).get_identifier().unwrap(), lexer.pos(index))), 1);
        }
        if let Some(keyword) = lexer.nth(index).get_keyword() {
            trace!{ "is some keyword" }
            if keyword.is_prim_type() {
                trace!{ "is primitive type keyword, return" }
                return (Some(TypeUse::Base(format!("{}", keyword), lexer.pos(index))), 1);
            } 
        }

        let mut current_len = 0;
        if lexer.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            trace!{ "is left bracket, try get array inner type" }
            current_len += 1;
            match TypeUse::parse(lexer, messages, index + current_len) {
                (None, length) => { // TODO: recover by find paired right bracket
                    trace!{ "parse array inner type failed, return none" }
                    return (None, current_len + length);
                }  
                (Some(inner), inner_length) => {
                    trace!{ "parse array inner type succeed" }
                    current_len += inner_length;
                    if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                        trace!{ "parse array inner type succeed, expect right bracket" }
                        current_len += 1;
                        return (
                            Some(TypeUse::Array(
                                Box::new(inner), 
                                StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len - 1))
                            )), 
                            inner_length + 2    
                        );
                    } else {
                        trace!{ "parse array failed, not right bracket" }
                        return push_unexpect!(lexer, messages, "right bracket", index + current_len, current_len);
                    }
                } 
            }
        }

        if lexer.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            trace!{ "meet left paren, start tuple" }
            if lexer.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {  // still, '(, )' is not allowed here
                trace!{ "is left paren and right paren, it's unit" }
                return (Some(TypeUse::Unit(
                    StringPosition::merge(lexer.pos(index), lexer.pos(index + 1)),
                )), 2)
            }
            
            current_len += 1;
            let mut types = Vec::new();
            match TypeUse::parse(lexer, messages, index + current_len) {
                (Some(ty), ty_len) => {
                    trace!{ "parse first tuple element succeed" }
                    types.push(ty);
                    current_len += ty_len;
                }
                (None, length) => {
                    trace!{ "parse first tuple element failed, return" }
                    return (None, length);
                }
            }

            loop {
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    trace!{ "parse tuple elements finished" }
                    current_len += 1;
                    break;
                }
                if lexer.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    if lexer.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                        current_len += 1;
                        break;
                    }
                    match TypeUse::parse(lexer, messages, index + current_len) {
                        (Some(ty), ty_len) => {
                            trace!{ "parse tuple elements succeed" }
                            types.push(ty);
                            current_len += ty_len;
                        }
                        (None, length) => {
                            return (None, current_len + length);
                        }
                    }
                }
            }
            
            let pos = StringPosition::merge(lexer.pos(index), lexer.pos(index + current_len - 1));
            if types.len() == 0 {
                unreachable!()
            } else if types.len() == 1 {
                messages.push(SyntaxMessage::SingleItemTupleType{ pos: pos });
                return (Some(TypeUse::Tuple(types, pos)), current_len);
            } else if types.len() >= 2 {
                return (Some(TypeUse::Tuple(types, pos)), current_len);
            }
        }

        trace!{ "pass every check and return None at end" }
        return push_unexpect!(lexer, messages, ["primitive type keyword", "left bracket", "left parenthenes", "identifier", ], index, 0);
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn ast_smtype_parse() {
        use message::LegacyMessage as Message;
        use message::SyntaxMessage;
        use super::TypeUse;
        use codepos::StringPosition;
        use super::super::ast_item::TestCase;

        // Primitive
        ast_test_case!{ "u8", 1, make_str_pos!(1, 1, 1, 2),
            TypeUse::Base("u8".to_owned(), make_str_pos!(1, 1, 1, 2))
        }
        ast_test_case!{ "i16", 1, make_str_pos!(1, 1, 1, 3),
            TypeUse::Base("i16".to_owned(), make_str_pos!(1, 1, 1, 3))
        }
        ast_test_case!{ "char", 1, make_str_pos!(1, 1, 1, 4),
            TypeUse::Base("char".to_owned(), make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "f32", 1, make_str_pos!(1, 1, 1, 3),
            TypeUse::Base("f32".to_owned(), make_str_pos!(1, 1, 1, 3))
        }
        ast_test_case!{ "bool", 1, make_str_pos!(1, 1, 1, 4),
            TypeUse::Base("bool".to_owned(), make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "string", 1, make_str_pos!(1, 1, 1, 6),
            TypeUse::Base("string".to_owned(), make_str_pos!(1, 1, 1, 6))
        }

        // Simple user define
        ast_test_case!{ "helloworld_t", 1, make_str_pos!(1, 1, 1, 12),
            TypeUse::Base("helloworld_t".to_owned(), make_str_pos!(1, 1, 1, 12))
        }

        // Array
        ast_test_case!{ "[u8]", 3, make_str_pos!(1, 1, 1, 4),
            TypeUse::Array(Box::new(
                TypeUse::Base("u8".to_owned(), make_str_pos!(1, 2, 1, 3)),
            ), make_str_pos!(1, 1, 1, 4))
        }
        ast_test_case!{ "[[helloworld_t]]", 5, make_str_pos!(1, 1, 1, 16),
            TypeUse::Array(Box::new(
                TypeUse::Array(Box::new( 
                    TypeUse::Base("helloworld_t".to_owned(), make_str_pos!(1, 3, 1, 14))
                ), make_str_pos!(1, 2, 1, 15))
            ), make_str_pos!(1, 1, 1, 16))
        }

        // Unit
        ast_test_case!{ "()", 2, make_str_pos!(1, 1, 1, 2),
            TypeUse::Unit(make_str_pos!(1, 1, 1, 2))
        }

        // Tuple
        //           1234567890123
        ast_test_case!{ "(i32, string)", 5, make_str_pos!(1, 1, 1, 13),
            TypeUse::Tuple(
                vec![
                    TypeUse::Base("i32".to_owned(), make_str_pos!(1, 2, 1, 4)),
                    TypeUse::Base("string".to_owned(), make_str_pos!(1, 7, 1, 12)),
                ],
                make_str_pos!(1, 1, 1, 13)
            )
        }        //  12345678901234
        ast_test_case!{ "(char, hw_t, )", 6, make_str_pos!(1, 1, 1, 14),
            TypeUse::Tuple(
                vec![
                    TypeUse::Base("char".to_owned(), make_str_pos!(1, 2, 1, 5)),
                    TypeUse::Base("hw_t".to_owned(), make_str_pos!(1, 8, 1, 11)),
                ],
                make_str_pos!(1, 1, 1, 14),
            )    //  0        1         2         3
        }        //  123456789012345678901234567890123456
        ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, make_str_pos!(1, 1, 1, 36), 
            TypeUse::Tuple(
                vec![
                    TypeUse::Array(Box::new(
                        TypeUse::Base("char".to_owned(), make_str_pos!(1, 3, 1, 6))
                    ), make_str_pos!(1, 2, 1, 7)),
                    TypeUse::Base("i32".to_owned(), make_str_pos!(1, 10, 1, 12)),
                    TypeUse::Base("u17".to_owned(), make_str_pos!(1, 15, 1, 17)),
                    TypeUse::Array(Box::new(
                        TypeUse::Tuple(
                            vec![
                                TypeUse::Unit(make_str_pos!(1, 22, 1, 23)),
                                TypeUse::Base("u8".to_owned(), make_str_pos!(1, 26, 1, 27)),
                                TypeUse::Base("f128".to_owned(), make_str_pos!(1, 30, 1, 33)),
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
            TypeUse::Tuple(
                vec![
                    TypeUse::Base("i32".to_owned(), make_str_pos!(1, 2, 1, 4)),
                ],
                make_str_pos!(1, 1, 1, 5),
            ),
            [
                Message::Syntax(SyntaxMessage::SingleItemTupleType{ pos: make_str_pos!(1, 1, 1, 5) })
            ]
        }
    }
}