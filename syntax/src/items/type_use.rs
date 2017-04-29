
// TypeUse = fPrimitiveType 
//           | fIdentifier 
//           | fLeftBracket TypeUse fRightBracket 
//           | fLeftParen TypeUse [[fComma Type]* | [fComma]] fRightParen 

// Some history, just for complain...
// First, only primitive types and one level of array are supported, 
//     recursive types are not supported, so TypeUseBase are primitive types, TypeUse are enum of them or an array      // TypeBase + Type
// Then recursive array are accepted and boxed value are accepted so there is only TypeUse 
//     and primitive types as enum member, including the recursive array                                                // Type
// Then position info is add and TypeUse is then devided into TypeUseBase and its position                              // TypeBase + Type
// Then more primitive types added and TypeUseBase become large, nearly twenty members                                  // TypeBase + Type
// Then tuple type and identifier as user define types are accepted and TypeUseBase is managed to 6 enum members 
//      and position info are moved back, so there is no more TypeUseBase again                                         // Type
// Then a Primtype enum is added instead of the keywordkind, for unit is not keyword but prim type                      // Type + PrimitiveType
// Then primitive type are removed from sytnax parse                                                                    // Type
// Then rename SMType to TypeUse                                                                                        // TypeUse
// Then enum members are hide, 17/4/10                                                                                  // TypeUse + ActualTypeUse

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;

#[derive(Eq, PartialEq, Clone)]
enum ActualTypeUse {
    Unit, 
    Simple(String),         // strpos for identifier
    Array(Box<TypeUse>),    // strpos for bracket
    Tuple(Vec<TypeUse>),    // strpos for paren
}
#[derive(Eq, PartialEq, Clone)]
pub struct TypeUse(ActualTypeUse, StringPosition);

impl ISyntaxItemFormat for TypeUse {
    fn format(&self, indent: u32) -> String {
        match (&self.0, &self.1) {
            (&ActualTypeUse::Unit, strpos) => format!("{}TypeUse '()' <{:?}>", TypeUse::indent_str(indent), strpos),
            (&ActualTypeUse::Simple(ref name), strpos) => format!("{}TypeUse '{}' <{:?}>", TypeUse::indent_str(indent), name, strpos),
            (&ActualTypeUse::Tuple(ref type_uses), strpos) => 
                format!("{}TypeUse::Tuple, <{:?}>{}",
                    TypeUse::indent_str(indent), strpos, 
                    type_uses.iter().fold(String::new(), |mut buf, typeuse| { buf.push_str("\n"); buf.push_str(&typeuse.format(indent + 1)); buf })),
            (&ActualTypeUse::Array(ref inner), strpos) => 
                format!("{}TypeUse::Array <{:?}>\n{}", 
                    TypeUse::indent_str(indent), strpos,
                    inner.format(indent + 1)),
        }
    }
}
impl fmt::Debug for TypeUse {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.format(0))
    }
}
impl TypeUse { // Get

    pub fn is_unit(&self) -> bool { match self.0 { ActualTypeUse::Unit => true, _ => false } }
    pub fn is_simple(&self) -> bool { match self.0 { ActualTypeUse::Simple(_) => true, _ => false } }
    pub fn is_array(&self) -> bool { match self.0 { ActualTypeUse::Array(_) => true, _ => false } }
    pub fn is_tuple(&self) -> bool { match self.0 { ActualTypeUse::Tuple(_) => true, _ => false } }

    pub fn get_simple(&self) -> Option<&String> { match self.0 { ActualTypeUse::Simple(ref name) => Some(name), _ => None } }
    pub fn get_array_inner(&self) -> Option<&TypeUse> { match self.0 { ActualTypeUse::Array(ref inner) => Some(inner.as_ref()), _ => None } }
    pub fn get_tuple_items(&self) -> Option<&Vec<TypeUse>> { match self.0 { ActualTypeUse::Tuple(ref type_uses) => Some(type_uses), _ => None } }

    pub fn get_all_strpos(&self) -> StringPosition { self.1 }
}

/// TypeUse Factory
pub struct TypeUseF;
impl TypeUseF { // New

    pub fn new_unit(strpos: StringPosition) -> TypeUse { TypeUse(ActualTypeUse::Unit, strpos) }
    pub fn new_simple(name: String, ident_strpos: StringPosition) -> TypeUse { TypeUse(ActualTypeUse::Simple(name), ident_strpos) }
    pub fn new_simple_test(name: &str, ident_strpos: StringPosition) -> TypeUse { TypeUse(ActualTypeUse::Simple(name.to_owned()), ident_strpos) }
    pub fn new_array(bracket_strpos: StringPosition, inner: TypeUse) -> TypeUse { TypeUse(ActualTypeUse::Array(Box::new(inner)), bracket_strpos) }
    pub fn new_tuple(paren_strpos: StringPosition, items: Vec<TypeUse>) -> TypeUse { TypeUse(ActualTypeUse::Tuple(items), paren_strpos) }
}

impl ISyntaxItem for TypeUse {

    fn pos_all(&self) -> StringPosition { self.1 }

    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_ident()
        || tokens.nth(index).is_seperator(SeperatorKind::LeftBracket)
        || tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes)
        || match tokens.nth(index).get_keyword() {
            Some(keyword) => keyword.is_prim_type(),
            None => false,    
        }
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<TypeUse>, usize) {
        #[cfg(feature = "trace_type_use_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[TypeUse]"); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_type_use_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        if tokens.nth(index).is_ident() {
            trace!{ "is ident, return" }
            return (Some(TypeUseF::new_simple(tokens.nth(index).get_identifier().unwrap(), tokens.pos(index))), 1);
        }
        if let Some(keyword) = tokens.nth(index).get_keyword() {
            trace!{ "is some keyword" }
            if keyword.is_prim_type() {
                trace!{ "is primitive type keyword, return" }
                return (Some(TypeUseF::new_simple(format!("{}", keyword), tokens.pos(index))), 1);
            } 
        }

        let mut current_len = 0;
        if tokens.nth(index).is_seperator(SeperatorKind::LeftBracket) {
            trace!{ "is left bracket, try get array inner type" }
            current_len += 1;
            match TypeUse::parse(tokens, messages, index + current_len) {
                (None, length) => { // TODO: recover by find paired right bracket
                    trace!{ "parse array inner type failed, return none" }
                    return (None, current_len + length);
                }  
                (Some(inner), inner_length) => {
                    trace!{ "parse array inner type succeed" }
                    current_len += inner_length;
                    if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                        trace!{ "parse array inner type succeed, expect right bracket" }
                        current_len += 1;
                        return (
                            Some(TypeUseF::new_array(StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len - 1)), inner)),
                            inner_length + 2    
                        );
                    } else {
                        trace!{ "parse array failed, not right bracket" }
                        return push_unexpect!(tokens, messages, "right bracket", index + current_len, current_len);
                    }
                } 
            }
        }

        if tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            trace!{ "meet left paren, start tuple" }
            if tokens.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {  // still, '(, )' is not allowed here
                trace!{ "is left paren and right paren, it's unit" }
                return (Some(TypeUseF::new_unit(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2)
            }
            
            current_len += 1;
            let mut types = Vec::new();
            match TypeUse::parse(tokens, messages, index + current_len) {
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
                if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    trace!{ "parse tuple elements finished" }
                    current_len += 1;
                    break;
                }
                if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                        current_len += 1;
                        break;
                    }
                    match TypeUse::parse(tokens, messages, index + current_len) {
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
            
            let pos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len - 1));
            if types.len() == 0 {
                unreachable!()
            } else if types.len() == 1 {
                messages.push(Message::new_by_str("Single item tuple type use", vec![(pos, "type use here")]));
                return (Some(TypeUseF::new_tuple(pos, types)), current_len);
            } else if types.len() >= 2 {
                return (Some(TypeUseF::new_tuple(pos, types)), current_len);
            }
        }

        trace!{ "pass every check and return None at end" }
        return push_unexpect!(tokens, messages, ["primitive type keyword", "left bracket", "left parenthenes", "identifier", ], index, 0);
    }
}

#[cfg(test)] #[test]
fn type_use_parse() {
    use super::super::TestCase;

    macro_rules! simple { 
        ($ident: expr, $strpos: expr) => (TypeUseF::new_simple_test($ident, $strpos));
    }

    // Primitive
    ast_test_case!{ "u8", 1, make_str_pos!(1, 1, 1, 2),
        simple!("u8", make_str_pos!(1, 1, 1, 2))
    }
    ast_test_case!{ "i32", 1, make_str_pos!(1, 1, 1, 3),
        simple!("i32", make_str_pos!(1, 1, 1, 3))
    }
    ast_test_case!{ "char", 1, make_str_pos!(1, 1, 1, 4),
        simple!("char", make_str_pos!(1, 1, 1, 4))
    }
    ast_test_case!{ "string", 1, make_str_pos!(1, 1, 1, 6),
        simple!("string", make_str_pos!(1, 1, 1, 6))
    }

    // Simple user define
    ast_test_case!{ "helloworld_t", 1, make_str_pos!(1, 1, 1, 12),
        simple!("helloworld_t", make_str_pos!(1, 1, 1, 12))
    }
    ast_test_case!{ "a", 1, make_str_pos!(1, 1, 1, 1),
        simple!("a", make_str_pos!(1, 1, 1, 1))
    }

    // Array
    ast_test_case!{ "[u8]", 3, make_str_pos!(1, 1, 1, 4),
        TypeUseF::new_array(make_str_pos!(1, 1, 1, 4),
            simple!("u8", make_str_pos!(1, 2, 1, 3)),
        )
    }
    ast_test_case!{ "[[helloworld_t]]", 5, make_str_pos!(1, 1, 1, 16),
        TypeUseF::new_array(make_str_pos!(1, 1, 1, 16), 
            TypeUseF::new_array(make_str_pos!(1, 2, 1, 15),
                simple!("helloworld_t", make_str_pos!(1, 3, 1, 14))
            )
        )
    }

    // Unit
    ast_test_case!{ "()", 2, make_str_pos!(1, 1, 1, 2),
        TypeUseF::new_unit(make_str_pos!(1, 1, 1, 2))
    }

    // Tuple
    //           1234567890123
    ast_test_case!{ "(i32, string)", 5, make_str_pos!(1, 1, 1, 13),
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 13), vec![
            simple!("i32", make_str_pos!(1, 2, 1, 4)),
            simple!("string", make_str_pos!(1, 7, 1, 12)),
        ])
    }        //  12345678901234
    ast_test_case!{ "(char, hw_t, )", 6, make_str_pos!(1, 1, 1, 14),
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 14), vec![
            simple!("char", make_str_pos!(1, 2, 1, 5)),
            simple!("hw_t", make_str_pos!(1, 8, 1, 11)),
        ])   //  0        1         2         3
    }        //  123456789012345678901234567890123456
    ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, make_str_pos!(1, 1, 1, 36), 
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 36), vec![
            TypeUseF::new_array(make_str_pos!(1, 2, 1, 7),
                simple!("char", make_str_pos!(1, 3, 1, 6))
            ),
            simple!("i32", make_str_pos!(1, 10, 1, 12)),
            simple!("u17", make_str_pos!(1, 15, 1, 17)),
            TypeUseF::new_array(make_str_pos!(1, 20, 1, 35),
                TypeUseF::new_tuple(make_str_pos!(1, 21, 1, 34), vec![
                    TypeUseF::new_unit(make_str_pos!(1, 22, 1, 23)),
                    simple!("u8", make_str_pos!(1, 26, 1, 27)),
                    simple!("f128", make_str_pos!(1, 30, 1, 33)),
                ])
            ), 
        ])
    } //             1234567
    ast_test_case!{ "(i233,)", 4, make_str_pos!(1, 1, 1, 7), 
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 7), vec![
            simple!("i233", make_str_pos!(1, 2, 1, 5))
        ])
    }
    ast_test_case!{ "(i32)", 3, make_str_pos!(1, 1, 1, 5),
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 5), vec![
            simple!("i32", make_str_pos!(1, 2, 1, 4)),
        ]),
        [ Message::new_by_str("Single item tuple type use", vec![(make_str_pos!(1, 1, 1, 5), "type use here")]) ]
    }

    // Auto generated mixed
    //               0        1         2
    //               12345678901234567890123
    ast_test_case!{ "((i8, clL, Kopu), f64,)", 12, make_str_pos!(1, 1, 1, 23),
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 23), vec![
            TypeUseF::new_tuple(make_str_pos!(1, 2, 1, 16), vec![
                simple!("i8", make_str_pos!(1, 3, 1, 4)),
                simple!("clL", make_str_pos!(1, 7, 1, 9)),
                simple!("Kopu", make_str_pos!(1, 12, 1, 15))
            ]), 
            simple!("f64", make_str_pos!(1, 19, 1, 21))
        ])
    } //             12345678
    ast_test_case!{ "[BJlbk4]", 3, make_str_pos!(1, 1, 1, 8),
        TypeUseF::new_array(make_str_pos!(1, 1, 1, 8), 
            simple!("BJlbk4", make_strpos!(1, 2, 1, 7)),
        ) //         0        1         2         3         4         5         6         7         8         9
    } //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    ast_test_case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)", 49, make_strpos!(1, 1, 1, 94),
        TypeUseF::new_tuple(make_strpos!(1, 1, 1, 94), vec![
            TypeUseF::new_tuple(make_str_pos!(1, 2, 1, 77), vec![
                simple!("char", make_strpos!(1, 3, 1, 6)), 
                simple!("jq", make_strpos!(1, 9, 1, 10)),
                TypeUseF::new_tuple(make_str_pos!(1, 13, 1, 75), vec![
                    TypeUseF::new_tuple(make_strpos!(1, 14, 1, 60), vec![
                        simple!("u8", make_strpos!(1, 15, 1, 16)),
                        TypeUseF::new_array(make_str_pos!(1, 19, 1, 54), 
                            TypeUseF::new_tuple(make_str_pos!(1, 20, 1, 53), vec![
                                TypeUseF::new_array(make_strpos!(1, 21, 1, 27), 
                                    simple!("o2fcd", make_strpos!(1, 22, 1, 26))
                                ),
                                TypeUseF::new_array(make_str_pos!(1, 30, 1, 34), 
                                    simple!("CKw", make_strpos!(1, 31, 1, 33))
                                ),
                                TypeUseF::new_tuple(make_strpos!(1, 37, 1, 52), vec![
                                    TypeUseF::new_array(make_strpos!(1, 38, 1, 45), 
                                        simple!("eCDn2I", make_strpos!(1, 39, 1, 44))
                                    ), 
                                    simple!("u16", make_strpos!(1, 48, 1, 50))
                                ])
                            ])
                        ),
                        simple!("i16", make_strpos!(1, 57, 1, 59))
                    ]),
                    TypeUseF::new_array(make_str_pos!(1, 63, 1, 69), 
                        simple!("pxplh", make_strpos!(1, 64, 1, 68))
                    ),
                    simple!("u32", make_strpos!(1, 72, 1, 74))
                ])
            ]),
            TypeUseF::new_array(make_strpos!(1, 80, 1, 88), 
                simple!("vrud2vC", make_str_pos!(1, 81, 1, 87))
            ),
            simple!("u64", make_strpos!(1, 91, 1,93))
        ])
    }
    ast_test_case!{ "sxM4", 1, make_strpos!(1, 1, 1, 4), simple!("sxM4", make_str_pos!(1, 1, 1, 4)) }
    //               0        1         2
    //               12345678901234567890123
    ast_test_case!{ "([pwi], [u64], i33, i8)", 13, make_strpos!(1, 1, 1, 23), 
        TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 23), vec![
            TypeUseF::new_array(make_str_pos!(1, 2, 1, 6), 
                simple!("pwi", make_strpos!(1, 3, 1, 5))
            ), 
            TypeUseF::new_array(make_strpos!(1, 9, 1, 13),
                simple!("u64", make_strpos!(1, 10, 1, 12))
            ),
            simple!("i33", make_strpos!(1, 16, 1, 18)),
            simple!("i8", make_strpos!(1, 21, 1, 22))
        ])
    }
}