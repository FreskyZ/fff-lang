///! fff-lang
///! 
///! syntax/typeuse

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
use lexical::Token;
use lexical::SeperatorKind;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;

#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Clone)]            // Move out of boxed
pub enum ActualTypeUse {
    Unit, 
    Simple(String),         // strpos for identifier
    Array(Box<TypeUse>),    // strpos for bracket
    Tuple(Vec<TypeUse>),    // strpos for paren
}
#[cfg_attr(test, derive(Eq, PartialEq))]
#[derive(Clone)]            // Move out of boxed
pub struct TypeUse {
    pub actual: ActualTypeUse, 
    pub all_strpos: StringPosition,
}
impl ISyntaxItemFormat for TypeUse {
    fn format(&self, indent: u32) -> String {
        match (&self.actual, &self.all_strpos) {
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

    pub fn is_unit(&self) -> bool { match self.actual { ActualTypeUse::Unit => true, _ => false } }
    pub fn is_simple(&self) -> bool { match self.actual { ActualTypeUse::Simple(_) => true, _ => false } }
    pub fn is_array(&self) -> bool { match self.actual { ActualTypeUse::Array(_) => true, _ => false } }
    pub fn is_tuple(&self) -> bool { match self.actual { ActualTypeUse::Tuple(_) => true, _ => false } }

    pub fn get_simple(&self) -> Option<&String> { match self.actual { ActualTypeUse::Simple(ref name) => Some(name), _ => None } }
    pub fn get_array_inner(&self) -> Option<&TypeUse> { match self.actual { ActualTypeUse::Array(ref inner) => Some(inner.as_ref()), _ => None } }
    pub fn get_tuple_items(&self) -> Option<&Vec<TypeUse>> { match self.actual { ActualTypeUse::Tuple(ref type_uses) => Some(type_uses), _ => None } }

    pub fn get_all_strpos(&self) -> StringPosition { self.all_strpos }
}

/// TypeUse Factory
pub struct TypeUseF;
impl TypeUseF { // New

    /// decided to be internal new standard name
    fn new_(actual: ActualTypeUse, strpos: StringPosition) -> TypeUse { TypeUse{ actual, all_strpos: strpos } }
    pub fn new_unit(strpos: StringPosition) -> TypeUse { TypeUseF::new_(ActualTypeUse::Unit, strpos) }
    pub fn new_simple(name: String, ident_strpos: StringPosition) -> TypeUse { TypeUseF::new_(ActualTypeUse::Simple(name), ident_strpos) }
    pub fn new_simple_test(name: &str, ident_strpos: StringPosition) -> TypeUse { TypeUseF::new_(ActualTypeUse::Simple(name.to_owned()), ident_strpos) }
    pub fn new_array(bracket_strpos: StringPosition, inner: TypeUse) -> TypeUse { TypeUseF::new_(ActualTypeUse::Array(Box::new(inner)), bracket_strpos) }
    pub fn new_tuple(paren_strpos: StringPosition, items: Vec<TypeUse>) -> TypeUse { TypeUseF::new_(ActualTypeUse::Tuple(items), paren_strpos) }
}
impl ISyntaxItemGrammar for TypeUse {
    fn is_first_final(sess: &ParseSession) -> bool {
        match sess.tk {
            &Token::Ident(_) 
            | &Token::Sep(SeperatorKind::LeftBracket)
            | &Token::Sep(SeperatorKind::LeftParenthenes) => true,
            &Token::Keyword(kw) => kw.is_prim_type(),
            _ => false,
        }
    }
}
impl ISyntaxItemParse for TypeUse {

    fn parse(sess: &mut ParseSession) -> ParseResult<TypeUse> {
        #[cfg(feature = "trace_type_use_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[TypeUse]"); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_type_use_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        match (sess.tk, sess.pos) {
            (&Token::Ident(ref ident), ref ident_strpos) => {
                sess.move_next();
                return Ok(TypeUseF::new_simple(ident.clone(), *ident_strpos));
            }
            (&Token::Keyword(keyword), ref keyword_strpos) if keyword.is_prim_type() => {
                sess.move_next();
                return Ok(TypeUseF::new_simple(format!("{}", keyword), *keyword_strpos));
            }
            (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_strpos) => {
                sess.move_next();
                let inner = TypeUse::parse(sess)?;
                let right_bracket_strpos = sess.expect_sep(SeperatorKind::RightBracket)?;
                return Ok(TypeUseF::new_array(StringPosition::merge(*left_bracket_strpos, right_bracket_strpos), inner));
            }
            (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_strpos) => {
                sess.move_next();
                if let (&Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) = (sess.tk, sess.pos) { 
                    sess.move_next();
                    return Ok(TypeUseF::new_unit(StringPosition::merge(*left_paren_strpos, *right_paren_strpos)));
                }

                let mut tuple_types = Vec::new();
                let ending_strpos: StringPosition;
                let end_by_comma: bool;
                tuple_types.push(TypeUse::parse(sess)?);
                loop {
                    match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                        (&Token::Sep(SeperatorKind::Comma), _, 
                            &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) => {
                            sess.move_next2();
                            ending_strpos = *right_paren_strpos;
                            end_by_comma = true;
                            break;        
                        }
                        (&Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos, _, _) => {
                            sess.move_next();
                            ending_strpos = *right_paren_strpos;
                            end_by_comma = false;
                            break;
                        }
                        (&Token::Sep(SeperatorKind::Comma), _, _, _) => {
                            sess.move_next();
                            tuple_types.push(TypeUse::parse(sess)?);
                        }
                        _ => return sess.push_unexpect("comma, right parenthenes"),
                    }
                }
                
                let paren_pair_strpos = StringPosition::merge(*left_paren_strpos, ending_strpos);
                if tuple_types.len() == 1 && !end_by_comma {
                    sess.push_message(Message::new_by_str("Single item tuple type use", vec![(paren_pair_strpos, "type use here")]));
                    return Ok(TypeUseF::new_tuple(paren_pair_strpos, tuple_types));
                } else { // len() == 0 already rejected
                    return Ok(TypeUseF::new_tuple(paren_pair_strpos, tuple_types));
                }
            }
            _ => return sess.push_unexpect("primitive type keyword, left bracket, left parenthenes, identifier"),
        }
    }
}

#[cfg(test)] #[test]
fn type_use_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ TypeUse::with_test_str("u8"), TypeUseF::new_simple("u8".to_owned(), make_strpos!(1, 1, 1, 2)) }
    assert_eq!{ TypeUse::with_test_str("i32"), TypeUseF::new_simple("i32".to_owned(), make_strpos!(1, 1, 1, 3)) }
    assert_eq!{ TypeUse::with_test_str("char"), TypeUseF::new_simple("char".to_owned(), make_strpos!(1, 1, 1, 4)) }
    assert_eq!{ TypeUse::with_test_str("string"), TypeUseF::new_simple("string".to_owned(), make_strpos!(1, 1, 1, 6)) }
    assert_eq!{ TypeUse::with_test_str("helloworld_t"), TypeUseF::new_simple("helloworld_t".to_owned(), make_strpos!(1, 1, 1, 12)) }

    assert_eq!{ TypeUse::with_test_str("()"), TypeUseF::new_unit(make_strpos!(1, 1, 1, 2)) }

    assert_eq!{ TypeUse::with_test_str("[u8]"),
        TypeUseF::new_array(make_strpos!(1, 1, 1, 4), 
            TypeUseF::new_simple("u8".to_owned(), make_strpos!(1, 2, 1, 3))
        )
    }
    assert_eq!{ TypeUse::with_test_str("[[he_t]]"),
        TypeUseF::new_array(make_strpos!(1, 1, 1, 8),
            TypeUseF::new_array(make_strpos!(1, 2, 1, 7), 
                TypeUseF::new_simple("he_t".to_owned(), make_strpos!(1, 3, 1, 6))
            )
        )
    }

    // // Tuple
    // (i32,)
    // //           1234567890123
    // ast_test_case!{ "(i32, string)", 5, make_str_pos!(1, 1, 1, 13),
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 13), vec![
    //         simple!("i32", make_str_pos!(1, 2, 1, 4)),
    //         simple!("string", make_str_pos!(1, 7, 1, 12)),
    //     ])
    // }        //  12345678901234
    // ast_test_case!{ "(char, hw_t, )", 6, make_str_pos!(1, 1, 1, 14),
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 14), vec![
    //         simple!("char", make_str_pos!(1, 2, 1, 5)),
    //         simple!("hw_t", make_str_pos!(1, 8, 1, 11)),
    //     ])   //  0        1         2         3
    // }        //  123456789012345678901234567890123456
    // ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, make_str_pos!(1, 1, 1, 36), 
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 36), vec![
    //         TypeUseF::new_array(make_str_pos!(1, 2, 1, 7),
    //             simple!("char", make_str_pos!(1, 3, 1, 6))
    //         ),
    //         simple!("i32", make_str_pos!(1, 10, 1, 12)),
    //         simple!("u17", make_str_pos!(1, 15, 1, 17)),
    //         TypeUseF::new_array(make_str_pos!(1, 20, 1, 35),
    //             TypeUseF::new_tuple(make_str_pos!(1, 21, 1, 34), vec![
    //                 TypeUseF::new_unit(make_str_pos!(1, 22, 1, 23)),
    //                 simple!("u8", make_str_pos!(1, 26, 1, 27)),
    //                 simple!("f128", make_str_pos!(1, 30, 1, 33)),
    //             ])
    //         ), 
    //     ])
    // } //             1234567
    // ast_test_case!{ "(i233,)", 4, make_str_pos!(1, 1, 1, 7), 
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 7), vec![
    //         simple!("i233", make_str_pos!(1, 2, 1, 5))
    //     ])
    // }
    // ast_test_case!{ "(i32)", 3, make_str_pos!(1, 1, 1, 5),
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 5), vec![
    //         simple!("i32", make_str_pos!(1, 2, 1, 4)),
    //     ]),
    //     [ Message::new_by_str("Single item tuple type use", vec![(make_str_pos!(1, 1, 1, 5), "type use here")]) ]
    // }

    // // Auto generated mixed
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "((i8, clL, Kopu), f64,)", 12, make_str_pos!(1, 1, 1, 23),
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 23), vec![
    //         TypeUseF::new_tuple(make_str_pos!(1, 2, 1, 16), vec![
    //             simple!("i8", make_str_pos!(1, 3, 1, 4)),
    //             simple!("clL", make_str_pos!(1, 7, 1, 9)),
    //             simple!("Kopu", make_str_pos!(1, 12, 1, 15))
    //         ]), 
    //         simple!("f64", make_str_pos!(1, 19, 1, 21))
    //     ])
    // } //             12345678
    // ast_test_case!{ "[BJlbk4]", 3, make_str_pos!(1, 1, 1, 8),
    //     TypeUseF::new_array(make_str_pos!(1, 1, 1, 8), 
    //         simple!("BJlbk4", make_strpos!(1, 2, 1, 7)),
    //     ) //         0        1         2         3         4         5         6         7         8         9
    // } //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    // ast_test_case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)", 49, make_strpos!(1, 1, 1, 94),
    //     TypeUseF::new_tuple(make_strpos!(1, 1, 1, 94), vec![
    //         TypeUseF::new_tuple(make_str_pos!(1, 2, 1, 77), vec![
    //             simple!("char", make_strpos!(1, 3, 1, 6)), 
    //             simple!("jq", make_strpos!(1, 9, 1, 10)),
    //             TypeUseF::new_tuple(make_str_pos!(1, 13, 1, 75), vec![
    //                 TypeUseF::new_tuple(make_strpos!(1, 14, 1, 60), vec![
    //                     simple!("u8", make_strpos!(1, 15, 1, 16)),
    //                     TypeUseF::new_array(make_str_pos!(1, 19, 1, 54), 
    //                         TypeUseF::new_tuple(make_str_pos!(1, 20, 1, 53), vec![
    //                             TypeUseF::new_array(make_strpos!(1, 21, 1, 27), 
    //                                 simple!("o2fcd", make_strpos!(1, 22, 1, 26))
    //                             ),
    //                             TypeUseF::new_array(make_str_pos!(1, 30, 1, 34), 
    //                                 simple!("CKw", make_strpos!(1, 31, 1, 33))
    //                             ),
    //                             TypeUseF::new_tuple(make_strpos!(1, 37, 1, 52), vec![
    //                                 TypeUseF::new_array(make_strpos!(1, 38, 1, 45), 
    //                                     simple!("eCDn2I", make_strpos!(1, 39, 1, 44))
    //                                 ), 
    //                                 simple!("u16", make_strpos!(1, 48, 1, 50))
    //                             ])
    //                         ])
    //                     ),
    //                     simple!("i16", make_strpos!(1, 57, 1, 59))
    //                 ]),
    //                 TypeUseF::new_array(make_str_pos!(1, 63, 1, 69), 
    //                     simple!("pxplh", make_strpos!(1, 64, 1, 68))
    //                 ),
    //                 simple!("u32", make_strpos!(1, 72, 1, 74))
    //             ])
    //         ]),
    //         TypeUseF::new_array(make_strpos!(1, 80, 1, 88), 
    //             simple!("vrud2vC", make_str_pos!(1, 81, 1, 87))
    //         ),
    //         simple!("u64", make_strpos!(1, 91, 1,93))
    //     ])
    // }
    // ast_test_case!{ "sxM4", 1, make_strpos!(1, 1, 1, 4), simple!("sxM4", make_str_pos!(1, 1, 1, 4)) }
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "([pwi], [u64], i33, i8)", 13, make_strpos!(1, 1, 1, 23), 
    //     TypeUseF::new_tuple(make_str_pos!(1, 1, 1, 23), vec![
    //         TypeUseF::new_array(make_str_pos!(1, 2, 1, 6), 
    //             simple!("pwi", make_strpos!(1, 3, 1, 5))
    //         ), 
    //         TypeUseF::new_array(make_strpos!(1, 9, 1, 13),
    //             simple!("u64", make_strpos!(1, 10, 1, 12))
    //         ),
    //         simple!("i33", make_strpos!(1, 16, 1, 18)),
    //         simple!("i8", make_strpos!(1, 21, 1, 22))
    //     ])
    // }
}