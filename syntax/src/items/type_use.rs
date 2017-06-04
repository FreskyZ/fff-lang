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

use codemap::Span;
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
    pub all_strpos: Span,
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

    pub fn get_all_strpos(&self) -> Span { self.all_strpos }
}

/// TypeUse Factory
pub struct TypeUseF;
impl TypeUseF { // New

    /// decided to be internal new standard name
    fn new_(actual: ActualTypeUse, strpos: Span) -> TypeUse { TypeUse{ actual, all_strpos: strpos } }
    pub fn new_unit(strpos: Span) -> TypeUse { TypeUseF::new_(ActualTypeUse::Unit, strpos) }
    pub fn new_simple(name: String, ident_strpos: Span) -> TypeUse { TypeUseF::new_(ActualTypeUse::Simple(name), ident_strpos) }
    pub fn new_simple_test(name: &str, ident_strpos: Span) -> TypeUse { TypeUseF::new_(ActualTypeUse::Simple(name.to_owned()), ident_strpos) }
    pub fn new_array(bracket_strpos: Span, inner: TypeUse) -> TypeUse { TypeUseF::new_(ActualTypeUse::Array(Box::new(inner)), bracket_strpos) }
    pub fn new_tuple(paren_strpos: Span, items: Vec<TypeUse>) -> TypeUse { TypeUseF::new_(ActualTypeUse::Tuple(items), paren_strpos) }
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
                return Ok(TypeUseF::new_array(left_bracket_strpos.merge(&right_bracket_strpos), inner));
            }
            (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_strpos) => {
                sess.move_next();
                if let (&Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_strpos) = (sess.tk, sess.pos) { 
                    sess.move_next();
                    return Ok(TypeUseF::new_unit(left_paren_strpos.merge(&right_paren_strpos)));
                }

                let mut tuple_types = Vec::new();
                let ending_strpos: Span;
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
                
                let paren_pair_strpos = left_paren_strpos.merge(&ending_strpos);
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

    assert_eq!{ TypeUse::with_test_str("u8"), TypeUseF::new_simple("u8".to_owned(), make_span!(0, 1)) }
    assert_eq!{ TypeUse::with_test_str("i32"), TypeUseF::new_simple("i32".to_owned(), make_span!(0, 2)) }
    assert_eq!{ TypeUse::with_test_str("char"), TypeUseF::new_simple("char".to_owned(), make_span!(0, 3)) }
    assert_eq!{ TypeUse::with_test_str("string"), TypeUseF::new_simple("string".to_owned(), make_span!(0, 5)) }
    assert_eq!{ TypeUse::with_test_str("helloworld_t"), TypeUseF::new_simple("helloworld_t".to_owned(), make_span!(0, 11)) }

    assert_eq!{ TypeUse::with_test_str("()"), TypeUseF::new_unit(make_span!(0, 1)) }

    assert_eq!{ TypeUse::with_test_str("[u8]"),
        TypeUseF::new_array(make_span!(0, 3), 
            TypeUseF::new_simple("u8".to_owned(), make_span!(1, 2))
        )
    }
    assert_eq!{ TypeUse::with_test_str("[[he_t]]"),
        TypeUseF::new_array(make_span!(0, 7),
            TypeUseF::new_array(make_span!(1, 6), 
                TypeUseF::new_simple("he_t".to_owned(), make_span!(2, 5))
            )
        )
    }

    // // Tuple
    // (i32,)
    // //           1234567890123
    // ast_test_case!{ "(i32, string)", 5, make_span!(0, 12),
    //     TypeUseF::new_tuple(make_span!(0, 12), vec![
    //         simple!("i32", make_span!(1, 3)),
    //         simple!("string", make_span!(6, 11)),
    //     ])
    // }        //  12345678901234
    // ast_test_case!{ "(char, hw_t, )", 6, make_span!(0, 13),
    //     TypeUseF::new_tuple(make_span!(0, 13), vec![
    //         simple!("char", make_span!(1, 4)),
    //         simple!("hw_t", make_span!(7, 10)),
    //     ])   //  0        1         2         3
    // }        //  123456789012345678901234567890123456
    // ast_test_case!{ "([char], i32, u17, [((), u8, f128)])", 20, make_span!(0, 35), 
    //     TypeUseF::new_tuple(make_span!(0, 35), vec![
    //         TypeUseF::new_array(make_span!(1, 6),
    //             simple!("char", make_span!(2, 5))
    //         ),
    //         simple!("i32", make_span!(9, 11)),
    //         simple!("u17", make_span!(14, 16)),
    //         TypeUseF::new_array(make_span!(19, 34),
    //             TypeUseF::new_tuple(make_span!(20, 33), vec![
    //                 TypeUseF::new_unit(make_span!(21, 22)),
    //                 simple!("u8", make_span!(25, 26)),
    //                 simple!("f128", make_span!(29, 32)),
    //             ])
    //         ), 
    //     ])
    // } //             1234567
    // ast_test_case!{ "(i233,)", 4, make_span!(0, 6), 
    //     TypeUseF::new_tuple(make_span!(0, 6), vec![
    //         simple!("i233", make_span!(1, 4))
    //     ])
    // }
    // ast_test_case!{ "(i32)", 3, make_span!(0, 4),
    //     TypeUseF::new_tuple(make_span!(0, 4), vec![
    //         simple!("i32", make_span!(1, 3)),
    //     ]),
    //     [ Message::new_by_str("Single item tuple type use", vec![(make_span!(0, 4), "type use here")]) ]
    // }

    // // Auto generated mixed
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "((i8, clL, Kopu), f64,)", 12, make_span!(0, 22),
    //     TypeUseF::new_tuple(make_span!(0, 22), vec![
    //         TypeUseF::new_tuple(make_span!(1, 15), vec![
    //             simple!("i8", make_span!(2, 3)),
    //             simple!("clL", make_span!(6, 8)),
    //             simple!("Kopu", make_span!(11, 14))
    //         ]), 
    //         simple!("f64", make_span!(18, 20))
    //     ])
    // } //             12345678
    // ast_test_case!{ "[BJlbk4]", 3, make_span!(0, 7),
    //     TypeUseF::new_array(make_span!(0, 7), 
    //         simple!("BJlbk4", make_span!(1, 6)),
    //     ) //         0        1         2         3         4         5         6         7         8         9
    // } //             1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234
    // ast_test_case!{ "((char, jq, ((u8, [([o2fcd], [CKw], ([eCDn2I], u16,))], i16), [pxplh], u32),), [vrud2vC], u64)", 49, make_span!(0, 93),
    //     TypeUseF::new_tuple(make_span!(0, 93), vec![
    //         TypeUseF::new_tuple(make_span!(1, 76), vec![
    //             simple!("char", make_span!(2, 5)), 
    //             simple!("jq", make_span!(8, 9)),
    //             TypeUseF::new_tuple(make_span!(12, 74), vec![
    //                 TypeUseF::new_tuple(make_span!(13, 59), vec![
    //                     simple!("u8", make_span!(14, 15)),
    //                     TypeUseF::new_array(make_span!(18, 53), 
    //                         TypeUseF::new_tuple(make_span!(19, 52), vec![
    //                             TypeUseF::new_array(make_span!(20, 26), 
    //                                 simple!("o2fcd", make_span!(21, 25))
    //                             ),
    //                             TypeUseF::new_array(make_span!(29, 33), 
    //                                 simple!("CKw", make_span!(30, 32))
    //                             ),
    //                             TypeUseF::new_tuple(make_span!(36, 51), vec![
    //                                 TypeUseF::new_array(make_span!(37, 44), 
    //                                     simple!("eCDn2I", make_span!(38, 43))
    //                                 ), 
    //                                 simple!("u16", make_span!(47, 49))
    //                             ])
    //                         ])
    //                     ),
    //                     simple!("i16", make_span!(56, 58))
    //                 ]),
    //                 TypeUseF::new_array(make_span!(62, 68), 
    //                     simple!("pxplh", make_span!(63, 67))
    //                 ),
    //                 simple!("u32", make_span!(71, 73))
    //             ])
    //         ]),
    //         TypeUseF::new_array(make_span!(79, 87), 
    //             simple!("vrud2vC", make_span!(80, 86))
    //         ),
    //         simple!("u64", make_span!(90, 92))
    //     ])
    // }
    // ast_test_case!{ "sxM4", 1, make_span!(0, 3), simple!("sxM4", make_span!(0, 3)) }
    // //               0        1         2
    // //               12345678901234567890123
    // ast_test_case!{ "([pwi], [u64], i33, i8)", 13, make_span!(0, 22), 
    //     TypeUseF::new_tuple(make_span!(0, 22), vec![
    //         TypeUseF::new_array(make_span!(1, 5), 
    //             simple!("pwi", make_span!(2, 4))
    //         ), 
    //         TypeUseF::new_array(make_span!(8, 12),
    //             simple!("u64", make_span!(9, 11))
    //         ),
    //         simple!("i33", make_span!(15, 17)),
    //         simple!("i8", make_span!(20, 21))
    //     ])
    // }
}