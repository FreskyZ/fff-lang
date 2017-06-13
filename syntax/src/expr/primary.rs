///! fff-lang
///!
///! syntax/primary_expr
// PrimaryExpr = 
//     fIdentifier 
//     | fLiteral 
//     | fLeftParen fRighParen                                            // `()`, unit type and unit literal
//     | fLeftParen Expression fRightParen
//     | fLeftParen Expression [fComma Expression]+ fRightParen           // var tuple = (1, "abc", 'd') 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

// TODO: split ArrayDef TupleDef out

use std::fmt;

use codemap::Span;
use codemap::SymbolID;

use lexical::Token;
use lexical::SeperatorKind;
use lexical::KeywordKind;
use lexical::LitValue;
use lexical::NumLitValue;

use super::super::ParseSession;
use super::super::ParseResult;
use super::super::ISyntaxItemParse;
use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::binary::BinaryExpr;

#[cfg_attr(test, derive(Eq, PartialEq))]
enum ActualPrimaryExpr {
    Ident(SymbolID),
    Lit(LitValue),
    ParenExpr(BinaryExpr),
    TupleDef(Vec<BinaryExpr>),
    ArrayDef(Vec<BinaryExpr>),
    ArrayDupDef(BinaryExpr, BinaryExpr),   // Now position of `;` is not recorded because I think I don't use it
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct PrimaryExpr(ActualPrimaryExpr, Span);

impl ISyntaxItemFormat for PrimaryExpr {
    fn format(&self, indent: u32) -> String {
        match (&self.0, self.1) {
            (&ActualPrimaryExpr::Ident(ref ident_name), strpos) =>
                format!("{}Ident {:?} <{:?}>", 
                    PrimaryExpr::indent_str(indent), ident_name, strpos),
            (&ActualPrimaryExpr::Lit(ref lit_value), strpos) => 
                format!("{}Literal {:?} <{:?}>", 
                    PrimaryExpr::indent_str(indent), lit_value, strpos),
            (&ActualPrimaryExpr::ParenExpr(ref inner_expr), strpos) =>
                format!("{}ParenExpr <{:?}>\n{}", 
                    PrimaryExpr::indent_str(indent), strpos, 
                    inner_expr.format(indent + 1)),
            (&ActualPrimaryExpr::ArrayDupDef(ref expr1, ref expr2), strpos) =>
                format!("{}DefineArrayDuply <{:?}>\n{}\n{}",
                    PrimaryExpr::indent_str(indent), strpos, 
                    expr1.format(indent + 1),
                    expr2.format(indent + 2)),
            (&ActualPrimaryExpr::TupleDef(ref exprs), strpos) =>
                format!("{}DefineTuple <{:?}>{}", 
                    PrimaryExpr::indent_str(indent), strpos,
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 1)); buf })),
            (&ActualPrimaryExpr::ArrayDef(ref exprs), strpos) =>
                format!("{}DefineArray {}<{:?}>{}", 
                    PrimaryExpr::indent_str(indent), if exprs.is_empty() { "(empty) " } else { "" }, strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 1)); buf })),
        }
    }
}
impl fmt::Debug for PrimaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl PrimaryExpr { // New

    pub fn new_ident(ident_name: SymbolID, ident_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Ident(ident_name), ident_span)
    }
    pub fn new_lit(lit_value: LitValue, lit_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(lit_value), lit_span)
    }
    pub fn new_lit_num(num_val: NumLitValue, num_val_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Num(Some(num_val))), num_val_span)
    }
    pub fn new_lit_str(value: SymbolID, str_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Str(Some(value))), str_span)
    }
    pub fn new_lit_char(ch: char, ch_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Char(Some(ch))), ch_span)
    }
    pub fn new_lit_bool(value: bool, bool_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Bool(value)), bool_span)
    }
    pub fn new_unit(unit_span: Span) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Unit), unit_span)
    }
    pub fn new_paren(paren_span: Span, inner: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ParenExpr(inner), paren_span)
    }
    pub fn new_array_dup(bracket_span: Span, expr1: BinaryExpr, expr2: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDupDef(expr1, expr2), bracket_span)
    }
    pub fn new_tuple(paren_span: Span, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::TupleDef(exprs), paren_span)
    }
    pub fn new_array(bracket_span: Span, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDef(exprs), bracket_span)
    }
    
    pub fn get_all_span(&self) -> Span { self.1 }
}
impl PrimaryExpr { // Get

    // pub fn is_ident(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::Ident(_) => true, _ => false }
    // }    
    // pub fn is_lit(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::Lit(_) => true, _ => false }
    // }    
    // pub fn is_paren(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::ParenExpr(_) => true, _ => false }
    // }    
    // pub fn is_tuple(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::TupleDef(_) => true, _ => false }
    // }
    // pub fn is_array(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::ArrayDef(_) => true, _ => false }
    // }
    // pub fn is_array_dup(&self) -> bool {
    //     match self.0 { ActualPrimaryExpr::ArrayDupDef(_, _) => true, _ => false }
    // }

    // pub fn get_ident_name(&self) -> Option<&String> {
    //     match self.0 { ActualPrimaryExpr::Ident(ref ident_name) => Some(ident_name), _ => None }
    // }
    // pub fn get_lit_value(&self) -> Option<&LitValue> {
    //     match self.0 { ActualPrimaryExpr::Lit(ref lit_value) => Some(lit_value), _ => None }
    // }
    // pub fn get_paren_inner(&self) -> Option<&BinaryExpr> {
    //     match self.0 { ActualPrimaryExpr::ParenExpr(ref inner) => Some(inner), _ => None }
    // }
    // pub fn get_tuple_inners(&self) -> Option<&Vec<BinaryExpr>> {
    //     match self.0 { ActualPrimaryExpr::TupleDef(ref exprs) => Some(exprs), _ => None }
    // }
    // pub fn get_array_inners(&self) -> Option<&Vec<BinaryExpr>> {
    //     match self.0 { ActualPrimaryExpr::ArrayDef(ref exprs) => Some(exprs), _ => None }
    // }

    // pub fn get_array_dup_element(&self) -> Option<&BinaryExpr> {
    //     match self.0 { ActualPrimaryExpr::ArrayDupDef(ref expr1, _) => Some(expr1), _ => None }
    // }
    // pub fn get_array_dup_count(&self) -> Option<&BinaryExpr> {
    //     match self.0 { ActualPrimaryExpr::ArrayDupDef(_, ref expr2) => Some(expr2), _ => None }
    // }
}
impl ISyntaxItemGrammar for PrimaryExpr {
    fn is_first_final(sess: &ParseSession) -> bool {
        match sess.tk {
            &Token::Ident(_) 
            | &Token::Lit(_)
            | &Token::Sep(SeperatorKind::LeftParenthenes)
            | &Token::Sep(SeperatorKind::LeftBracket) => true,
            _ => false,
        }
    }
}
impl ISyntaxItemParse for PrimaryExpr {
    
    fn parse(sess: &mut ParseSession) -> ParseResult<PrimaryExpr> {
        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("start parsing, current token: {:?}", sess.tk);

        match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
            (&Token::Lit(ref lit_val), ref lit_val_span, _, _) => {
                sess.move_next();
                trace!("returning literal {:?} at {:?}", lit_val, lit_val_span);
                return Ok(PrimaryExpr::new_lit(lit_val.clone(), *lit_val_span));
            }
            (&Token::Ident(ref ident_name), ref ident_span, _, _) => {
                sess.move_next();
                trace!("returning identifier {:?} at {:?} ", ident_name, ident_span);
                return Ok(PrimaryExpr::new_ident(*ident_name, *ident_span));
            }
            (&Token::Keyword(KeywordKind::This), ref ident_span, _, _) => {
                sess.move_next();
                trace!("returning identifier this at {:?}", ident_span);
                return Ok(PrimaryExpr::new_ident(sess.symbols.intern_str("this"), *ident_span));
            }
            (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_span, 
                &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_span) => {
                sess.move_next2();
                trace!("returning unit at {:?}", left_paren_span);
                return Ok(PrimaryExpr::new_unit(left_paren_span.merge(right_paren_span)));
            }
            (&Token::Sep(SeperatorKind::LeftParenthenes), ref left_paren_span, _, _) => {
                sess.move_next();
                let mut maybe_tuple_exprs = Vec::new();
                let end_by_comma: bool;
                let ending_span: Span;
                loop {
                    maybe_tuple_exprs.push(BinaryExpr::parse(sess)?);
                    match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                        (&Token::Sep(SeperatorKind::Comma), _, &Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_span) => {
                            sess.move_next2();
                            end_by_comma = true;
                            ending_span = *right_paren_span;
                            break;
                        }
                        (&Token::Sep(SeperatorKind::RightParenthenes), ref right_paren_span, _, _) => {
                            sess.move_next();
                            end_by_comma = false;
                            ending_span = *right_paren_span;
                            break;
                        }
                        (&Token::Sep(SeperatorKind::Comma), _, _, _) => {
                            sess.move_next();
                        }
                        _ => {
                            return sess.push_unexpect("right paren, comma, expr");
                        }
                    }
                }

                trace!("after left paren's loop, exprs are {:?}", maybe_tuple_exprs);
                let paren_span = left_paren_span.merge(&ending_span);
                if maybe_tuple_exprs.len() == 1 && !end_by_comma {
                    return Ok(PrimaryExpr::new_paren(paren_span, maybe_tuple_exprs.into_iter().last().unwrap()));
                } else { // no length = 0 here because it is rejected before
                    return Ok(PrimaryExpr::new_tuple(paren_span, maybe_tuple_exprs));
                }
            }
            (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_span, 
                &Token::Sep(SeperatorKind::RightBracket), ref right_bracket_span) => {
                sess.move_next2();
                return Ok(PrimaryExpr::new_array(left_bracket_span.merge(right_bracket_span), Vec::new()));
            }
            (&Token::Sep(SeperatorKind::LeftBracket), ref left_bracket_span, _, _) => {
                sess.move_next();
                let expr1 = BinaryExpr::parse(sess)?; 

                if sess.tk == &Token::Sep(SeperatorKind::SemiColon) {
                    sess.move_next();
                    let expr2 = BinaryExpr::parse(sess)?; 
                    let right_bracket_span = sess.expect_sep(SeperatorKind::RightBracket)?;
                    trace!("parsing array dup def succeed, expr1: {:?}, expr2: {:?}", expr1, expr2);
                    return Ok(PrimaryExpr::new_array_dup(left_bracket_span.merge(&right_bracket_span), expr1, expr2));
                }
                
                trace!("parsing array def, before loop");
                let mut exprs = vec![expr1];
                loop {
                    match (sess.tk, sess.pos, sess.next_tk, sess.next_pos) {
                        (&Token::Sep(SeperatorKind::Comma), _, 
                            &Token::Sep(SeperatorKind::RightBracket), ref right_bracket_span) => {
                            sess.move_next2();
                            return Ok(PrimaryExpr::new_array(left_bracket_span.merge(right_bracket_span), exprs));
                        }
                        (&Token::Sep(SeperatorKind::RightBracket), ref right_bracket_span, _, _) => {
                            sess.move_next();
                            return Ok(PrimaryExpr::new_array(left_bracket_span.merge(right_bracket_span), exprs));
                        }
                        (&Token::Sep(SeperatorKind::Comma), _, _, _) => {
                            sess.move_next();
                            exprs.push(BinaryExpr::parse(sess)?);
                        }
                        _ => return sess.push_unexpect("comma, left bracket"),
                    }
                }
            }
            _ => return sess.push_unexpect("literal, identifier, left parenthenes, left bracekt"),
        }
    }
}

#[cfg(remove_this_after_expr_refactor)]
#[cfg(test)] #[test]
fn primary_expr_parse() {
    use super::super::ISyntaxItemWithStr;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),   
        PrimaryExpr::new_array(
            make_span!(0, 2), vec![
                BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(1, 1)))
            ]
        )
    }

    //                                        0        1         2         3         4
    //                                        01234567890123456789012345678901234567890123456
    assert_eq!{ PrimaryExpr::with_test_input("(463857, IEfN, atau8M, (fNAE, ((cAeJN4)), nHg))", 
        //                  1       2         3       4         5
        &mut make_symbols!["IEfN", "atau8M", "fNAE", "cAeJN4", "nHG"]), 
        PrimaryExpr::new_tuple(make_span!(0, 46), vec![
            BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(463857), make_span!(1, 6))),
            BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(1), make_span!(9, 12))),
            BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(2), make_span!(15, 20))),
            BinaryExpr::new_primary(PrimaryExpr::new_tuple(make_span!(23, 45), vec![
                BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(3), make_span!(24, 27))),
                BinaryExpr::new_primary(PrimaryExpr::new_paren(make_span!(30, 39), 
                    BinaryExpr::new_primary(PrimaryExpr::new_paren(make_span!(31, 38), 
                        BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(4), make_span!(32, 37)))
                    ))
                )),
                BinaryExpr::new_primary(PrimaryExpr::new_ident(make_id!(5), make_span!(42, 44)))
            ]))
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("10363"), PrimaryExpr::new_lit(LitValue::from(10363), make_span!(0, 4)) }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0         1         2         3         4         5         6         7         8         9         0         1         2         3       
    //   01234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567 8901234567890123456789012345678901234
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        PrimaryExpr::new_array(make_span!(0, 134), vec![
            BinaryExpr::new_paren(make_span!(1, 6), 
                BinaryExpr::new_lit(LitValue::from(0x7E), make_span!(2, 5))
            ),
            BinaryExpr::new_ident("FFGqfJe".to_owned(), make_span!(9, 15)),
            BinaryExpr::new_ident("I4".to_owned(), make_span!(18, 19)), 
            BinaryExpr::new_array(make_span!(22, 133), vec![
                BinaryExpr::new_tuple(make_span!(23, 105), vec![
                    BinaryExpr::new_ident("m7A".to_owned(), make_span!(24, 26)),
                    BinaryExpr::new_tuple(make_span!(29, 90), vec![
                        BinaryExpr::new_lit(LitValue::from(41), make_span!(30, 31)),
                        BinaryExpr::new_paren(make_span!(34, 68), 
                            BinaryExpr::new_array(make_span!(35, 67), vec![
                                BinaryExpr::new_tuple(make_span!(36, 60), vec![
                                    BinaryExpr::new_ident("jL".to_owned(), make_span!(37, 38)), 
                                    BinaryExpr::new_ident("rAn".to_owned(), make_span!(41, 43)),
                                    BinaryExpr::new_ident("K0FgLc7h".to_owned(), make_span!(46, 53)),
                                    BinaryExpr::new_lit(LitValue::from(true), make_span!(56, 59))
                                ]),
                                BinaryExpr::new_ident("C".to_owned(), make_span!(63, 63)),
                                BinaryExpr::new_ident("w".to_owned(), make_span!(66, 66)),
                            ])
                        ),
                        BinaryExpr::new_tuple(make_span!(71, 89), vec![
                            BinaryExpr::new_ident("J3cEFDG".to_owned(), make_span!(72, 78)),
                            BinaryExpr::new_ident("d".to_owned(), make_span!(81, 81)),
                            BinaryExpr::new_paren(make_span!(84, 88), 
                                BinaryExpr::new_ident("j8h".to_owned(), make_span!(85, 87))
                            )
                        ])
                    ]),
                    BinaryExpr::new_unit(make_span!(93, 94)),
                    BinaryExpr::new_ident("eIuArjF".to_owned(), make_span!(98, 104))
                ]),
                BinaryExpr::new_lit(LitValue::from(400), make_span!(108, 110)),
                BinaryExpr::new_lit(LitValue::from(0o535147505), make_span!(113, 123)),
                BinaryExpr::new_lit(LitValue::from(0xDB747), make_span!(126, 132))
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), PrimaryExpr::new_ident(make_id!(1), make_span!(0, 4)) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), PrimaryExpr::new_lit(LitValue::from(false), make_span!(0, 4)) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        PrimaryExpr::new_array(make_span!(0, 101), vec![
            BinaryExpr::new_ident("uy6".to_owned(), make_span!(1, 3)),
            BinaryExpr::new_lit(LitValue::from(4373577), make_span!(6, 12)),
            BinaryExpr::new_array(make_span!(15, 100), vec![
                BinaryExpr::new_tuple(make_span!(16, 36), vec![
                    BinaryExpr::new_ident("q".to_owned(), make_span!(17, 17)),
                    BinaryExpr::new_ident("AJBN0n".to_owned(), make_span!(20, 25)), 
                    BinaryExpr::new_ident("MDEgKh5".to_owned(), make_span!(28, 34))
                ]), 
                BinaryExpr::new_ident("KG".to_owned(), make_span!(39, 40)),
                BinaryExpr::new_tuple(make_span!(43, 74), vec![
                    BinaryExpr::new_ident("NsL".to_owned(), make_span!(44, 46)),
                    BinaryExpr::new_tuple(make_span!(49, 67), vec![
                        BinaryExpr::new_unit(make_span!(50, 51)),
                        BinaryExpr::new_ident("D".to_owned(), make_span!(54, 54)),
                        BinaryExpr::new_lit(LitValue::from(false), make_span!(57, 61)),
                        BinaryExpr::new_ident("d".to_owned(), make_span!(64, 64)),
                    ]),
                    BinaryExpr::new_lit(LitValue::from("H="), make_span!(70, 73))
                ]),
                BinaryExpr::new_lit(LitValue::from(true), make_span!(77, 80)),
                BinaryExpr::new_paren(make_span!(83, 99), 
                    BinaryExpr::new_tuple(make_span!(84, 98), vec![
                        BinaryExpr::new_ident("vvB3".to_owned(), make_span!(85, 88)),
                        BinaryExpr::new_lit(LitValue::from(true), make_span!(91, 94)),
                        BinaryExpr::new_lit(LitValue::from(5), make_span!(97, 97))
                    ])
                )
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("(() )"), PrimaryExpr::new_paren(make_span!(0, 4), BinaryExpr::new_unit(make_span!(1, 2))) }
    assert_eq!{ PrimaryExpr::with_test_str("((),)"), PrimaryExpr::new_tuple(make_span!(0, 4), vec![BinaryExpr::new_unit(make_span!(1, 2))]) }

    //                                      1234567890123
    assert_eq!{ PrimaryExpr::with_test_str("[Fhi;vjIj0Dt]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 12), 
            BinaryExpr::new_ident("Fhi".to_owned(), make_span!(1, 3)),
            BinaryExpr::new_ident("vjIj0Dt".to_owned(), make_span!(5, 11))
        )
    }

    assert_eq!{ PrimaryExpr::with_test_str("(\"o5\")"), 
        PrimaryExpr::new_paren(make_span!(0, 5), 
            BinaryExpr::new_lit(LitValue::from("o5"), make_span!(1, 4))
        )
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false;true]), 183455)"),
        PrimaryExpr::new_tuple(make_span!(0, 27), vec![
            BinaryExpr::new_ident("nn".to_owned(), make_span!(1, 2)),
            BinaryExpr::new_paren(make_span!(5, 18), 
                BinaryExpr::new_array_dup(make_span!(6, 17), 
                    BinaryExpr::new_lit(LitValue::from(false), make_span!(7, 11)),
                    BinaryExpr::new_lit(LitValue::from(true), make_span!(13, 16))
                )
            ),
            BinaryExpr::new_lit(LitValue::from(183455), make_span!(21, 26))
        ])
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        PrimaryExpr::new_tuple(make_span!(0, 77), vec![
            BinaryExpr::new_tuple(make_span!(1, 68), vec![
                BinaryExpr::new_lit(LitValue::from(true), make_span!(2, 5)),
                BinaryExpr::new_tuple(make_span!(8, 49), vec![
                    BinaryExpr::new_ident("mO".to_owned(), make_span!(9, 10)), 
                    BinaryExpr::new_array_dup(make_span!(13, 21), 
                        BinaryExpr::new_paren(make_span!(14, 18), 
                            BinaryExpr::new_ident("q5k".to_owned(), make_span!(15, 17))
                        ),
                        BinaryExpr::new_ident("a".to_owned(), make_span!(20, 20))
                    ),
                    BinaryExpr::new_paren(make_span!(24, 33), 
                        BinaryExpr::new_paren(make_span!(25, 32), 
                            BinaryExpr::new_paren(make_span!(26, 31), 
                                BinaryExpr::new_ident("KttG".to_owned(), make_span!(27, 30))
                            )
                        )
                    ),
                    BinaryExpr::new_tuple(make_span!(36, 48), vec![
                        BinaryExpr::new_ident("K5DJ".to_owned(), make_span!(37, 40)), 
                        BinaryExpr::new_ident("r".to_owned(), make_span!(43, 43)),
                        BinaryExpr::new_unit(make_span!(46, 47))
                    ]),
                ]),
                BinaryExpr::new_tuple(make_span!(52, 67), vec![
                    BinaryExpr::new_ident("McsaEdfdfalse".to_owned(), make_span!(53, 65))
                ])
            ]),
            BinaryExpr::new_ident("rIOKt".to_owned(), make_span!(71, 75))
        ])
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[\"il\", 0o52u32, sO04n]"),
        PrimaryExpr::new_array(make_span!(0, 21), vec![
            BinaryExpr::new_lit(LitValue::from("il"), make_span!(1, 4)),
            BinaryExpr::new_lit(LitValue::from(0o52u32), make_span!(7, 13)), 
            BinaryExpr::new_ident("sO04n".to_owned(), make_span!(16, 20))
        ])
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f';()]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 7), 
            BinaryExpr::new_lit(LitValue::from('f'), make_span!(1, 3)),
            BinaryExpr::new_unit(make_span!(5, 6))
        )
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), PrimaryExpr::new_array(make_span!(0, 1), vec![]) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        PrimaryExpr::new_array(make_span!(0, 43), vec![
            BinaryExpr::new_lit(LitValue::from(8), make_span!(1, 1)),
            BinaryExpr::new_lit(LitValue::from("@=?GF"), make_span!(4, 10)), 
            BinaryExpr::new_lit(LitValue::from(87f32), make_span!(13, 17)),
            BinaryExpr::new_lit(LitValue::from(1340323.74f64), make_span!(20, 32)),
            BinaryExpr::new_ident("FKOxAvx5".to_owned(), make_span!(35, 42))
        ])
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ PrimaryExpr::with_test_str(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        PrimaryExpr::new_array(make_span!(2, 62), vec![
            BinaryExpr::new_array(make_span!(3, 21), vec![
                BinaryExpr::new_ident("dnr4".to_owned(), make_span!(4, 7)),
                BinaryExpr::new_ident("lGFd3yL".to_owned(), make_span!(10, 16)),
                BinaryExpr::new_ident("tJ".to_owned(), make_span!(19, 20))
            ]),
            BinaryExpr::new_array(make_span!(24, 48), vec![
                BinaryExpr::new_lit(LitValue::from('\\'), make_span!(25, 28)), 
                BinaryExpr::new_ident("p".to_owned(), make_span!(31, 31)),
                BinaryExpr::new_tuple(make_span!(34, 43), vec![
                    BinaryExpr::new_ident("xGaBwiL".to_owned(), make_span!(35, 41))
                ]),
                BinaryExpr::new_ident("DE".to_owned(), make_span!(46, 47))
            ]),
            BinaryExpr::new_lit(LitValue::from(true), make_span!(51, 54)),
            BinaryExpr::new_ident("aB8aE".to_owned(), make_span!(57, 61))
        ])
    } 

    // Previous manual tests
    //                                      0        1           2          3         4         5           6
    //                                      12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ PrimaryExpr::with_test_str("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
        PrimaryExpr::new_array(make_span!(0, 65), vec![
            BinaryExpr::new_ident("abc".to_owned(), make_span!(1, 3)),
            BinaryExpr::new_lit(LitValue::from(123u32), make_span!(6, 11)),
            BinaryExpr::new_lit(LitValue::from("456"), make_span!(14, 18)),
            BinaryExpr::new_lit(LitValue::from('\u{0065}'), make_span!(21, 28)),
            BinaryExpr::new_lit(LitValue::from(false), make_span!(31, 35)),
            BinaryExpr::new_unit(make_span!(38, 39)),
            BinaryExpr::new_paren(make_span!(42, 44), 
                BinaryExpr::new_ident("a".to_owned(), make_span!(43, 43))
            ),
            BinaryExpr::new_tuple(make_span!(47, 62), vec![
                BinaryExpr::new_ident("abc".to_owned(), make_span!(48, 50)),
                BinaryExpr::new_lit(LitValue::from("hello"), make_span!(53, 59)),
            ])
        ])
    }       

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), PrimaryExpr::new_unit(make_span!(0, 30)) }

    //                                      0        1         2
    //                                      123456789012345678901
    assert_eq!{ PrimaryExpr::with_test_str("[[123u32, abc]; 4567]"), 
        PrimaryExpr::new_array_dup(make_span!(0, 20), 
            BinaryExpr::new_array(make_span!(1, 13), vec![
                BinaryExpr::new_lit(LitValue::from(123u32), make_span!(2, 7)), 
                BinaryExpr::new_ident("abc".to_owned(), make_span!(10, 12))
            ]),
            BinaryExpr::new_lit(LitValue::from(4567), make_span!(16, 19))
        )
    }
}

#[cfg(test)] #[test] #[ignore]
fn primary_expr_errors() {
    use super::super::ISyntaxItemWithStr;
    use message::Message;
    use message::MessageCollection;

    const UNEXPECTED_SYMBOL: &'static str = "Unexpected symbol";

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(1, ]"), (
        None, 
        3,
        make_messages![
            Message::with_help_by_str(UNEXPECTED_SYMBOL, 
                vec![(make_span!(4, 4), "Meet Seperator `]`(RightBracket) <0>1:5-1:5")],
                vec![&("Expect ".to_owned() + "error_strings::ExpectExpression")]
            )
        ], 
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("(,)"), (
        None, 
        1,
        make_messages![],
    )}

    assert_eq!{ PrimaryExpr::with_test_str_ret_size_messages("[1, )"), (
        None,
        1,
        make_messages![],
    )}
}