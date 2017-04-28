
// PrimaryExpr = 
//     fIdentifier 
//     | fLiteral 
//     | fLeftParen fRighParen                                            // `()`, unit type and unit literal
//     | fLeftParen Expression fRightParen
//     | fLeftParen Expression [fComma Expression]+ fRightParen           // var tuple = (1, "abc", 'd') 
//     | fLeftBracket [Expression [fComma Expression]*] fRightBracket     // var array = [1, 2, 3, a, b, c]
//     | fLeftBracket Expression fSemiColon Expression fRightBracket      // var array = [false; 100]

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;
use lexical::LitValue;
use lexical::NumLitValue;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::binary::BinaryExpr;

#[derive(Eq, PartialEq)]
enum ActualPrimaryExpr {
    Ident(String),
    Lit(LitValue),
    ParenExpr(BinaryExpr),
    TupleDef(Vec<BinaryExpr>),
    ArrayDef(Vec<BinaryExpr>),
    ArrayDupDef(BinaryExpr, BinaryExpr),   // Now position of `;` is not recorded because I think I don't use it
}
#[derive(Eq, PartialEq)]
pub struct PrimaryExpr(ActualPrimaryExpr, StringPosition);

impl ISyntaxItemFormat for PrimaryExpr {
    fn format(&self, indent: u32) -> String {
        match (&self.0, self.1) {
            (&ActualPrimaryExpr::Ident(ref ident_name), strpos) =>
                format!("{}Ident '{}' <{:?}>", 
                    PrimaryExpr::indent_str(indent), ident_name, strpos),
            (&ActualPrimaryExpr::Lit(ref lit_value), strpos) => 
                format!("{}Literal {} <{:?}>", 
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
                format!("{}DefineArray <{:?}>{}", 
                    PrimaryExpr::indent_str(indent), strpos, 
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

    pub fn new_ident(ident_name: String, ident_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Ident(ident_name), ident_strpos)
    }
    pub fn new_lit(lit_value: LitValue, lit_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(lit_value), lit_strpos)
    }
    pub fn new_lit_num(num_val: NumLitValue, num_val_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Num(Some(num_val))), num_val_strpos)
    }
    pub fn new_lit_str(value: String, str_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Str(Some(value))), str_strpos)
    }
    pub fn new_lit_char(ch: char, ch_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Char(Some(ch))), ch_strpos)
    }
    pub fn new_lit_bool(value: bool, bool_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Bool(value)), bool_strpos)
    }
    pub fn new_unit(unit_strpos: StringPosition) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::Lit(LitValue::Unit), unit_strpos)
    }
    pub fn new_paren(paren_strpos: StringPosition, inner: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ParenExpr(inner), paren_strpos)
    }
    pub fn new_array_dup(bracket_strpos: StringPosition, expr1: BinaryExpr, expr2: BinaryExpr) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDupDef(expr1, expr2), bracket_strpos)
    }
    pub fn new_tuple(paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::TupleDef(exprs), paren_strpos)
    }
    pub fn new_array(bracket_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PrimaryExpr {
        PrimaryExpr(ActualPrimaryExpr::ArrayDef(exprs), bracket_strpos)
    }
}
impl PrimaryExpr { // Get

    pub fn get_strpos(&self) -> StringPosition {
        self.1
    }

    pub fn is_ident(&self) -> bool {
        match self.0 { ActualPrimaryExpr::Ident(_) => true, _ => false }
    }    
    pub fn is_lit(&self) -> bool {
        match self.0 { ActualPrimaryExpr::Lit(_) => true, _ => false }
    }    
    pub fn is_paren(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ParenExpr(_) => true, _ => false }
    }    
    pub fn is_tuple(&self) -> bool {
        match self.0 { ActualPrimaryExpr::TupleDef(_) => true, _ => false }
    }
    pub fn is_array(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ArrayDef(_) => true, _ => false }
    }
    pub fn is_array_dup(&self) -> bool {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(_, _) => true, _ => false }
    }

    pub fn get_ident_name(&self) -> Option<&String> {
        match self.0 { ActualPrimaryExpr::Ident(ref ident_name) => Some(ident_name), _ => None }
    }
    pub fn get_lit_value(&self) -> Option<&LitValue> {
        match self.0 { ActualPrimaryExpr::Lit(ref lit_value) => Some(lit_value), _ => None }
    }
    pub fn get_paren_inner(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ParenExpr(ref inner) => Some(inner), _ => None }
    }
    pub fn get_tuple_inners(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0 { ActualPrimaryExpr::TupleDef(ref exprs) => Some(exprs), _ => None }
    }
    pub fn get_array_inners(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0 { ActualPrimaryExpr::ArrayDef(ref exprs) => Some(exprs), _ => None }
    }

    pub fn get_array_dup_element(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(ref expr1, _) => Some(expr1), _ => None }
    }
    pub fn get_array_dup_count(&self) -> Option<&BinaryExpr> {
        match self.0 { ActualPrimaryExpr::ArrayDupDef(_, ref expr2) => Some(expr2), _ => None }
    }
}
impl ISyntaxItem for PrimaryExpr {
    
    fn pos_all(&self) -> StringPosition { self.1 }
    
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        tokens.nth(index).is_ident()
        || tokens.nth(index).is_lit()
        || tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes)
        || tokens.nth(index).is_seperator(SeperatorKind::LeftBracket)
    }

    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<PrimaryExpr>, usize) {

        #[cfg(feature = "trace_primary_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PrimaryExpr: {}]", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_primary_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        trace!("start parsing, current token: {:?}", tokens.nth(index));

        if tokens.nth(index).is_lit() {
            return (Some(PrimaryExpr::new_lit(tokens.nth(index).get_lit_val().unwrap(), tokens.pos(index))), 1);
        }
        match tokens.nth(index).get_identifier() {
            Some(ident) => {
                trace!("yes this is a identifier: {:?}, going to return", ident);
                return (Some(PrimaryExpr::new_ident(ident.clone(), tokens.pos(index))), 1);
            }
            None => (),
        }
        if tokens.nth(index).is_keyword(KeywordKind::This) {
            return (Some(PrimaryExpr::new_ident("this".to_owned(), tokens.pos(index))), 1);
        }

        trace!("parsing primary not literal or identifier");
        if tokens.nth(index).is_seperator(SeperatorKind::LeftParenthenes) {
            if tokens.nth(index + 1).is_seperator(SeperatorKind::RightParenthenes) {
                return (Some(PrimaryExpr::new_unit(StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)))), 2);
            }

            let mut current_len = 1;
            let mut exprs = Vec::new();
            let mut end_by_comma = false;
            loop {
                match BinaryExpr::parse(tokens, messages, index + current_len) {
                    (None, length) => {
                        trace!("parsing paren expression get expression failed");
                        return (None, current_len + length);
                    }
                    (Some(expr), expr_len) => {
                        current_len += expr_len;
                        exprs.push(expr);
                    }
                }
                if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightParenthenes) {
                    current_len += 1;
                    break; // Finished
                } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_len + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    end_by_comma = true;
                    current_len += 2; 
                    break; // Finished
                } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                    current_len += 1;
                    continue;
                } else {
                    return push_unexpect!(tokens, messages, "Right paren", index + current_len, current_len);
                }
            }

            trace!("after left paren's loop, exprs are {:?}", exprs);
            let paren_strpos = StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len - 1));
            if exprs.len() == 0 {
                unreachable!() // very confident about this
            } else if exprs.len() == 1 && !end_by_comma {
                return (Some(PrimaryExpr::new_paren(paren_strpos, exprs.into_iter().last().unwrap())), current_len);
            } else {
                return (Some(PrimaryExpr::new_tuple(paren_strpos, exprs)), current_len);
            }
        }

        if tokens.nth(index).is_seperator(SeperatorKind::LeftBracket) {
             // Empty array literal, accept in syntax parse, currently denied in codegen
            if tokens.nth(index + 1).is_seperator(SeperatorKind::RightBracket) {
                return (
                    Some(PrimaryExpr::new_array(
                        StringPosition::merge(tokens.pos(index), tokens.pos(index + 1)),
                        Vec::new(), 
                    )), 
                    2
                );
            }
            match BinaryExpr::parse(tokens, messages, index + 1) {
                (None, length) => {
                    trace!("parsing array (dup) def failed, parse expr1 return none");
                    return (None, length);  // recover by find paired right bracket
                }
                (Some(expr1), expr1_len) => {
                    trace!("parsing array (dup) def get expr1: {:?} with length {} and next is {:?}", expr1, expr1_len, tokens.nth(index + 1 + expr1_len));
                    if tokens.nth(index + 1 + expr1_len).is_seperator(SeperatorKind::SemiColon) {
                        // let semicolon_pos = tokens.pos(index + 1 + expr1_len); // semicolon is not recorded now
                        match BinaryExpr::parse(tokens, messages, index + 2 + expr1_len) {
                            (None, length) => {
                                trace!("parsing array dup def failed, parse expr2 failed");
                                return (None, expr1_len + 2 + length);
                            } 
                            (Some(expr2), expr2_len) => {
                                if tokens.nth(index + 2 + expr1_len + expr2_len).is_seperator(SeperatorKind::RightBracket) {
                                    trace!("parsing array dup def succeed, expr1: {:?}, expr2: {:?}", expr1, expr2);
                                    return (
                                        Some(PrimaryExpr::new_array_dup(
                                            StringPosition::merge(tokens.pos(index), tokens.pos(index + expr1_len + expr2_len + 2)), 
                                            expr1, expr2,
                                        )),
                                        expr1_len + expr2_len + 3
                                    );
                                } else {
                                    trace!("parsing array dup def failed, not followed right bracket");
                                    return push_unexpect!(tokens, messages, "Right bracket after array dup def", index + 3 + expr1_len + expr2_len, expr1_len + expr2_len + 1);
                                }
                            }
                        }
                    }

                    trace!("parsing array def, before loop");
                    let mut current_len = 1 + expr1_len; // 1 for left bracket
                    let mut exprs = vec![expr1];
                    loop {
                        trace!("parsing array def, in loop, current: {:?}", tokens.nth(index + current_len));
                        if tokens.nth(index + current_len).is_seperator(SeperatorKind::RightBracket) {
                            trace!("parsing array def succeed, exprs: {:?}", exprs);
                            return (
                                Some(PrimaryExpr::new_array(
                                    StringPosition::merge(tokens.pos(index), tokens.pos(index + current_len)),
                                    exprs, 
                                )), 
                                current_len + 1
                            );
                        } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma)  // Accept [1, 2, 3, abc, ] 
                            && tokens.nth(index + current_len + 1).is_seperator(SeperatorKind::RightBracket) {
                            trace!("parsing array def succeed, exprs: {:?}", exprs);
                            return (
                                Some(PrimaryExpr::new_array(
                                    StringPosition::merge(tokens.pos(index), tokens.pos(index + 1 + current_len)),
                                    exprs, 
                                )), 
                                current_len + 2
                            );
                        } else if tokens.nth(index + current_len).is_seperator(SeperatorKind::Comma) {
                            current_len += 1;
                            match BinaryExpr::parse(tokens, messages, index + current_len) {
                                (Some(exprn), exprn_len) => {
                                    trace!("parsing array def, get expression n {:?}", exprn);
                                    current_len += exprn_len;
                                    exprs.push(exprn);
                                }
                                (None, length) => {
                                    trace!("parsing array def failed, parse expression return none");
                                    return (None, current_len + length);
                                }
                            }
                        } else {
                            return push_unexpect!(tokens, messages, ["comma", "left bracket", ], index + current_len, current_len);
                        }
                    }
                }
            }
        }

        trace!("Failed in prim expr parse, not start with left paren or left bracket");
        return push_unexpect!(tokens, messages, ["identifier", "literal", "array def", ], index, 0);
    }
} 

#[cfg(test)] #[test]
fn primary_expr_parse() {
    use super::super::ISyntaxItemWithStr;

    // this is the loop of tokens.nth(current) is left bracket does not cover everything and infinite loop is here
    assert_eq!{ PrimaryExpr::with_test_str("[a]"),   
        PrimaryExpr::new_array(
            make_strpos!(1, 1, 1, 3), vec![
                BinaryExpr::new_primary(PrimaryExpr::new_ident("a".to_owned(), make_strpos!(1, 2, 1, 2)))
            ]
        )
    }

    //                                      0        1         2         3         4
    //                                      12345678901234567890123456789012345678901234567
    assert_eq!{ PrimaryExpr::with_test_str("(463857, IEfN, atau8M, (fNAE, ((cAeJN4)), nHg))"), 
        PrimaryExpr::new_tuple(make_strpos!(1, 1, 1, 47), vec![
            BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(463857), make_strpos!(1, 2, 1, 7))),
            BinaryExpr::new_primary(PrimaryExpr::new_ident("IEfN".to_owned(), make_strpos!(1, 10, 1, 13))),
            BinaryExpr::new_primary(PrimaryExpr::new_ident("atau8M".to_owned(), make_strpos!(1, 16, 1, 21))),
            BinaryExpr::new_primary(PrimaryExpr::new_tuple(make_strpos!(1, 24, 1, 46), vec![
                BinaryExpr::new_primary(PrimaryExpr::new_ident("fNAE".to_owned(), make_strpos!(1, 25, 1, 28))),
                BinaryExpr::new_primary(PrimaryExpr::new_paren(make_strpos!(1, 31, 1, 40), 
                    BinaryExpr::new_primary(PrimaryExpr::new_paren(make_strpos!(1, 32, 1, 39), 
                        BinaryExpr::new_primary(PrimaryExpr::new_ident("cAeJN4".to_owned(), make_strpos!(1, 33, 1, 38)))
                    ))
                )),
                BinaryExpr::new_primary(PrimaryExpr::new_ident("nHg".to_owned(), make_strpos!(1, 43, 1, 45)))
            ]))
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("10363"), PrimaryExpr::new_lit(LitValue::from(10363), make_strpos!(1, 1, 1, 5)) }

    assert_eq!{ PrimaryExpr::with_test_str(
    //   0        1         2         3         4         5         6         7         8         9         0        1         2         3       
    //   12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678 1234567890123456789012345678901234567
        "[(0x7E), FFGqfJe, I4, [(m7A, (41, ([(jL, rAn, K0FgLc7h, true), C, w]), (J3cEFDG, d, (j8h))), (), \neIuArjF), 400, 0o535147505, 0xDB747]]"),
        PrimaryExpr::new_array(make_strpos!(1, 1, 2, 37), vec![
            BinaryExpr::new_paren(make_strpos!(1, 2, 1, 7), 
                BinaryExpr::new_lit(LitValue::from(0x7E), make_strpos!(1, 3, 1, 6))
            ),
            BinaryExpr::new_ident("FFGqfJe".to_owned(), make_strpos!(1, 10, 1, 16)),
            BinaryExpr::new_ident("I4".to_owned(), make_strpos!(1, 19, 1, 20)), 
            BinaryExpr::new_array(make_strpos!(1, 23, 2, 36), vec![
                BinaryExpr::new_tuple(make_strpos!(1, 24, 2, 8), vec![
                    BinaryExpr::new_ident("m7A".to_owned(), make_strpos!(1, 25, 1, 27)),
                    BinaryExpr::new_tuple(make_strpos!(1, 30, 1, 91), vec![
                        BinaryExpr::new_lit(LitValue::from(41), make_strpos!(1, 31, 1, 32)),
                        BinaryExpr::new_paren(make_strpos!(1, 35, 1, 69), 
                            BinaryExpr::new_array(make_strpos!(1, 36, 1, 68), vec![
                                BinaryExpr::new_tuple(make_strpos!(1, 37, 1, 61), vec![
                                    BinaryExpr::new_ident("jL".to_owned(), make_strpos!(1, 38, 1, 39)), 
                                    BinaryExpr::new_ident("rAn".to_owned(), make_strpos!(1, 42, 1, 44)),
                                    BinaryExpr::new_ident("K0FgLc7h".to_owned(), make_strpos!(1, 47, 1, 54)),
                                    BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 57, 1, 60))
                                ]),
                                BinaryExpr::new_ident("C".to_owned(), make_strpos!(1, 64, 1, 64)),
                                BinaryExpr::new_ident("w".to_owned(), make_strpos!(1, 67, 1, 67)),
                            ])
                        ),
                        BinaryExpr::new_tuple(make_strpos!(1, 72, 1, 90), vec![
                            BinaryExpr::new_ident("J3cEFDG".to_owned(), make_strpos!(1, 73, 1, 79)),
                            BinaryExpr::new_ident("d".to_owned(), make_strpos!(1, 82, 1, 82)),
                            BinaryExpr::new_paren(make_strpos!(1, 85, 1, 89), 
                                BinaryExpr::new_ident("j8h".to_owned(), make_strpos!(1, 86, 1, 88))
                            )
                        ])
                    ]),
                    BinaryExpr::new_unit(make_strpos!(1, 94, 1, 95)),
                    BinaryExpr::new_ident("eIuArjF".to_owned(), make_strpos!(2, 1, 2, 7))
                ]),
                BinaryExpr::new_lit(LitValue::from(400), make_strpos!(2, 11, 2, 13)),
                BinaryExpr::new_lit(LitValue::from(0o535147505), make_strpos!(2, 16, 2, 26)),
                BinaryExpr::new_lit(LitValue::from(0xDB747), make_strpos!(2, 29, 2, 35))
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("CMDoF"), PrimaryExpr::new_ident("CMDoF".to_owned(), make_strpos!(1, 1, 1, 5)) }
    assert_eq!{ PrimaryExpr::with_test_str("false"), PrimaryExpr::new_lit(LitValue::from(false), make_strpos!(1, 1, 1, 5)) }

    
    //                                      0        1         2         3         4         5         6          7          8         9         A
    //                                      12345678901234567890123456789012345678901234567890123456789012345678901 234 5678901234567890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[uy6, 4373577, [(q, AJBN0n, MDEgKh5,), KG, (NsL, ((), D, false, d, ), \"H=\"), true, ((vvB3, true, 5))]]"), 
        PrimaryExpr::new_array(make_strpos!(1, 1, 1, 102), vec![
            BinaryExpr::new_ident("uy6".to_owned(), make_strpos!(1, 2, 1, 4)),
            BinaryExpr::new_lit(LitValue::from(4373577), make_strpos!(1, 7, 1, 13)),
            BinaryExpr::new_array(make_strpos!(1, 16, 1, 101), vec![
                BinaryExpr::new_tuple(make_strpos!(1, 17, 1, 37), vec![
                    BinaryExpr::new_ident("q".to_owned(), make_strpos!(1, 18, 1, 18)),
                    BinaryExpr::new_ident("AJBN0n".to_owned(), make_strpos!(1, 21, 1, 26)), 
                    BinaryExpr::new_ident("MDEgKh5".to_owned(), make_strpos!(1, 29, 1, 35))
                ]), 
                BinaryExpr::new_ident("KG".to_owned(), make_strpos!(1, 40, 1, 41)),
                BinaryExpr::new_tuple(make_strpos!(1, 44, 1, 75), vec![
                    BinaryExpr::new_ident("NsL".to_owned(), make_strpos!(1, 45, 1, 47)),
                    BinaryExpr::new_tuple(make_strpos!(1, 50, 1, 68), vec![
                        BinaryExpr::new_unit(make_strpos!(1, 51, 1, 52)),
                        BinaryExpr::new_ident("D".to_owned(), make_strpos!(1, 55, 1, 55)),
                        BinaryExpr::new_lit(LitValue::from(false), make_strpos!(1, 58, 1, 62)),
                        BinaryExpr::new_ident("d".to_owned(), make_strpos!(1, 65, 1, 65)),
                    ]),
                    BinaryExpr::new_lit(LitValue::from("H="), make_strpos!(1, 71, 1, 74))
                ]),
                BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 78, 1, 81)),
                BinaryExpr::new_paren(make_strpos!(1, 84, 1, 100), 
                    BinaryExpr::new_tuple(make_strpos!(1, 85, 1, 99), vec![
                        BinaryExpr::new_ident("vvB3".to_owned(), make_strpos!(1, 86, 1, 89)),
                        BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 92, 1, 95)),
                        BinaryExpr::new_lit(LitValue::from(5), make_strpos!(1, 98, 1, 98))
                    ])
                )
            ])
        ])
    }

    assert_eq!{ PrimaryExpr::with_test_str("(() )"), PrimaryExpr::new_paren(make_strpos!(1, 1, 1, 5), BinaryExpr::new_unit(make_strpos!(1, 2, 1, 3))) }
    assert_eq!{ PrimaryExpr::with_test_str("((),)"), PrimaryExpr::new_tuple(make_strpos!(1, 1, 1, 5), vec![BinaryExpr::new_unit(make_strpos!(1, 2, 1, 3))]) }

    //                                      1234567890123
    assert_eq!{ PrimaryExpr::with_test_str("[Fhi;vjIj0Dt]"), 
        PrimaryExpr::new_array_dup(make_strpos!(1, 1, 1, 13), 
            BinaryExpr::new_ident("Fhi".to_owned(), make_strpos!(1, 2, 1, 4)),
            BinaryExpr::new_ident("vjIj0Dt".to_owned(), make_strpos!(1, 6, 1, 12))
        )
    }

    assert_eq!{ PrimaryExpr::with_test_str("(\"o5\")"), 
        PrimaryExpr::new_paren(make_strpos!(1, 1, 1, 6), 
            BinaryExpr::new_lit(LitValue::from("o5"), make_strpos!(1, 2, 1, 5))
        )
    }

    //                                      0        1         2        
    //                                      1234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("(nn, ([false;true]), 183455)"),
        PrimaryExpr::new_tuple(make_strpos!(1, 1, 1, 28), vec![
            BinaryExpr::new_ident("nn".to_owned(), make_strpos!(1, 2, 1, 3)),
            BinaryExpr::new_paren(make_strpos!(1, 6, 1, 19), 
                BinaryExpr::new_array_dup(make_strpos!(1, 7, 1, 18), 
                    BinaryExpr::new_lit(LitValue::from(false), make_strpos!(1, 8, 1, 12)),
                    BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 14, 1, 17))
                )
            ),
            BinaryExpr::new_lit(LitValue::from(183455), make_strpos!(1, 22, 1, 27))
        ])
    }
    
    //                                      0        1         2         3         4         5         6         7       
    //                                      123456789012345678901234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PrimaryExpr::with_test_str("((true, (mO, [(q5k);a], (((KttG))), (K5DJ, r, ())), (McsaEdfdfalse,)), rIOKt,)"),
        PrimaryExpr::new_tuple(make_strpos!(1, 1, 1, 78), vec![
            BinaryExpr::new_tuple(make_strpos!(1, 2, 1, 69), vec![
                BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 3, 1, 6)),
                BinaryExpr::new_tuple(make_strpos!(1, 9, 1, 50), vec![
                    BinaryExpr::new_ident("mO".to_owned(), make_strpos!(1, 10, 1, 11)), 
                    BinaryExpr::new_array_dup(make_strpos!(1, 14, 1, 22), 
                        BinaryExpr::new_paren(make_strpos!(1, 15, 1, 19), 
                            BinaryExpr::new_ident("q5k".to_owned(), make_strpos!(1, 16, 1, 18))
                        ),
                        BinaryExpr::new_ident("a".to_owned(), make_strpos!(1, 21, 1, 21))
                    ),
                    BinaryExpr::new_paren(make_strpos!(1, 25, 1, 34), 
                        BinaryExpr::new_paren(make_strpos!(1, 26, 1, 33), 
                            BinaryExpr::new_paren(make_strpos!(1, 27, 1, 32), 
                                BinaryExpr::new_ident("KttG".to_owned(), make_strpos!(1, 28, 1, 31))
                            )
                        )
                    ),
                    BinaryExpr::new_tuple(make_strpos!(1, 37, 1, 49), vec![
                        BinaryExpr::new_ident("K5DJ".to_owned(), make_strpos!(1, 38, 1, 41)), 
                        BinaryExpr::new_ident("r".to_owned(), make_strpos!(1, 44, 1, 44)),
                        BinaryExpr::new_unit(make_strpos!(1, 47, 1, 48))
                    ]),
                ]),
                BinaryExpr::new_tuple(make_strpos!(1, 53, 1, 68), vec![
                    BinaryExpr::new_ident("McsaEdfdfalse".to_owned(), make_strpos!(1, 54, 1, 66))
                ])
            ]),
            BinaryExpr::new_ident("rIOKt".to_owned(), make_strpos!(1, 72, 1, 76))
        ])
    }

    //                                      0          1         2      
    //                                      12 345 67890123456789012
    assert_eq!{ PrimaryExpr::with_test_str("[\"il\", 0o52u32, sO04n]"),
        PrimaryExpr::new_array(make_strpos!(1, 1, 1, 22), vec![
            BinaryExpr::new_lit(LitValue::from("il"), make_strpos!(1, 2, 1, 5)),
            BinaryExpr::new_lit(LitValue::from(0o52u32), make_strpos!(1, 8, 1, 14)), 
            BinaryExpr::new_ident("sO04n".to_owned(), make_strpos!(1, 17, 1, 21))
        ])
    }
    //                                      12345678
    assert_eq!{ PrimaryExpr::with_test_str("['f';()]"), 
        PrimaryExpr::new_array_dup(make_strpos!(1, 1, 1, 8), 
            BinaryExpr::new_lit(LitValue::from('f'), make_strpos!(1, 2, 1, 4)),
            BinaryExpr::new_unit(make_strpos!(1, 6, 1, 7))
        )
    }
    assert_eq!{ PrimaryExpr::with_test_str("[]"), PrimaryExpr::new_array(make_strpos!(1, 1, 1, 2), vec![]) }

    //                                      0        1           2         3         4      
    //                                      12345 678901 234567890123456789012345678901234
    assert_eq!{ PrimaryExpr::with_test_str("[8, \"@=?GF\", 87f32, 1340323.74f64, FKOxAvx5]"),
        PrimaryExpr::new_array(make_strpos!(1, 1, 1, 44), vec![
            BinaryExpr::new_lit(LitValue::from(8), make_strpos!(1, 2, 1, 2)),
            BinaryExpr::new_lit(LitValue::from("@=?GF"), make_strpos!(1, 5, 1, 11)), 
            BinaryExpr::new_lit(LitValue::from(87f32), make_strpos!(1, 14, 1, 18)),
            BinaryExpr::new_lit(LitValue::from(1340323.74f64), make_strpos!(1, 21, 1, 33)),
            BinaryExpr::new_ident("FKOxAvx5".to_owned(), make_strpos!(1, 36, 1, 43))
        ])
    }

    //                                        0        1         2         3         4         5         6     
    //                                        123456789012345678901234567890123456789012345678901234567890123
    assert_eq!{ PrimaryExpr::with_test_str(r#"  [[dnr4, lGFd3yL, tJ], ['\\', p, (xGaBwiL,), DE], true, aB8aE]"#),
        PrimaryExpr::new_array(make_strpos!(1, 3, 1, 63), vec![
            BinaryExpr::new_array(make_strpos!(1, 4, 1, 22), vec![
                BinaryExpr::new_ident("dnr4".to_owned(), make_strpos!(1, 5, 1, 8)),
                BinaryExpr::new_ident("lGFd3yL".to_owned(), make_strpos!(1, 11, 1, 17)),
                BinaryExpr::new_ident("tJ".to_owned(), make_strpos!(1, 20, 1, 21))
            ]),
            BinaryExpr::new_array(make_strpos!(1, 25, 1, 49), vec![
                BinaryExpr::new_lit(LitValue::from('\\'), make_strpos!(1, 26, 1, 29)), 
                BinaryExpr::new_ident("p".to_owned(), make_strpos!(1, 32, 1, 32)),
                BinaryExpr::new_tuple(make_strpos!(1, 35, 1, 44), vec![
                    BinaryExpr::new_ident("xGaBwiL".to_owned(), make_strpos!(1, 36, 1, 42))
                ]),
                BinaryExpr::new_ident("DE".to_owned(), make_strpos!(1, 47, 1, 48))
            ]),
            BinaryExpr::new_lit(LitValue::from(true), make_strpos!(1, 52, 1, 55)),
            BinaryExpr::new_ident("aB8aE".to_owned(), make_strpos!(1, 58, 1, 62))
        ])
    } 

    // Previous manual tests
    //                                      0        1           2          3         4         5           6
    //                                      12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
    assert_eq!{ PrimaryExpr::with_test_str("[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
        PrimaryExpr::new_array(make_strpos!(1, 1, 1, 66), vec![
            BinaryExpr::new_ident("abc".to_owned(), make_strpos!(1, 2, 1, 4)),
            BinaryExpr::new_lit(LitValue::from(123u32), make_strpos!(1, 7, 1, 12)),
            BinaryExpr::new_lit(LitValue::from("456"), make_strpos!(1, 15, 1, 19)),
            BinaryExpr::new_lit(LitValue::from('\u{0065}'), make_strpos!(1, 22, 1, 29)),
            BinaryExpr::new_lit(LitValue::from(false), make_strpos!(1, 32, 1, 36)),
            BinaryExpr::new_unit(make_strpos!(1, 39, 1, 40)),
            BinaryExpr::new_paren(make_strpos!(1, 43, 1, 45), 
                BinaryExpr::new_ident("a".to_owned(), make_strpos!(1, 44, 1, 44))
            ),
            BinaryExpr::new_tuple(make_strpos!(1, 48, 1, 63), vec![
                BinaryExpr::new_ident("abc".to_owned(), make_strpos!(1, 49, 1, 51)),
                BinaryExpr::new_lit(LitValue::from("hello"), make_strpos!(1, 54, 1, 60)),
            ])
        ])
    }       

    assert_eq!{ PrimaryExpr::with_test_str("(                             )"), PrimaryExpr::new_unit(make_strpos!(1, 1, 1, 31)) }

    //                                      0        1         2
    //                                      123456789012345678901
    assert_eq!{ PrimaryExpr::with_test_str("[[123u32, abc]; 4567]"), 
        PrimaryExpr::new_array_dup(make_strpos!(1, 1, 1, 21), 
            BinaryExpr::new_array(make_strpos!(1, 2, 1, 14), vec![
                BinaryExpr::new_lit(LitValue::from(123u32), make_strpos!(1, 3, 1, 8)), 
                BinaryExpr::new_ident("abc".to_owned(), make_strpos!(1, 11, 1, 13))
            ]),
            BinaryExpr::new_lit(LitValue::from(4567), make_strpos!(1, 17, 1, 20))
        )
    }
}