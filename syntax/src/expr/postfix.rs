
// PostfixExpr = 
//     PrimaryExpr [
//             fLeftBracket [Expression [fComma Expression]*] fRightBracket
//             | fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier fLeftParen [Expression [fComma Expression]*] fRightParen
//             | fDot fIdentifier
//             | fAs TypeUse
//         ]*

use std::fmt;

use codepos::StringPosition;
use message::Message;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::KeywordKind;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::binary::BinaryExpr;
use super::super::TypeUse;

use super::primary::PrimaryExpr;

#[derive(Eq, PartialEq)]
enum ActualPostfix {
    Subscription(Vec<BinaryExpr>, StringPosition),      // []'s position
    FunctionCall(Vec<BinaryExpr>, StringPosition),      // ()'s position
    MemberFunctionCall(String, Vec<BinaryExpr>, StringPosition, StringPosition), // .xxx()'s position
    MemberAccess(String, StringPosition),               // .xxx's position
    TypeCast(TypeUse, StringPosition),                  // as position
}
#[derive(Eq, PartialEq)]
enum PostfixExprImpl {
    Primary(PrimaryExpr),
    Postfix(PostfixExpr, ActualPostfix, StringPosition), // all_strpos
}
#[derive(Eq, PartialEq)]
pub struct PostfixExpr(Box<PostfixExprImpl>);

impl ISyntaxItemFormat for PostfixExpr {
    fn format(&self, indent: u32) -> String {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(ref primary_expr) => primary_expr.format(indent),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::Subscription(ref exprs, ref bracket_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}Subscription\n{}Bracket at <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), 
                    PostfixExpr::indent_str(indent + 2), bracket_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::FunctionCall(ref exprs, ref paren_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}FunctionCall\n{}Paren at <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), 
                    PostfixExpr::indent_str(indent + 2), paren_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::MemberFunctionCall(ref name, ref exprs, ref ident_strpos, ref paren_strpos), ref all_strpos) =>
                format!("{}PostfixExpr <{:?}>\n{}\n{}MemberFunctionCall\n{}{} <{:?}>, Paren at <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), 
                    PostfixExpr::indent_str(indent + 2), name, ident_strpos, paren_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })), 
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::MemberAccess(ref name, ref name_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}AccessMember: {} <{:?}>",
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1), PostfixExpr::indent_str(indent + 1), name, name_strpos),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::TypeCast(ref typeuse, ref as_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}CastAs\n{}As at <{:?}>\n{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), 
                    PostfixExpr::indent_str(indent + 2), as_strpos, 
                    typeuse.format(indent + 2)),
        }
    }
}
impl fmt::Debug for PostfixExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl PostfixExpr { // New

    pub fn new_primary(primary_expr: PrimaryExpr) -> PostfixExpr {
        PostfixExpr(Box::new(PostfixExprImpl::Primary(primary_expr)))
    }

    pub fn new_subscription(postfix_expr: PostfixExpr, bracket_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), bracket_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::Subscription(exprs, bracket_strpos), all_strpos)))
    }
    pub fn new_function_call(postfix_expr: PostfixExpr, paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), paren_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::FunctionCall(exprs, paren_strpos), all_strpos)))
    }
    pub fn new_member_function_call(postfix_expr: PostfixExpr, name: String, ident_strpos: StringPosition, paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), paren_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::MemberFunctionCall(name, exprs, ident_strpos, paren_strpos), all_strpos)))
    }
    pub fn new_member_access(postfix_expr: PostfixExpr, name: String, name_strpos: StringPosition) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), name_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::MemberAccess(name, name_strpos), all_strpos)))
    }
    pub fn new_type_cast(postfix_expr: PostfixExpr, typeuse: TypeUse, as_strpos: StringPosition) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), as_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, ActualPostfix::TypeCast(typeuse, as_strpos), all_strpos)))
    }
}
impl PostfixExpr { // Get

    pub fn is_primary(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(_) => true,
            &PostfixExprImpl::Postfix(_, _, _) => false,
        }
    }
    pub fn is_postfix(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(_) => false,
            &PostfixExprImpl::Postfix(_, _, _) => true,
        }
    }

    pub fn get_primary(&self) -> Option<&PrimaryExpr> {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(ref primary_expr) => Some(primary_expr),
            &PostfixExprImpl::Postfix(_, _, _) => None,
        }
    }
    pub fn get_postfix(&self) -> Option<&PostfixExpr> {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(_) => None,
            &PostfixExprImpl::Postfix(ref postfix_expr, ref _2, ref _3) => Some(postfix_expr),
        }
    }
    pub fn get_all_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &PostfixExprImpl::Primary(ref primary_expr) => primary_expr.pos_all(),
            &PostfixExprImpl::Postfix(ref _1, ref _2, ref all_strpos) => *all_strpos,
        }
    }

    pub fn is_subscription(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::Subscription(_, _), ref _2) => true,
            _ => false,
        }
    }
    pub fn is_function_call(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::FunctionCall(_, _), ref _2) => true,
            _ => false,
        }
    }
    pub fn is_member_function_call(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::MemberFunctionCall(_, _, _, _), ref _2) => true,
            _ => false,
        }
    }
    pub fn is_member_access(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::MemberAccess(_, _), ref _2) => true,
            _ => false,
        }
    }
    pub fn is_type_cast(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::TypeCast(_, _), ref _2) => true,
            _ => false,
        }
    }

    pub fn get_exprs(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::Subscription(ref exprs, _), _)
            | &PostfixExprImpl::Postfix(_, ActualPostfix::FunctionCall(ref exprs, _), _) => Some(exprs),
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, ref exprs, _, _), _) => Some(exprs),
            _ => None,
        }
    }
    pub fn get_delimeter_strpos(&self) -> StringPosition { // [] or ()'s position
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::Subscription(_, ref delimeter_strpos), _)
            | &PostfixExprImpl::Postfix(_, ActualPostfix::FunctionCall(_, ref delimeter_strpos), _) => *delimeter_strpos,
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, _, _, ref delimeter_strpos), _) => *delimeter_strpos,
            _ => StringPosition::new(),
        }
    }
    pub fn get_name(&self) -> Option<&String> {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberAccess(ref name, _), _) => Some(name),
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(ref name, _, _, _), _) => Some(name),
            _ => None,            
        }
    }
    pub fn get_name_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberAccess(_, ref name_strpos), _) => *name_strpos,
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, _, ref name_strpos, _), _) => *name_strpos,
            _ => StringPosition::new(),
        }
    }
    pub fn get_typeuse(&self) -> Option<&TypeUse> {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::TypeCast(ref typeuse, _), _) => Some(typeuse),
            _ => None,
        }
    }
    pub fn get_as_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::TypeCast(_, ref as_strpos), _) => *as_strpos,
            _ => StringPosition::new(),
        }
    }
}
impl ISyntaxItem for PostfixExpr {
    
    fn pos_all(&self) -> StringPosition { 
        self.get_all_strpos()
    }
    fn is_first_final(tokens: &mut TokenStream, index: usize) -> bool {
        PrimaryExpr::is_first_final(tokens, index)
    }
    
    #[allow(unused_assignments)]
    fn parse(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<PostfixExpr>, usize) {
        
        #[cfg(feature = "trace_postfix_expr_parse")]
        macro_rules! trace { ($($arg:tt)*) => ({ print!("[PostfixExpr:{}] ", line!()); println!($($arg)*); }) }
        #[cfg(not(feature = "trace_postfix_expr_parse"))]
        macro_rules! trace { ($($arg:tt)*) => () }

        let (mut current_retval, mut current_length) = match PrimaryExpr::parse(tokens, messages, index) {
            (Some(primary_expr), primary_length) => (PostfixExpr::new_primary(primary_expr), primary_length),
            (None, none_length) => return (None, none_length), // no recover
        };
        trace!("parsed primary, current is {:?}", current_retval);

        'postfix: loop {
            // function call and subscription both accept expression list, merge the processor and it is the end seperator
            let mut expect_end_sep = SeperatorKind::Add;
            if tokens.nth(index + current_length).is_seperator(SeperatorKind::Dot) {
                match tokens.nth(index + current_length + 1).get_identifier() {
                    Some(ident) => {
                        trace!{ "get one postfix, member access {:?}", ident, }
                        current_retval = PostfixExpr::new_member_access(
                            current_retval, ident.clone(), StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1))
                        );
                        current_length += 2;
                        continue 'postfix;
                    }
                    None => {
                        trace!("get postfix failed, member access not followed ident");
                        return push_unexpect!(tokens, messages, "member identifier", index + current_length + 1, current_length + 1);
                    }
                }
            } else if tokens.nth(index + current_length).is_keyword(KeywordKind::As) {
                match TypeUse::parse(tokens, messages, index + current_length + 1) {
                    (Some(ty), ty_len) => {
                        trace!("get one postfix, type cast as {:?}", ty);
                        current_retval = PostfixExpr::new_type_cast(
                            current_retval, ty, tokens.pos(index + current_length)
                        );
                        current_length += ty_len + 1;
                        continue 'postfix;
                    }
                    (None, length) => {
                        trace!{ "get postfix failed, type cast not followed typedef" }
                        return (None, current_length + 1 + length);  
                    } 
                }
            } else if tokens.nth(index + current_length).is_seperator(SeperatorKind::LeftParenthenes) {
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    trace!("get one postfix, none parameter function call");
                    current_retval = PostfixExpr::new_function_call(
                        current_retval, StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1)), Vec::new()
                    );
                    current_length += 2;
                    continue 'postfix; // no param function call
                }
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_length + 2).is_seperator(SeperatorKind::RightParenthenes) {
                        let paren_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 2));
                        // let pos2 = tokens.pos(index + current_length + 1).start_pos();  // comma pos is not used now
                        messages.push(Message::new_by_str("Single comma in function call", vec![(paren_strpos, "Function call here")]));
                        current_retval = PostfixExpr::new_function_call(current_retval, paren_strpos, Vec::new());
                        current_length += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if tokens.nth(index + current_length).is_seperator(SeperatorKind::LeftBracket) {
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::RightBracket) {
                    let pos = tokens.pos(index + current_length);
                    messages.push(Message::new_by_str("Empty subscription", vec![(pos, "subscription here")]));
                    current_retval = PostfixExpr::new_subscription(
                        current_retval, StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1)), Vec::new()
                    );
                    current_length += 2;
                    continue 'postfix;
                }
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_length + 2).is_seperator(SeperatorKind::RightBracket) {
                        let bracket_pair_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 2));
                        // let pos2 = tokens.pos(index + current_length + 1).start_pos(); // comma pos is not used now
                        messages.push(Message::new_by_str("Empty subscription", vec![(bracket_pair_strpos, "Subscription here")]));
                        current_retval = PostfixExpr::new_subscription(
                            current_retval, bracket_pair_strpos, Vec::new()
                        );
                        current_length += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightBracket;
            } else {
                break; // other final token, break
            }

            // Get the expression list
            trace!{ "parsing postfix, start processing expression list of {}", 
                if expect_end_sep == SeperatorKind::RightParenthenes { "function call".to_owned() } else { "subscription".to_owned() }, }
            current_length += 1; 
            match BinaryExpr::parse(tokens, messages, index + current_length) {
                (None, length) => { 
                    trace!{ "parsing postfix's expression list, expression parse failed" }
                    return (None, current_length + length);
                }
                (Some(expr1), expr1_len) => {
                    let mut exprs_len = expr1_len;
                    let mut exprs = vec![expr1];
                    'expr: loop { 
                        match expect_end_sep {
                            SeperatorKind::RightParenthenes 
                                if tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::RightParenthenes) 
                                    || (tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && tokens.nth(index + current_length + exprs_len + 1).is_seperator(SeperatorKind::RightParenthenes))  => {
                                trace!{ "parsing postfix function call's expression list finished" }
                                let pos_left_paren = tokens.pos(index + current_length - 1);
                                current_length += exprs_len + if tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_paren = tokens.pos(index + current_length - 1);
                                current_retval = PostfixExpr::new_function_call(
                                    current_retval, StringPosition::merge(pos_left_paren, pos_right_paren), exprs
                                );
                                continue 'postfix;
                            }
                            SeperatorKind::RightBracket 
                                if tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::RightBracket)
                                    || (tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::Comma)
                                        && tokens.nth(index + current_length + exprs_len + 1).is_seperator(SeperatorKind::RightBracket)) => {
                                trace!{ "parsing postfix subscription's expression list finished" }
                                let pos_left_bracket = tokens.pos(index + current_length - 1);
                                current_length += exprs_len + if tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::Comma) { 2 } else { 1 };
                                let pos_right_bracket = tokens.pos(index + current_length - 1);
                                current_retval = PostfixExpr::new_subscription(
                                    current_retval, StringPosition::merge(pos_left_bracket, pos_right_bracket), exprs
                                );
                                continue 'postfix;
                            }
                            SeperatorKind::RightParenthenes | SeperatorKind::RightBracket => (),
                            _ => unreachable!()
                        }
                        if tokens.nth(index + current_length + exprs_len).is_seperator(SeperatorKind::Comma) {
                            exprs_len += 1;
                            match BinaryExpr::parse(tokens, messages, index + current_length + exprs_len) {
                                (Some(expr), expr_len) => {
                                    trace!("parsing postfix's expression list, get expression: {:?}", expr);
                                    exprs_len += expr_len;
                                    exprs.push(expr);
                                }
                                (None, length) => {
                                    trace!{ "parsing postfix's expression list failed, get none expression" }
                                    return (None, current_length + exprs_len + length);
                                }
                            }
                        }
                    }
                }
            }
        }

        trace!("parsing postfix finished, get retval: {:?}", current_retval);
        (Some(current_retval), current_length)
    }
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use lexical::LitValue;
    use super::super::ISyntaxItemWithStr;

    //                                      0        1         2         3         4         5     
    //                                      12345678901234567890123456789012345678901234567890123
    assert_eq!{ PostfixExpr::with_test_str("1.a[[3](4, [5, 6,], )](7, 8)() as [i32].b[10, 11, 12]"),
        PostfixExpr::new_subscription(
            PostfixExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))),
            make_str_pos!(1, 42, 1, 53),
            vec![
                BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(10), make_str_pos!(1, 43, 1, 44))),
                BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(11), make_str_pos!(1, 47, 1, 48))),
                BinaryExpr::new_primary(PrimaryExpr::new_lit(LitValue::from(12), make_str_pos!(1, 51, 1, 52))),
            ]
        )
    }

    // "[FihB1w8, true, (), 75] as u64" 
    // "L()" 
    // "f.heA4fp" 
    // "iryl8L(hOvKj01K(xLvnM604, ()[(MK1), lCN0pK, [wpwh, "e\rbx", 0o5249485353525454], 220], a2iDC(mC8fhmr(tq6, Evaj5Hap, jwDO5G2E.GB7lt7 as eFaF, 64112, (false)[Cuov4, (c, [vr2sAs;false], q), Ch]), D as (), OyM6jv).O, qgh4j([([[L6E, (BgGyKg), (gg221GyNK,)], (), (true)]), 0d336, true, (eGgyN), 'm'] as [[f64]], a(), mJ.pNHwe34[tg2 as i8, n as (u64, Bs, (brft, [LEG], [gao])), esr(), 7806533], ot() as klE, 3.0853 as i8).yL3s5K2)EHMDCmO,)" 
    // "(true, oxA4r4g, 0o495451524853545453i16, 5, true) as KbM0" 
    // "hio3yaH(tIBONx, 842154.uuvsBB1r[jvLh('\', kDxLG0H, "QB".Iww0tJia, () as [f0rdrt][[1875, [jsIvKGhD023i,], 0b0, (), (Bm)][], D6ta1wB[Mlcdq0Dnhj.c,], D6s2lIE3(true, BNm4CvL8.p4kHd, Ap5Iq()) as m3qcm7, 154.766204 as [repp03].xDArJE]), y4ibtGo, O48J.II6Juwq as [i8]] as r, BI7hxse)" 
    // "pbfN6 as ((([[[DBm]]], [a0kjv;0d6i32], NxvlxA), n0ANq, J8car8cE, string, i16)[bool],)" 
    // "L2y(trueGh4p(false as ().Gpj, 3825727.l1Jm3B[true[0b10111110, true, (0b000110010001u6,), jDw], DAeBm, Dw1tylyj.vn, true.j], M),)" 
    // "g[(), (CttKay4f), true, wE, 0b11110110111] as ([Bx1A7F], [(tbje, bool, [mb])], (char, (u64, bool, A, string), (ttrof32,)), gjMaxI4f, [tJEp3])" 
    // "[false, (((wg), thLaI, IggO, "t")), ()] as u8" 
    // "f(h.w, [0xc2u32, [jcGs0f30, (cN5qg), "13{\r-xw"], (true), ("H8eHs[9", (), v, lumL)].m.GMjriIc, c023qs[q, u34, pEkuEB, false], [false, 0d866, 362388u16, (43E1)].AsI).Ksmfhi" 

    // let right = expr_to_postfix!{
    //     PrimaryExpr::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
    //     Postfix::MemberAccess("defg".to_owned(), make_str_pos!(1, 4, 1, 8))
    //     Postfix::Subscription(vec![
    //         expr_to_postfix!(
    //             PrimaryExpr::ArrayDef(
    //                 vec![expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 11, 1, 11))],
    //                 make_str_pos!(1, 10, 1, 12)
    //             ), 
    //             Postfix::FunctionCall(vec![
    //                 expr_ident!("klm", make_str_pos!(1, 14, 1, 16)),
    //                 expr_array_def!([
    //                     expr_num_lit!(NumLitValue::I32(123), make_str_pos!(1, 20, 1, 22)),
    //                     expr_num_lit!(NumLitValue::I32(456), make_str_pos!(1, 25, 1, 27)), ]
    //                     make_str_pos!(1, 19, 1, 29)
    //                 )], 
    //                 make_str_pos!(1, 13, 1, 32)
    //             )
    //         )], 
    //         make_str_pos!(1, 9, 1, 33)
    //     )
    //     Postfix::FunctionCall(vec![
    //         expr_ident!("opq", make_str_pos!(1, 35, 1, 37)),
    //         expr_num_lit!(NumLitValue::F64(456f64), make_str_pos!(1, 40, 1, 43))],
    //         make_str_pos!(1, 34, 1, 44) 
    //     )
    //     Postfix::FunctionCall(
    //         Vec::new(),
    //         make_str_pos!(1, 45, 1, 46)
    //     )
    //     Postfix::TypeCast(
    //         TypeUse::Array(Box::new(
    //             TypeUse::Base("i32".to_owned(), make_str_pos!(1, 52, 1, 54))
    //         ), make_str_pos!(1, 51, 1, 55)),
    //         make_str_pos!(1, 48, 1, 49)
    //     )
    //     Postfix::MemberAccess("rst".to_owned(), make_str_pos!(1, 56, 1, 59))
    //     Postfix::Subscription(vec![
    //         expr_ident!("uvw", make_str_pos!(1, 61, 1, 63)),
    //         expr_ident!("xyz", make_str_pos!(1, 66, 1, 68)),
    //         expr_ident!("ABC", make_str_pos!(1, 71, 1, 73)),],
    //         make_str_pos!(1, 60, 1, 74),
    //     )
    // };

    // assert_eq!(left, right);

    // perrorln!("{:?}", BinaryExpr::with_test_str("writeln(\"helloworld\")"));
}