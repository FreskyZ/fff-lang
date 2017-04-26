
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
use super::super::BinaryExpr;
use super::super::TypeUse;

use super::primary::PrimaryExpr;

#[derive(Eq, PartialEq)]
enum ActualPostfix {
    MemberAccess(String, [StringPosition; 2]),          // dot's position, xxx's position
    Subscription(Vec<BinaryExpr>, StringPosition),      // []'s position
    FunctionCall(Vec<BinaryExpr>, StringPosition),      // ()'s position
    MemberFunctionCall(String, Vec<BinaryExpr>, [StringPosition; 3]), // dot's position, xxx's position ()'s position
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
                format!("{}PostfixExpr <{:?}>\n{}\n{}Subscription <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), bracket_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, ActualPostfix::FunctionCall(ref exprs, ref paren_strpos), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}FunctionCall <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), paren_strpos, 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })),
            &PostfixExprImpl::Postfix(ref postfix_expr, 
                ActualPostfix::MemberFunctionCall(ref name, ref exprs, ref strposs), ref all_strpos) =>
                format!("{}PostfixExpr <{:?}>\n{}\n{}MemberFunctionCall <{:?}>\n{}'.' <{:?}>\n{}Ident '{}' <{:?}>\n{}paren <{:?}>{}", 
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1),
                    PostfixExpr::indent_str(indent + 1), StringPosition::merge(strposs[0], strposs[2]),
                    PostfixExpr::indent_str(indent + 2), strposs[0],
                    PostfixExpr::indent_str(indent + 2), name, strposs[1],
                    PostfixExpr::indent_str(indent + 2), strposs[2], 
                    exprs.iter().fold(String::new(), |mut buf, expr| { buf.push_str("\n"); buf.push_str(&expr.format(indent + 2)); buf })), 
            &PostfixExprImpl::Postfix(ref postfix_expr, 
                ActualPostfix::MemberAccess(ref name, ref strposs), ref all_strpos) => 
                format!("{}PostfixExpr <{:?}>\n{}\n{}MemberAccess <{:?}>\n{}'.' <{:?}>\n{}Ident '{}' <{:?}>",
                    PostfixExpr::indent_str(indent), all_strpos,
                    postfix_expr.format(indent + 1), 
                    PostfixExpr::indent_str(indent + 1), StringPosition::merge(strposs[0], strposs[1]),
                    PostfixExpr::indent_str(indent + 2), strposs[0],
                    PostfixExpr::indent_str(indent + 2), name, strposs[1]),
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
    pub fn new_member_function_call(postfix_expr: PostfixExpr, 
        dot_strpos: StringPosition, name: String, ident_strpos: StringPosition, paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), paren_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, 
            ActualPostfix::MemberFunctionCall(name, exprs, [dot_strpos, ident_strpos, paren_strpos]), all_strpos)))
    }
    pub fn new_member_access(postfix_expr: PostfixExpr, dot_strpos: StringPosition, name: String, name_strpos: StringPosition) -> PostfixExpr {
        let all_strpos = StringPosition::merge(postfix_expr.pos_all(), name_strpos);
        PostfixExpr(Box::new(PostfixExprImpl::Postfix(postfix_expr, 
            ActualPostfix::MemberAccess(name, [dot_strpos, name_strpos]), all_strpos)))
    }

    // parse helper, don't want to handle this there
    fn new_function_call_auto_merge_prev_member_access(postfix_expr: PostfixExpr, paren_strpos: StringPosition, exprs: Vec<BinaryExpr>) -> PostfixExpr {
        use std::mem; // strange method to move boxed out from immutable ref self
        unsafe { 
            match *Box::into_raw(postfix_expr.0) {
                PostfixExprImpl::Postfix(ref mut prev_prev_postfix_expr, ActualPostfix::MemberAccess(ref name, ref dot_and_name_strpos), _) => {
                    let mut owned_prev_prev_postfix_expr = mem::uninitialized();
                    mem::swap(prev_prev_postfix_expr, &mut owned_prev_prev_postfix_expr);
                    return PostfixExpr::new_member_function_call(owned_prev_prev_postfix_expr, dot_and_name_strpos[0], name.clone(), dot_and_name_strpos[1], paren_strpos, exprs);
                }
                ref mut other_impl => return PostfixExpr::new_function_call(PostfixExpr(Box::from_raw(other_impl as *mut PostfixExprImpl)), paren_strpos, exprs),
            }
        }
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
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::MemberFunctionCall(_, _, _), ref _2) => true,
            _ => false,
        }
    }
    pub fn is_member_access(&self) -> bool {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(ref _1, ActualPostfix::MemberAccess(_, _), ref _2) => true,
            _ => false,
        }
    }

    pub fn get_exprs(&self) -> Option<&Vec<BinaryExpr>> {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::Subscription(ref exprs, _), _)
            | &PostfixExprImpl::Postfix(_, ActualPostfix::FunctionCall(ref exprs, _), _) => Some(exprs),
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, ref exprs, _), _) => Some(exprs),
            _ => None,
        }
    }
    pub fn get_delimeter_strpos(&self) -> StringPosition { // [] or ()'s position
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::Subscription(_, ref delimeter_strpos), _)
            | &PostfixExprImpl::Postfix(_, ActualPostfix::FunctionCall(_, ref delimeter_strpos), _) => *delimeter_strpos,
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, _, ref strposs), _) => strposs[2],
            _ => StringPosition::new(),
        }
    }
    pub fn get_name(&self) -> Option<&String> {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberAccess(ref name, _), _) => Some(name),
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(ref name, _, _), _) => Some(name),
            _ => None,            
        }
    }
    pub fn get_name_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberAccess(_, ref strposs), _) => strposs[1],
            &PostfixExprImpl::Postfix(_, ActualPostfix::MemberFunctionCall(_, _, ref strposs), _) => strposs[1],
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
        macro_rules! trace { ($($arg:tt)*) => ({ perror!("    [PostfixExpr:{}] ", line!()); perrorln!($($arg)*); }) }
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
                let dot_strpos = tokens.pos(index + current_length);
                let maybe_ident_strpos = tokens.pos(index + current_length + 1);
                match tokens.nth(index + current_length + 1).get_identifier() {
                    Some(ident) => {
                        current_retval = PostfixExpr::new_member_access(
                            current_retval, dot_strpos, ident.clone(), maybe_ident_strpos,
                        );
                        trace!("get one postfix, member access {:?} at {:?} and {:?}, current retval is {:?}", 
                            ident, dot_strpos, maybe_ident_strpos, current_retval);
                        current_length += 2;
                        trace!("after member access finished, current token: {:?}", tokens.nth(index + current_length));
                        continue 'postfix;
                    }
                    None => {
                        trace!("get postfix failed, member access not followed ident");
                        return push_unexpect!(tokens, messages, "identifier", index + current_length + 1, current_length + 1);
                    }
                }
            } else if tokens.nth(index + current_length).is_seperator(SeperatorKind::LeftParenthenes) {
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::RightParenthenes) {
                    trace!("get one postfix, none parameter function call");
                    current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(
                        current_retval, StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1)), Vec::new()
                    );
                    current_length += 2;
                    continue 'postfix; // no param function call
                }
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_length + 2).is_seperator(SeperatorKind::RightParenthenes) {
                        let paren_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 2));
                        messages.push(Message::new_by_str("Single comma in function call", vec![(paren_strpos, "function call here")]));
                        current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(current_retval, paren_strpos, Vec::new());
                        current_length += 3;
                        continue 'postfix;
                }
                expect_end_sep = SeperatorKind::RightParenthenes;
            } else if tokens.nth(index + current_length).is_seperator(SeperatorKind::LeftBracket) {
                trace!("meet left bracket '{:?}', assume subscription", tokens.nth(index + current_length));
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::RightBracket) {
                    let bracket_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1));
                    messages.push(Message::new_by_str("Empty subscription", vec![(bracket_strpos, "subscription here")]));
                    current_retval = PostfixExpr::new_subscription(
                        current_retval, StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 1)), Vec::new()
                    );
                    current_length += 2;
                    continue 'postfix;
                }
                if tokens.nth(index + current_length + 1).is_seperator(SeperatorKind::Comma) 
                    && tokens.nth(index + current_length + 2).is_seperator(SeperatorKind::RightBracket) {
                        let bracket_pair_strpos = StringPosition::merge(tokens.pos(index + current_length), tokens.pos(index + current_length + 2));
                        messages.push(Message::new_by_str("Empty subscription", vec![(bracket_pair_strpos, "subscription here")]));
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
            current_length += 1; 
            trace!("parsing postfix, start processing expression list of {}, current token is {:?}", 
                if expect_end_sep == SeperatorKind::RightParenthenes { "function call".to_owned() } else { "subscription".to_owned() }, 
                tokens.nth(index + current_length));
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
                                current_retval = PostfixExpr::new_function_call_auto_merge_prev_member_access(
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
fn postfix_expr_format() {
    use super::super::ISyntaxItemWithStr;

    macro_rules! test_case {
        ($left: expr, $right: expr) => {
            if $left != $right {
                let left_owned = $left.to_owned();
                let left_lines = left_owned.lines();
                let right_lines = $right.lines();
                for (index, (left_line, right_line)) in left_lines.zip(right_lines).enumerate() {
                    if left_line != right_line {
                        panic!("assertion failed at index {}\nleft: {}\nright: {}", index, $left, $right);
                    }
                }
                panic!("assertion failed, but cannot detected by compare each line\nleft: {}\nright: {}", $left, $right);
            }
        }
    }

    // Attention that this source code line's LF is also the string literal (test oracle)'s LF
    //                                     0        1         2         3         4         5        
    //                                     1234567890123456789012345678901234567890123456789012345678
    test_case!(PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]").format(0), r#"PostfixExpr <<0>1:1-1:58>
  PostfixExpr <<0>1:1-1:48>
    PostfixExpr <<0>1:1-1:39>
      PostfixExpr <<0>1:1-1:37>
        PostfixExpr <<0>1:1-1:33>
          PostfixExpr <<0>1:1-1:30>
            PostfixExpr <<0>1:1-1:28>
              PostfixExpr <<0>1:1-1:24>
                PostfixExpr <<0>1:1-1:12>
                  Ident 'a' <<0>1:1-1:1>
                  MemberFunctionCall <<0>1:2-1:12>
                    '.' <<0>1:2-1:2>
                    Ident 'b' <<0>1:3-1:3>
                    paren <<0>1:4-1:12>
                    Ident 'c' <<0>1:5-1:5>
                    Ident 'd' <<0>1:8-1:8>
                    Ident 'e' <<0>1:11-1:11>
                MemberFunctionCall <<0>1:13-1:24>
                  '.' <<0>1:13-1:13>
                  Ident 'f' <<0>1:14-1:14>
                  paren <<0>1:15-1:24>
                  Ident 'g' <<0>1:16-1:16>
                  Ident 'h' <<0>1:19-1:19>
                  Ident 'i' <<0>1:22-1:22>
              FunctionCall <<0>1:25-1:28>
                Ident 'u' <<0>1:26-1:26>
            MemberAccess <<0>1:29-1:30>
              '.' <<0>1:29-1:29>
              Ident 'j' <<0>1:30-1:30>
          Subscription <<0>1:31-1:33>
            Ident 'k' <<0>1:32-1:32>
        MemberFunctionCall <<0>1:34-1:37>
          '.' <<0>1:34-1:34>
          Ident 'l' <<0>1:35-1:35>
          paren <<0>1:36-1:37>
      MemberAccess <<0>1:38-1:39>
        '.' <<0>1:38-1:38>
        Ident 'm' <<0>1:39-1:39>
    Subscription <<0>1:40-1:48>
      Ident 'n' <<0>1:41-1:41>
      Ident 'o' <<0>1:44-1:44>
      Ident 'p' <<0>1:47-1:47>
  Subscription <<0>1:49-1:58>
    Ident 'r' <<0>1:50-1:50>
    Ident 's' <<0>1:53-1:53>
    Ident 't' <<0>1:56-1:56>"#
    );
}

#[cfg(test)] #[test]
fn postfix_expr_parse() {
    use lexical::LitValue;
    use super::super::TypeUseF;
    use super::super::ISyntaxItemWithStr;

    macro_rules! ident {
        ($name: expr, $strpos: expr) => (BinaryExpr::new_primary(PrimaryExpr::new_ident($name.to_owned(), $strpos)));
        (post, $name: expr, $strpos: expr) => (PostfixExpr::new_primary(PrimaryExpr::new_ident($name.to_owned(), $strpos)))
    }

    //                                      0        1         2         3         4         5     
    // plain                                1234567890123456789012345678901234567890123456789012345678
    assert_eq!{ PostfixExpr::with_test_str("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]"),
        PostfixExpr::new_subscription(
            PostfixExpr::new_subscription(
                PostfixExpr::new_member_access(
                    PostfixExpr::new_member_function_call(
                        PostfixExpr::new_subscription(
                            PostfixExpr::new_member_access(
                                PostfixExpr::new_function_call(
                                    PostfixExpr::new_member_function_call(
                                        PostfixExpr::new_member_function_call(
                                            ident!(post, "a", make_strpos!(1, 1, 1, 1)),
                                            make_strpos!(1, 2, 1, 2),
                                            "b".to_owned(), make_strpos!(1, 3, 1, 3),
                                            make_strpos!(1, 4, 1, 12), vec![
                                                ident!("c", make_strpos!(1, 5, 1, 5)),
                                                ident!("d", make_strpos!(1, 8, 1, 8)),
                                                ident!("e", make_strpos!(1, 11, 1, 11)),
                                            ]
                                        ),
                                        make_strpos!(1, 13, 1, 13),
                                        "f".to_owned(), make_strpos!(1, 14, 1, 14),
                                        make_strpos!(1, 15, 1, 24), vec![
                                            ident!("g", make_strpos!(1, 16, 1, 16)),
                                            ident!("h", make_strpos!(1, 19, 1, 19)),
                                            ident!("i", make_strpos!(1, 22, 1, 22)),
                                        ]
                                    ),
                                    make_strpos!(1, 25, 1, 28), vec![
                                        ident!("u", make_strpos!(1, 26, 1, 26))
                                    ]
                                ),
                                make_strpos!(1, 29, 1, 29),
                                "j".to_owned(), make_strpos!(1, 30, 1, 30)
                            ),
                            make_strpos!(1, 31, 1, 33), vec![
                                ident!("k", make_strpos!(1, 32, 1, 32))
                            ]
                        ),
                        make_strpos!(1, 34, 1, 34),
                        "l".to_owned(), make_strpos!(1, 35, 1, 35),
                        make_strpos!(1, 36, 1, 37),
                        vec![]
                    ),
                    make_strpos!(1, 38, 1, 38),
                    "m".to_owned(), make_strpos!(1, 39, 1, 39)
                ),
                make_strpos!(1, 40, 1, 48), vec![
                    ident!("n", make_strpos!(1, 41, 1, 41)),
                    ident!("o", make_strpos!(1, 44, 1, 44)),
                    ident!("p", make_strpos!(1, 47, 1, 47))
                ]
            ),
            make_strpos!(1, 49, 1, 58), vec![
                ident!("r", make_strpos!(1, 50, 1, 50)),
                ident!("s", make_strpos!(1, 53, 1, 53)),
                ident!("t", make_strpos!(1, 56, 1, 56))
            ]
        )
    }
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, "a", make_strpos!(1, 1, 1, 1)), 
            make_strpos!(1, 2, 1, 3), vec![]
        )), 
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_strpos!(1, 2, 1, 3), "subscription here")])
        ],
        3
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a[, ]"), (
        Some(PostfixExpr::new_subscription(
            ident!(post, "a", make_strpos!(1, 1, 1, 1)), 
            make_strpos!(1, 2, 1, 5), vec![]
        )), 
        make_messages![
            Message::new_by_str("Empty subscription", vec![(make_strpos!(1, 2, 1, 5), "subscription here")])
        ],
        4
    )}
    
    assert_eq!{ PostfixExpr::with_test_str_ret_size_messages("a(, )"), (
        Some(PostfixExpr::new_function_call(
            ident!(post, "a", make_strpos!(1, 1, 1, 1)),
            make_strpos!(1, 2, 1, 5), vec![]
        )), 
        make_messages![
            Message::new_by_str("Single comma in function call", vec![(make_strpos!(1, 2, 1, 5), "function call here")])
        ],
        4
    )}
}