
// MultiplicativeExpression = UnaryExpression | MultiplicativeExpression MultiplicativeOperator UnaryExpression
// AdditiveExpression = MultiplicativeExpression | AdditiveExpression AdditiveOperator MultiplicativeExpression
// ShiftExpression = AdditiveExpression | ShiftExpression ShiftOperator AdditiveExpression
// RelationalExpression = ShiftExpression | RelationalExpression RelationalOperator ShiftExpression
// BitAndExpression = RelationalExpression | BitAndExpression BitAndOperator RelationalExpression
// BitXorExpression = BitAndExpression | BitXorExpression BitXorOperator BitAndExpression
// BitOrExpression = BitXorExpression | BitOrExpression BitOrOperator BitXorExpression
// EqualityExpression = BitOrExpression | EqualityExpression EqualityOperator BitOrExpression  // `==` and `!=` lower than `|` for `if (enum_var & enum_mem1 == enum_mem1)` 
// LogicalAndExpression = EqualityExpression | LogicalAndExpression LogicalAndOperator EqualityExpression 
// LogicalOrExpression = LogicalAndExpression | LogicalOrExpression LogicalOrOperator LogicalAndExpression

use std::fmt;

use codepos::StringPosition;
use message::MessageCollection;

use lexical::TokenStream;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::unary::UnaryExpression;
use super::postfix::PostfixExpression;
use super::primary::PrimaryExpression;
use lexical::LitValue;

// TODO: try change them to cfg_attr(test, ...)
#[derive(Eq, PartialEq, Clone)]
struct BinaryBinaryExpr {
    left: BinaryExpr,
    operator: SeperatorKind,            // this means every binary operator matches a binary expr
    operator_strpos: StringPosition, 
    right: BinaryExpr,
    all_strpos: StringPosition,
}
#[derive(Eq, PartialEq, Clone)]
enum BinaryExprImpl {
    Unary(UnaryExpression),
    Binary(BinaryBinaryExpr),
}
#[derive(Eq, PartialEq, Clone)]
pub struct BinaryExpr(Box<BinaryExprImpl>); // wrapper for make it not public

impl ISyntaxItemFormat for BinaryExpr {
    fn format(&self, indent: u32) -> String {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => format!("{}", unary_expr.format(indent)),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref left, ref right, ref operator, ref operator_strpos, ref all_strpos }) => {
                format!("{}BinaryExpr <{:?}>\n{}\n{}{} <{:?}>\n{}", 
                    BinaryExpr::indent_str(indent), all_strpos,
                    left.format(indent + 1),
                    BinaryExpr::indent_str(indent + 1), operator, operator_strpos,
                    right.format(indent + 1),
                )
            }
        }
    }
}
impl fmt::Debug for BinaryExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}
impl BinaryExpr { // New

    pub fn new_binary(left: BinaryExpr, operator: SeperatorKind, operator_strpos: StringPosition, right: BinaryExpr) -> BinaryExpr {

        let all_strpos =  StringPosition::merge(left.pos_all(), right.pos_all());
        BinaryExpr(Box::new(BinaryExprImpl::Binary(BinaryBinaryExpr{
            left: left,
            right: right,
            operator: operator,
            operator_strpos: operator_strpos,
            all_strpos: all_strpos
        })))
    }
    pub fn new_unary(unary_expr: UnaryExpression) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(unary_expr)))
    }
    pub fn new_postfix(postfix_expr: PostfixExpression) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(UnaryExpression::new_postfix(postfix_expr))))
    }
    pub fn new_primary(primary_expr: PrimaryExpression) -> BinaryExpr {
        BinaryExpr(Box::new(BinaryExprImpl::Unary(UnaryExpression::new_primary(primary_expr))))
    }
}
impl BinaryExpr { // get

    pub fn is_unary(&self) -> bool {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => true,
            &BinaryExprImpl::Binary(_) => false,
        }
    }
    pub fn is_binary(&self) -> bool {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => false,
            &BinaryExprImpl::Binary(_) => true,
        }
    }

    pub fn get_unary(&self) -> Option<&UnaryExpression> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => Some(unary_expr),
            &BinaryExprImpl::Binary(_) => None,
        }
    }
    pub fn get_left(&self) -> Option<&BinaryExpr> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref left, right: ref _1, operator: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(left),
        }
    }
    pub fn get_right(&self) -> Option<&BinaryExpr> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref right, left: ref _1, operator: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(right),
        }
    }
    pub fn get_operator(&self) -> Option<&SeperatorKind> {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => None,
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref operator, right: ref _1, left: ref _2, operator_strpos: ref _3, all_strpos: ref _4 }) => Some(operator),
        }
    }
    pub fn get_operator_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(_) => StringPosition::new(),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref operator_strpos, right: ref _1, operator: ref _2, left: ref _3, all_strpos: ref _4 }) => *operator_strpos,
        }
    }
    pub fn get_all_strpos(&self) -> StringPosition {
        match self.0.as_ref() {
            &BinaryExprImpl::Unary(ref unary_expr) => unary_expr.pos_all(),
            &BinaryExprImpl::Binary(BinaryBinaryExpr{ ref all_strpos, right: ref _1, operator: ref _2, operator_strpos: ref _3, left: ref _4 }) => *all_strpos,
        }
    }
}

fn parse_unary_wrapper(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpr>, usize) {
    match UnaryExpression::parse(tokens, messages, index) {
        (Some(unary_expr), symbol_len) => (Some(BinaryExpr::new_unary(unary_expr)), symbol_len),
        (None, symbol_len) => (None, symbol_len)
    }
}
macro_rules! impl_binary_parser {
    ($parser_name: ident, $previous_parser: ident, $op_category: expr) => (

        fn $parser_name(tokens: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpr>, usize) {
    
            let (left, mut current_len) = match $previous_parser(tokens, messages, index) {
                (None, length) => return (None, length),
                (Some(prev_level), prev_length) => (prev_level, prev_length),
            };

            if tokens.nth(index + current_len).is_seperator_category($op_category) {
                let operator_strpos = tokens.pos(index + current_len);
                let operator = tokens.nth(index + current_len).get_seperator().unwrap();
                current_len += 1;
                match $previous_parser(tokens, messages, index + current_len) {
                    (None, length) => (None, current_len + length),
                    (Some(right), right_len) => (Some(BinaryExpr::new_binary(left, operator, operator_strpos, right)), current_len + right_len),
                }
            } else {
                (Some(left), current_len)
            }
        }
    )
}
impl_binary_parser! { parse_multiplicative, parse_unary_wrapper, SeperatorCategory::Multiplicative }
impl_binary_parser! { parse_additive, parse_multiplicative, SeperatorCategory::Additive }
impl_binary_parser! { parse_shift, parse_additive, SeperatorCategory::Shift }
impl_binary_parser! { parse_relational, parse_shift, SeperatorCategory::Relational }
impl_binary_parser! { parse_bitand, parse_relational, SeperatorCategory::BitAnd }
impl_binary_parser! { parse_bitor, parse_bitand, SeperatorCategory::BitOr }
impl_binary_parser! { parse_bitxor, parse_bitor, SeperatorCategory::BitXor }
impl_binary_parser! { parse_equality, parse_bitxor, SeperatorCategory::Equality }
impl_binary_parser! { parse_logical_and, parse_equality, SeperatorCategory::LogicalAnd }
impl_binary_parser! { parse_logical_or, parse_logical_and, SeperatorCategory::LogicalOr }

impl ISyntaxItem for BinaryExpr {

    fn pos_all(&self) -> StringPosition {
        self.get_all_strpos()
    }

    fn is_first_final(lexer: &mut TokenStream, index: usize) -> bool {
        UnaryExpression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut TokenStream, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpr>, usize) {
        parse_logical_or(lexer, messages, index)
    }
}

#[cfg(test)] #[test]
fn binary_expr_format() {
    
    let binary_expr = BinaryExpr::new_binary(
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1))),
        SeperatorKind::Add,
        make_str_pos!(1, 3, 1, 3),
        BinaryExpr::new_primary(PrimaryExpression::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5))),
    );
    assert_eq!(binary_expr.format(0), "BinaryExpr <<0>1:1-1:5>\n  Literal: (i32)1 <<0>1:1-1:1>\n  + <<0>1:3-1:3>\n  Literal: (i32)2 <<0>1:5-1:5>");
}

#[cfg(test)] #[test]
fn binary_expr_parse() {

    perrorln!("{:?}", BinaryExpr::with_test_str("[1] * [2] / [3]"));
    perrorln!("{:?}", BinaryExpr::with_test_str("a * b / c + d % e - f"));
    perrorln!("{:?}", BinaryExpr::with_test_str("a * b << h / c + d % e - f >> g"));
    perrorln!("{:?}", BinaryExpr::with_test_str("a * b << h / c + d % e - f >> g > h * i < j << k > m && n || o & p | q ^ r != s == t"));
    perrorln!("{:?}", BinaryExpr::with_test_str("a & b == c"));
}

#[cfg(test)]
mod tests {
    use super::super::super::ast_item::ISyntaxItem;
    use codepos::StringPosition;

    use lexical::SeperatorKind;
    use lexical::NumLitValue;
    use lexical::LitValue;

    use super::super::SMType;
    use super::super::super::expression::postfix::PostfixExpression;
    use super::super::super::expression::primary::PrimaryExpression;
    use super::super::super::expression::postfix::Postfix;
    use super::super::super::expression::unary::UnaryOperator;
    use super::super::super::expression::unary::UnaryExpression;
    use super::super::super::expression::binary::BinaryExpr;
    
    // Helper macros
    // primary expression
    macro_rules! expr_to_primary {
        ($inner: expr) => (BinaryExpr::new_unary(UnaryExpression{ post: PostfixExpression{ prim: $inner, postfixs: Vec::new() }, unaries: Vec::new() }))
    }
    macro_rules! expr_ident { 
        ($name: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Ident($name.to_owned(), $pos)))
    }
    macro_rules! expr_str_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Str(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Str(Some($val.to_owned())), $pos)))
    }
    macro_rules! expr_char_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Char(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Char(Some($val)), $pos)))
    }
    macro_rules! expr_num_lit { 
        ($pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Num(None), $pos)));
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Num(Some($val)), $pos)))
    }
    macro_rules! expr_bool_lit { 
        ($val: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::Lit(LitValue::Bool($val), $pos)))
    }
    macro_rules! expr_paren_expr { 
        ($expr: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_paren($expr, $pos)))
    }
    macro_rules! expr_array_def { 
        ([$($exprs: expr, )*] $pos: expr) => (expr_to_primary!(PrimaryExpression::ArrayDef(vec![$($exprs, )*], $pos)))
    }
    macro_rules! expr_array_dup_def { 
        ($expr1: expr, $expr2: expr, $pos: expr) => (expr_to_primary!(PrimaryExpression::make_array_dup_def($expr1, $expr2, $pos))); 
    }

    // postfix expression
    macro_rules! expr_to_postfix {
        ($prim: expr, $($posts: expr)*) => (
            BinaryExpr::new_unary(UnaryExpression{ 
                post: PostfixExpression{ prim: $prim, postfixs: vec![$($posts, )*]}, unaries: Vec::new() })
        )
    }

    // unary expression
    macro_rules! expr_to_unary {
        ($post: expr) => (BinaryExpr::new_unary(UnaryExpression{ post: $post, unary: Vec::new() }));
        ($post: expr, $($unaries: expr)*) => 
            (BinaryExpr::new_unary(UnaryExpression{ post: $post, unaries: vec![$($unaries, )*] }));
    }

    #[test]
    fn ast_expr_prim_parse() {

        macro_rules! parse {
            ($program: expr) => ({
                BinaryExpr::with_test_str($program)
            })
        }

        // Case 0
        assert_eq!(//12345678901234567890
            parse!( "[1, 2, 3f128, 0u64]"),
            expr_array_def!{[
                expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 2, 1, 2)),
                expr_num_lit!(NumLitValue::I32(2), make_str_pos!(1, 5, 1, 5)), 
                expr_num_lit!(make_str_pos!(1, 8, 1, 12)),
                expr_num_lit!(NumLitValue::U64(0), make_str_pos!(1, 15, 1, 18)),]
                make_str_pos!(1, 1, 1, 19)
            }
        );

        // Case 1          0        1         2         3
        //                 12345678901234567890123456789012345
        assert_eq!(parse!("[[(1)], [abc, (3)], [4, this, [6]]]"),
            expr_array_def!{[
                expr_array_def!{[
                    expr_paren_expr!(expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 4, 1, 4)), make_str_pos!(1, 3, 1, 5)),]
                    make_str_pos!(1, 2, 1, 6) 
                },
                expr_array_def!{[
                    expr_ident!("abc", make_str_pos!(1, 10, 1, 12)),
                    expr_paren_expr!(expr_num_lit!(NumLitValue::I32(3), make_str_pos!(1, 16, 1, 16)), make_str_pos!(1, 15, 1, 17)),]
                    make_str_pos!(1, 9, 1, 18)
                },
                expr_array_def!{[
                    expr_num_lit!(NumLitValue::I32(4), make_str_pos!(1, 22, 1, 22)),
                    expr_ident!("this", make_str_pos!(1, 25, 1, 28)),
                    expr_array_def!{[
                        expr_num_lit!(NumLitValue::I32(6), make_str_pos!(1, 32, 1, 32)),]
                        make_str_pos!(1, 31, 1, 33)
                    },]
                    make_str_pos!(1, 21, 1, 34)
                },]
                make_str_pos!(1, 1, 1, 35)
            }
        );

        // Case 2, empty array literal
        assert_eq!(
            parse!("[]"),
            expr_array_def!{
                []
                make_str_pos!(1, 1, 1, 2)
            }
        );

        // Case 3    0        1           2          3         4         5           6
        assert_eq!(//12345678901234 5678 9012 3456789012345678901234567890123 456789 0123456
            parse!( "[abc, 123u32, \"456\", '\\u0065', false, (), (a), (abc, \"hello\", ), ]"),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 7, 1, 12)),
                expr_str_lit!("456", make_str_pos!(1, 15, 1, 19)),
                expr_char_lit!('\u{0065}', make_str_pos!(1, 22, 1, 29)),
                expr_bool_lit!(false, make_str_pos!(1, 32, 1, 36)),
                expr_to_primary!(PrimaryExpression::Unit(make_str_pos!(1, 39, 1, 40))),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 44, 1, 44)), make_str_pos!(1, 43, 1, 45)),
                expr_to_primary!(PrimaryExpression::TupleDef(
                    vec![
                        expr_ident!("abc", make_str_pos!(1, 49, 1, 51)),
                        expr_str_lit!("hello", make_str_pos!(1, 54, 1, 60)),
                    ], 
                    make_str_pos!(1, 48, 1, 63)
                )), ]
                make_str_pos!(1, 1, 1, 66)
            }
        );        
        
        // Case 4    0        1            2          3         4
        assert_eq!(//123456789012 3456 78 9012 345678901234567890123456
            parse!( "[abc, 123f, \"456\\u\", '\\u00', false, (a), (  )]"),
            expr_array_def!{[
                expr_ident!("abc", make_str_pos!(1, 2, 1, 4)),
                expr_num_lit!(make_str_pos!(1, 7, 1, 10)),
                expr_str_lit!(make_str_pos!(1, 13, 1, 19)),
                expr_char_lit!( make_str_pos!(1, 22, 1, 27)),
                expr_bool_lit!(false, make_str_pos!(1, 30, 1, 34)),
                expr_paren_expr!(expr_ident!("a", make_str_pos!(1, 38, 1, 38)), make_str_pos!(1, 37, 1, 39)),
                expr_to_primary!(PrimaryExpression::Unit(make_str_pos!(1, 42, 1, 45))),]
                make_str_pos!(1, 1, 1, 46)
            }
        );

        // Case 5    0        1         2
        assert_eq!(//1234567890123456789012
            parse!( "[[123u32, abc]; 4567]"),
            expr_array_dup_def!{
                expr_array_def!{[
                    expr_num_lit!(NumLitValue::U32(123), make_str_pos!(1, 3, 1, 8)),
                    expr_ident!("abc", make_str_pos!(1, 11, 1, 13)),]
                    make_str_pos!(1, 2, 1, 14)
                },
                expr_num_lit!(NumLitValue::I32(4567), make_str_pos!(1, 17, 1, 20)),
                [make_str_pos!(1, 1, 1, 21), make_str_pos!(1, 15, 1, 15)]
            }
        );   


    }
    
    #[test]
    fn ast_expr_post_parse() {
        // TODO: there was println!(messages) to manually check messages, update them to auto check

        //                                      0        1         2         3         4         5         6         7         8     
        //                                      1234567890123456789012345678901234567890123456789012345678901234567890123456789
        let left = BinaryExpr::with_test_str("abc.defg[[1](klm, [123, 456,], )](opq, 456.)() as [i32].rst[uvw, xyz, ABC]");
        let right = expr_to_postfix!{
            PrimaryExpression::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
            Postfix::MemberAccess("defg".to_owned(), make_str_pos!(1, 4, 1, 8))
            Postfix::Subscription(vec![
                expr_to_postfix!(
                    PrimaryExpression::ArrayDef(
                        vec![expr_num_lit!(NumLitValue::I32(1), make_str_pos!(1, 11, 1, 11))],
                        make_str_pos!(1, 10, 1, 12)
                    ), 
                    Postfix::FunctionCall(vec![
                        expr_ident!("klm", make_str_pos!(1, 14, 1, 16)),
                        expr_array_def!([
                            expr_num_lit!(NumLitValue::I32(123), make_str_pos!(1, 20, 1, 22)),
                            expr_num_lit!(NumLitValue::I32(456), make_str_pos!(1, 25, 1, 27)), ]
                            make_str_pos!(1, 19, 1, 29)
                        )], 
                        make_str_pos!(1, 13, 1, 32)
                    )
                )], 
                make_str_pos!(1, 9, 1, 33)
            )
            Postfix::FunctionCall(vec![
                expr_ident!("opq", make_str_pos!(1, 35, 1, 37)),
                expr_num_lit!(NumLitValue::F64(456f64), make_str_pos!(1, 40, 1, 43))],
                make_str_pos!(1, 34, 1, 44) 
            )
            Postfix::FunctionCall(
                Vec::new(),
                make_str_pos!(1, 45, 1, 46)
            )
            Postfix::TypeCast(
                SMType::Array(Box::new(
                    SMType::Base("i32".to_owned(), make_str_pos!(1, 52, 1, 54))
                ), make_str_pos!(1, 51, 1, 55)),
                make_str_pos!(1, 48, 1, 49)
            )
            Postfix::MemberAccess("rst".to_owned(), make_str_pos!(1, 56, 1, 59))
            Postfix::Subscription(vec![
                expr_ident!("uvw", make_str_pos!(1, 61, 1, 63)),
                expr_ident!("xyz", make_str_pos!(1, 66, 1, 68)),
                expr_ident!("ABC", make_str_pos!(1, 71, 1, 73)),],
                make_str_pos!(1, 60, 1, 74),
            )
        };

        assert_eq!(left, right);
        let left_desc = &format!("{:?}", left);
        let right_desc = &format!("{:?}", right);
        for (index, (ch1, ch2)) in left_desc.chars().into_iter().zip(right_desc.chars().into_iter()).enumerate() {
            if ch1 != ch2 {
                panic!("ch pair diff at {}: {}, {}", index, ch1, ch2);
          
            }
        }
    }

    #[test]
    fn ast_expr_post_helloworld_expr() {

        perrorln!("{:?}", BinaryExpr::with_test_str("writeln(\"helloworld\")"));
    }

    #[test]
    fn ast_expr_unary_parse() {
        
        assert_eq!( //                   12345678901234
            BinaryExpr::with_test_str("++!~[!1; ~--2]"),
            expr_to_unary!(
                PostfixExpression{ 
                    prim: PrimaryExpression::make_array_dup_def(
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::Lit(LitValue::Num(Some(NumLitValue::I32(1))), make_str_pos!(1, 7, 1, 7)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::LogicalNot, make_str_pos!(1, 6, 1, 6))
                        ), 
                        expr_to_unary!(
                            PostfixExpression{ 
                                prim: PrimaryExpression::Lit(LitValue::Num(Some(NumLitValue::I32(2))), make_str_pos!(1, 13, 1, 13)),
                                postfixs: Vec::new()
                            },
                            UnaryOperator::new(SeperatorKind::BitNot, make_str_pos!(1, 10, 1, 10))
                            UnaryOperator::new(SeperatorKind::Decrease, make_str_pos!(1, 11, 1, 12))
                        ), 
                        [make_str_pos!(1, 5, 1, 14), make_str_pos!(1, 8, 1, 8)]
                    ),
                    postfixs: Vec::new(),
                },
                UnaryOperator::new(SeperatorKind::Increase, make_str_pos!(1, 1, 1, 2))
                UnaryOperator::new(SeperatorKind::LogicalNot, make_str_pos!(1, 3, 1, 3))
                UnaryOperator::new(SeperatorKind::BitNot, make_str_pos!(1, 4, 1, 4))
            )
        );
    }
}