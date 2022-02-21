
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

use crate::common::From2;
use crate::common::StringPosition;

use crate::lexical::Lexer;
use crate::lexical::SeperatorKind;
use crate::lexical::SeperatorCategory;

use crate::syntax::ast_item::IASTItem;
use crate::syntax::expression::d3::D3Expression;
use crate::syntax::expression::unary::UnaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub struct BinaryOperator {
    pub operator: SeperatorKind,
    pub pos: StringPosition,
    pub oprand: D3Expression,
}

fn operator_to_string(op: &SeperatorKind) -> String {
    match *op {
        SeperatorKind::Mul => format!(".operator*"),
        SeperatorKind::Div => format!(".operator/"),
        SeperatorKind::Rem => format!(".operator%"),
        SeperatorKind::Add => format!(".operator+"),
        SeperatorKind::Sub => format!(".operator-"),
        SeperatorKind::ShiftLeft => format!(".operator<<"),
        SeperatorKind::ShiftRight => format!(".operator>>"),
        SeperatorKind::Equal => format!(".operator=="),
        SeperatorKind::NotEqual => format!(".operator!="),
        SeperatorKind::Great => format!(".operator>"),
        SeperatorKind::Less => format!(".operator<"),
        SeperatorKind::GreatEqual => format!(".operator>="),
        SeperatorKind::LessEqual => format!(".operator<="),
        SeperatorKind::BitAnd => format!(".operator&"),
        SeperatorKind::BitOr => format!(".operator|"),
        SeperatorKind::BitXor => format!(".operator^"),
        SeperatorKind::LogicalAnd => format!(".operator&&"),
        SeperatorKind::LogicalOr => format!(".operator||"),
        _ => unreachable!(),
    }
}

impl fmt::Debug for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} @ {:?}({:?})", operator_to_string(&self.operator), self.pos, self.oprand)
    }
}
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}({})", operator_to_string(&self.operator), self.oprand)
    }
}

#[derive(Eq, PartialEq, Clone)]
pub struct BinaryExpression {
    pub unary: UnaryExpression,
    pub ops: Vec<BinaryOperator>,
}

impl fmt::Debug for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{}", self.unary, self.ops.iter().fold(String::new(), |mut buf, op| { buf.push_str(&format!("{:?}", op)); buf }))
    }
}
impl fmt::Display for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.unary, self.ops.iter().fold(String::new(), |mut buf, op| { buf.push_str(&format!("{}", op)); buf }))
    }
}

fn parse_multiplicative(lexer: &mut Lexer, index: usize) -> (Option<BinaryExpression>, usize) {

    let (unary, mut current_len) = match UnaryExpression::parse(lexer, index) {
        (None, length) => return (None, length),
        (Some(unary), unary_length) => (unary, unary_length),
    };

    let mut ops = Vec::new();
    loop {
        match lexer.nth(index + current_len).get_seperator() {
            Some(ref sep) if sep.is_category(SeperatorCategory::Multiplicative) => {
                current_len += 1;
                match UnaryExpression::parse(lexer, index + current_len) {
                    (None, length) => return (None, current_len + length),
                    (Some(oprand), oprand_len) => {
                        ops.push(BinaryOperator{ 
                            operator: sep.clone(), 
                            pos: lexer.pos(index + current_len - 1), 
                            oprand: D3Expression(BinaryExpression{ unary: oprand, ops: Vec::new() }) 
                        });
                        current_len += oprand_len;
                    }
                }
            }
            Some(_) | None => break,
        }
    }

    (Some(BinaryExpression{ unary: unary, ops: ops }), current_len)
}

macro_rules! impl_binary_parser {
    ($parser_name: ident, $previous_parser: ident, $op_category: expr) => (

        fn $parser_name(lexer: &mut Lexer, index: usize) -> (Option<BinaryExpression>, usize) {
    
            let (mut left, mut current_len) = match $previous_parser(lexer, index) {
                (None, length) => return (None, length),
                (Some(prev_level), prev_length) => (prev_level, prev_length),
            };

            loop {
                match lexer.nth(index + current_len).get_seperator() {
                    Some(ref sep) if sep.is_category($op_category) => {
                        current_len += 1;
                        match $previous_parser(lexer, index + current_len) {
                            (None, length) => return (None, current_len + length),
                            (Some(oprand), oprand_len) => {
                                left.ops.push(BinaryOperator{ operator: sep.clone(), pos: lexer.pos(index + current_len - 1), oprand: D3Expression(oprand) });
                                current_len += oprand_len;
                            }
                        }
                    }
                    Some(_) | None => break,
                }
            }

            (Some(left), current_len)
        }
    )
}

impl_binary_parser! { parse_additive, parse_multiplicative, SeperatorCategory::Additive }
impl_binary_parser! { parse_shift, parse_additive, SeperatorCategory::Shift }
impl_binary_parser! { parse_relational, parse_shift, SeperatorCategory::Relational }
impl_binary_parser! { parse_bitand, parse_relational, SeperatorCategory::BitAnd }
impl_binary_parser! { parse_bitor, parse_bitand, SeperatorCategory::BitOr }
impl_binary_parser! { parse_bitxor, parse_bitor, SeperatorCategory::BitXor }
impl_binary_parser! { parse_equality, parse_bitxor, SeperatorCategory::Equality }
impl_binary_parser! { parse_logical_and, parse_equality, SeperatorCategory::LogicalAnd }
impl_binary_parser! { parse_logical_or, parse_logical_and, SeperatorCategory::LogicalOr }

impl BinaryExpression {
   
}

impl IASTItem for BinaryExpression {

    fn pos_all(&self) -> StringPosition {
        match self.ops.iter().last() {
            Some(op) => StringPosition::from2(self.unary.pos_all().start_pos, op.oprand.pos_all().end_pos),
            None => self.unary.pos_all()
        }
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        UnaryExpression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<BinaryExpression>, usize) {

        parse_logical_or(lexer, index)
    }
}