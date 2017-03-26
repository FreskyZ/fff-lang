
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

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::SeperatorCategory;

use super::super::ISyntaxItem;
use super::super::ISyntaxItemFormat;
use super::super::expression::d3::D3Expression;
use super::super::expression::unary::UnaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub struct BinaryOperator {
    pub operator: SeperatorKind,
    pub operator_strpos: StringPosition,
    pub operand: D3Expression,
}

#[derive(Eq, PartialEq, Clone)]
pub struct BinaryExpression {
    pub unary: UnaryExpression,
    pub operators: Vec<BinaryOperator>,
}
impl ISyntaxItemFormat for BinaryExpression {

    fn format(&self, indent: u32) -> String {
        if self.operators.len() == 0 {
            format!("{}", self.unary.format(indent))
        } else {
            format!("{}BinaryExpr:\n{}{}", 
                BinaryExpression::indent_str(indent), 
                self.unary.format(indent + 1),
                self.operators.iter().fold("\n".to_owned(),
                    |mut buf, &BinaryOperator{ ref operator, ref operator_strpos, ref operand }| { 
                        buf.push_str(&format!("{:?}<{:?}> {}", operator, operator_strpos, operand.format(indent + 1))); 
                        buf 
                    }
                ),
            )
        }
    }
}
impl fmt::Debug for BinaryExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\n{}", self.format(0))
    }
}

fn parse_multiplicative(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpression>, usize) {

    let (unary, mut current_len) = match UnaryExpression::parse(lexer, messages, index) {
        (None, length) => return (None, length),
        (Some(unary), unary_length) => (unary, unary_length),
    };

    let mut ops = Vec::new();
    loop {
        match lexer.nth(index + current_len).get_seperator() {
            Some(ref sep) if sep.is_category(SeperatorCategory::Multiplicative) => {
                current_len += 1;
                match UnaryExpression::parse(lexer, messages, index + current_len) {
                    (None, length) => return (None, current_len + length),
                    (Some(operand), operand_len) => {
                        ops.push(BinaryOperator{ 
                            operator: sep.clone(), 
                            operator_strpos: lexer.pos(index + current_len - 1), 
                            operand: D3Expression(BinaryExpression{ unary: operand, operators: Vec::new() }) 
                        });
                        current_len += operand_len;
                    }
                }
            }
            Some(_) | None => break,
        }
    }

    (Some(BinaryExpression{ unary: unary, operators: ops }), current_len)
}

macro_rules! impl_binary_parser {
    ($parser_name: ident, $previous_parser: ident, $op_category: expr) => (

        fn $parser_name(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpression>, usize) {
    
            let (mut left, mut current_len) = match $previous_parser(lexer, messages, index) {
                (None, length) => return (None, length),
                (Some(prev_level), prev_length) => (prev_level, prev_length),
            };

            loop {
                match lexer.nth(index + current_len).get_seperator() {
                    Some(ref sep) if sep.is_category($op_category) => {
                        current_len += 1;
                        match $previous_parser(lexer, messages, index + current_len) {
                            (None, length) => return (None, current_len + length),
                            (Some(operand), operand_len) => {
                                left.operators.push(BinaryOperator{ operator: sep.clone(), operator_strpos: lexer.pos(index + current_len - 1), operand: D3Expression(operand) });
                                current_len += operand_len;
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

impl ISyntaxItem for BinaryExpression {

    fn pos_all(&self) -> StringPosition {
        match self.operators.iter().last() {
            Some(operator) => StringPosition::merge(self.unary.pos_all(), operator.operand.pos_all()),
            None => self.unary.pos_all()
        }
    }

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        UnaryExpression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, messages: &mut MessageCollection, index: usize) -> (Option<BinaryExpression>, usize) {

        parse_logical_or(lexer, messages, index)
    }
}