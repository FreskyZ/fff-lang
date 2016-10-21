
// Expression interface
// to make 3d expression like Expression{ Expression, some other }, that is, flattened 3DExpression

use std::fmt;

use common::From2;
use common::Position;
use common::StringPosition;

use lexical::Lexer;
use lexical::NumericLiteralValue;
use lexical::SeperatorKind;

use syntax::SMType;
use syntax::ast_item::IASTItem;

mod primary;
mod postfix;
mod unary;
mod binary;
mod d3;

use self::d3::D3Expression;
// use self::binary::BinaryOperator;
use self::binary::BinaryExpression;
// use self::unary::UnaryOperator;
use self::unary::UnaryExpression;
use self::postfix::Postfix;
use self::postfix::PostfixExpression;
use self::primary::PrimaryExpressionBase;
use self::primary::PrimaryExpression;

#[derive(Eq, PartialEq)]
pub enum ExpressionBase {
    StringLiteral(String),
    CharLiteral(char),
    NumericLiteral(NumericLiteralValue),
    BooleanLiteral(bool),
    Identifier(String),
    Paren(Expression),
    ArrayDef(Vec<Expression>),
    ArrayDupDef(Expression, Expression, Position), // '[',  ';', ']''s position, TODO: this position is for semicolon, not implemented
}

fn format_vector_display<T: fmt::Display>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}
fn format_vector_debug<T: fmt::Debug>(items: &Vec<T>, sep: &str) -> String {
    let length = items.len();
    let mut buf = String::new();
    for (index, item) in items.iter().enumerate() {
        buf.push_str(&format!("{:?}", item));
        if index != length - 1 {
            buf.push_str(sep);
        }
    }
    buf
}

impl fmt::Debug for ExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionBase::Identifier(ref name) => write!(f, "{}", name),
            ExpressionBase::StringLiteral(ref val) => write!(f, "{:?}", val),
            ExpressionBase::CharLiteral(ref val) => write!(f, "{:?}", val),
            ExpressionBase::NumericLiteral(ref val) => write!(f, "{:?}", val),
            ExpressionBase::BooleanLiteral(ref val) => write!(f, "{}", val),
            ExpressionBase::Paren(ref expr) => write!(f, "({:?})", expr),
            ExpressionBase::ArrayDupDef(ref expr1, ref expr2, ref pos) => 
                write!(f, "[{:?}; @ {:?} {:?}]", expr1, pos, expr2),
            ExpressionBase::ArrayDef(ref exprs) => 
                write!(f, "[{}]", format_vector_debug(exprs, ", ")),
        }
    }
}
impl fmt::Display for ExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionBase::Identifier(ref name) => write!(f, "{}", name),
            ExpressionBase::StringLiteral(ref val) => write!(f, "{:?}", val),
            ExpressionBase::CharLiteral(ref val) => write!(f, "{:?}", val),
            ExpressionBase::NumericLiteral(ref val) => write!(f, "{}", val),
            ExpressionBase::BooleanLiteral(ref val) => write!(f, "{}", val),
            ExpressionBase::Paren(ref expr) => write!(f, "({})", expr),
            ExpressionBase::ArrayDupDef(ref expr1, ref expr2, ref _pos) => 
                write!(f, "[{}; {}]", expr1, expr2),
            ExpressionBase::ArrayDef(ref exprs) => 
                write!(f, "[{}]", format_vector_display(exprs, ", ")),
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum ExpressionOperator {
    MemberAccess(String, StringPosition), // `.xxxx`'s position 
    FunctionCall(Vec<Expression>, StringPosition), // '(', ')''s position, TODO: implement the position
    Subscription(Vec<Expression>, StringPosition), // '[', ']''s position, TODO: implement it
    TypeCast(SMType, StringPosition), // `as`'s position TODO: implement it 
    Unary(SeperatorKind, StringPosition),
    Binary(SeperatorKind, StringPosition, Expression), // operator, pos, operand
}
impl fmt::Debug for ExpressionOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            ExpressionOperator::Subscription(ref exprs, ref pos) => format!(".operator[]({}) @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionOperator::FunctionCall(ref exprs, ref pos) => format!(".operator()({}) @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionOperator::MemberAccess(ref name, ref pos) => format!(".operator->({:?} @ {:?})", name, pos),
            ExpressionOperator::TypeCast(ref ty, ref pos) => format!(".operator {:?}() @ {:?}", ty, pos),
            ExpressionOperator::Unary(SeperatorKind::BitNot, ref pos) => format!(".operator~() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::LogicalNot, ref pos) => format!(".operator!() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::Increase, ref pos) => format!(".operator++() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::Decrease, ref pos) => format!(".operator--() @ {}", pos),
            ExpressionOperator::Binary(SeperatorKind::Mul, ref pos, ref operand) => format!(".operator*({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Div, ref pos, ref operand) => format!(".operator/({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Rem, ref pos, ref operand) => format!(".operator%({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Add, ref pos, ref operand) => format!(".operator+({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Sub, ref pos, ref operand) => format!(".operator-({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::ShiftLeft, ref pos, ref operand) => format!(".operator<<({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::ShiftRight, ref pos, ref operand) => format!(".operator>>({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Equal, ref pos, ref operand) => format!(".operator==({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::NotEqual, ref pos, ref operand) => format!(".operator!=({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Great, ref pos, ref operand) => format!(".operator>({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::Less, ref pos, ref operand) => format!(".operator<({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::GreatEqual, ref pos, ref operand) => format!(".operator>=({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::LessEqual, ref pos, ref operand) => format!(".operator<=({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::BitAnd, ref pos, ref operand) => format!(".operator&({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::BitOr, ref pos, ref operand) => format!(".operator|({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::BitXor, ref pos, ref operand) => format!(".operator^({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::LogicalAnd, ref pos, ref operand) => format!(".operator&&({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Binary(SeperatorKind::LogicalOr, ref pos, ref operand) => format!(".operator||({:?}) @ {:?}", operand, pos),
            ExpressionOperator::Unary(_, _) => unreachable!(),
            ExpressionOperator::Binary(_, _, _) => unreachable!(),
        })
    }
}
impl fmt::Display for ExpressionOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            ExpressionOperator::Subscription(ref exprs, ref _pos) => format!(".operator[]({})", format_vector_display(exprs, ", ")),
            ExpressionOperator::FunctionCall(ref exprs, ref _pos) => format!(".operator()({})", format_vector_display(exprs, ", ")),
            ExpressionOperator::MemberAccess(ref name, ref _pos) => format!(".operator->({})", name),
            ExpressionOperator::TypeCast(ref ty, ref _pos) => format!(".operator {}()", ty),
            ExpressionOperator::Unary(SeperatorKind::BitNot, ref _pos) => format!(".operator~()"),
            ExpressionOperator::Unary(SeperatorKind::LogicalNot, ref _pos) => format!(".operator!()"),
            ExpressionOperator::Unary(SeperatorKind::Increase, ref _pos) => format!(".operator++()"),
            ExpressionOperator::Unary(SeperatorKind::Decrease, ref _pos) => format!(".operator--()"),
            ExpressionOperator::Binary(SeperatorKind::Mul, ref _pos, ref operand) => format!(".operator*({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Div, ref _pos, ref operand) => format!(".operator/({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Rem, ref _pos, ref operand) => format!(".operator%({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Add, ref _pos, ref operand) => format!(".operator+({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Sub, ref _pos, ref operand) => format!(".operator-({})", operand),
            ExpressionOperator::Binary(SeperatorKind::ShiftLeft, ref _pos, ref operand) => format!(".operator<<({})", operand),
            ExpressionOperator::Binary(SeperatorKind::ShiftRight, ref _pos, ref operand) => format!(".operator>>({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Equal, ref _pos, ref operand) => format!(".operator==({})", operand),
            ExpressionOperator::Binary(SeperatorKind::NotEqual, ref _pos, ref operand) => format!(".operator!=({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Great, ref _pos, ref operand) => format!(".operator>({})", operand),
            ExpressionOperator::Binary(SeperatorKind::Less, ref _pos, ref operand) => format!(".operator<({})", operand),
            ExpressionOperator::Binary(SeperatorKind::GreatEqual, ref _pos, ref operand) => format!(".operator>=({})", operand),
            ExpressionOperator::Binary(SeperatorKind::LessEqual, ref _pos, ref operand) => format!(".operator<=({})", operand),
            ExpressionOperator::Binary(SeperatorKind::BitAnd, ref _pos, ref operand) => format!(".operator&({})", operand),
            ExpressionOperator::Binary(SeperatorKind::BitOr, ref _pos, ref operand) => format!(".operator|({})", operand),
            ExpressionOperator::Binary(SeperatorKind::BitXor, ref _pos, ref operand) => format!(".operator^({})", operand),
            ExpressionOperator::Binary(SeperatorKind::LogicalAnd, ref _pos, ref operand) => format!(".operator&&({})", operand),
            ExpressionOperator::Binary(SeperatorKind::LogicalOr, ref _pos, ref operand) => format!(".operator||({})", operand),
            ExpressionOperator::Unary(_, _) => unreachable!(),
            ExpressionOperator::Binary(_, _, _) => unreachable!(),
        })
    }
}

#[derive(Eq, PartialEq)]
pub struct Expression {
    pub base: Box<ExpressionBase>,
    pub ops: Vec<ExpressionOperator>,
    base_pos: StringPosition,
    all_pos: StringPosition,
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?} @ {:?}{} @ {:?}", self.base, self.base_pos, format_vector_debug(&self.ops, ""), self.all_pos)
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.base, format_vector_display(&self.ops, ""))
    }
}

impl Expression {

    fn new(base: ExpressionBase, base_pos: StringPosition, ops: Vec<ExpressionOperator>, pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), base_pos: base_pos, ops: ops, all_pos: pos }
    }

    #[cfg(test)]
    pub fn new_test(base: ExpressionBase, base_pos: StringPosition, ops: Vec<ExpressionOperator>, pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), base_pos: base_pos, ops: ops, all_pos: pos }
    }

    pub fn pos_base(&self) -> StringPosition {
        self.base_pos
    }
}

fn d3_expr_to_expr(d3: D3Expression) -> Expression {

    let D3Expression(BinaryExpression{ 
        ops: bin_ops,
        unary: UnaryExpression{
            unaries: unary_ops,
            post: PostfixExpression{
                postfixs: postfix_ops,
                prim: PrimaryExpression(
                    primary_base, 
                    primary_pos), 
            },
        }, 
    }) = d3;

    let mut pos_start = primary_pos.start_pos;
    let mut pos_end = primary_pos.end_pos;
    let base_pos = primary_pos;
    let expr_base = match primary_base {
        PrimaryExpressionBase::ArrayDef(d3_exprs) => 
            ExpressionBase::ArrayDef(d3_exprs.into_iter().map(d3_expr_to_expr).collect()),
        PrimaryExpressionBase::ArrayDupDef(d3_expr1, d3_expr2) =>
            ExpressionBase::ArrayDupDef(d3_expr_to_expr(d3_expr1.as_ref().clone()), d3_expr_to_expr(d3_expr2.as_ref().clone()), Position::new()),

        PrimaryExpressionBase::BooleanLiteral(val) => ExpressionBase::BooleanLiteral(val),
        PrimaryExpressionBase::CharLiteral(val) => ExpressionBase::CharLiteral(val),
        PrimaryExpressionBase::StringLiteral(val) => ExpressionBase::StringLiteral(val),
        PrimaryExpressionBase::NumericLiteral(val) => ExpressionBase::NumericLiteral(val),
        PrimaryExpressionBase::Identifier(name) => ExpressionBase::Identifier(name),
        PrimaryExpressionBase::ParenExpression(d3_expr) => ExpressionBase::Paren(d3_expr_to_expr(d3_expr.as_ref().clone())),
    };

    let mut ops = Vec::new();
    for postfix in postfix_ops {
        // pos_end = ... // Wait for their pos
        ops.push(match postfix {
            Postfix::FunctionCall(d3_exprs) => ExpressionOperator::FunctionCall(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), StringPosition::new()),
            Postfix::Subscription(d3_exprs) => ExpressionOperator::Subscription(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), StringPosition::new()),
            Postfix::MemberAccess(ident, pos) => ExpressionOperator::MemberAccess(ident, pos),
            Postfix::TypeCast(ty) => ExpressionOperator::TypeCast(ty, StringPosition::new()),
        });
    }

    let mut is_first_prefix = true;
    for prefix in unary_ops {
        if is_first_prefix {
            pos_start = prefix.pos.start_pos;
            is_first_prefix = false;
        }
        ops.push(ExpressionOperator::Unary(prefix.op, prefix.pos));
    }

    for binary in bin_ops {
        pos_end = binary.pos.end_pos;
        ops.push(ExpressionOperator::Binary(binary.operator, binary.pos, d3_expr_to_expr(binary.oprand)));
    }

    Expression::new(expr_base, base_pos, ops, StringPosition::from2(pos_start, pos_end))
}

impl IASTItem for Expression {

    fn pos_all(&self) -> StringPosition { self.all_pos }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Expression>, usize) {
        
        let (d3, d3_length) = match D3Expression::parse(lexer, index) {
            (Some(d3), d3_length) => (d3, d3_length),
            (None, length) => return (None, length),
        };

        (Some(d3_expr_to_expr(d3)), d3_length)
    } 
}

#[cfg(test)]
mod tests {
    use super::Expression;
    // use super::ExpressionBase;
    // use super::ExpressionOperator;

    use lexical::Lexer;
    use syntax::ast_item::IASTItem;

    #[test]
    #[ignore] // strange interactive test
    fn ast_expr_flatten() {
        use std::io::stdin;

        loop {
            let mut buf = String::new();
            match stdin().read_line(&mut buf) {
                Ok(_) => (),
                Err(_) => break,
            }

            if buf != "break" {
                let lexer = &mut Lexer::new(buf);
                let (result, length) = Expression::parse(lexer, 0);
                perrorln!("Debug: ({:?}, {})", result, length);
                match result {
                    Some(result) => perrorln!("Display: {}", result),
                    None => perrorln!("messages: {:?}", lexer.messages()),
                }
            } else {
                break;
            }
        }
    }
}