
// Expression interface
// to make 3d expression like Expression{ Expression, some other }, that is, flattened 3DExpression

use std::fmt;

use common::From2;
use common::Position;
use common::StringPosition;
use common::format_vector_display;
use common::format_vector_debug;

use lexical::Lexer;
use lexical::NumLitValue;
use lexical::SeperatorKind;
use lexical::LexicalLiteral;

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
use self::primary::PrimaryExpression;

#[derive(Eq, PartialEq)]
pub enum ExpressionBase {
    Lit(LexicalLiteral, StringPosition),                // literal's postion
    Ident(String, StringPosition),                      // ident's position
    Paren(Expression, StringPosition),                  // '(', ')''s position
    Unit(StringPosition),                               // '(', ')''s position
    TupleDef(Vec<Expression>, StringPosition),          // '(', ')''s position
    ArrayDef(Vec<Expression>, StringPosition),          // '[', ']''s position
    ArrayDupDef(Expression, Expression, [StringPosition; 2]), // '[',  ';', ']''s position
}

impl fmt::Debug for ExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionBase::Ident(ref name, ref pos) => write!(f, "{} @ {:?}", name, pos),
            ExpressionBase::Lit(ref val, ref pos) => write!(f, "{:?} @ {:?}", val, pos),
            ExpressionBase::Paren(ref expr, ref pos) => write!(f, "({:?}) @ {:?}", expr, pos),
            ExpressionBase::Unit(ref pos) => write!(f, "() @ {:?}", pos),
            ExpressionBase::TupleDef(ref exprs, ref pos) => write!(f, "({}) @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionBase::ArrayDef(ref exprs, ref pos) => write!(f, "[{}] @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionBase::ArrayDupDef(ref expr1, ref expr2, ref pos) => write!(f, "[{:?}; @ {:?} {:?}] @ {:?}", expr1, pos[1], expr2, pos[0]),
        }
    }
}
impl fmt::Display for ExpressionBase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ExpressionBase::Ident(ref name, ref _pos) => write!(f, "{}", name),
            ExpressionBase::Lit(ref val, ref _pos) => write!(f, "{}", val),
            ExpressionBase::Paren(ref expr, ref _pos) => write!(f, "({})", expr),
            ExpressionBase::Unit(ref _pos) => write!(f, "()"),
            ExpressionBase::TupleDef(ref exprs, ref _pos) => write!(f, "({})", format_vector_display(exprs, ", ")),
            ExpressionBase::ArrayDef(ref exprs, ref _pos) =>  write!(f, "[{}]", format_vector_display(exprs, ", ")),
            ExpressionBase::ArrayDupDef(ref expr1, ref expr2, ref _pos) => write!(f, "[{}; {}]", expr1, expr2),
        }
    }
}
impl ExpressionBase {

    pub fn pos(&self) -> StringPosition {
        match *self {
            ExpressionBase::Ident(ref _name, ref pos) => *pos,
            ExpressionBase::Lit(ref _val, ref pos) => *pos,
            ExpressionBase::Paren(ref _expr, ref pos) => *pos,
            ExpressionBase::Unit(ref pos) => *pos,
            ExpressionBase::TupleDef(ref _exprs, ref pos) => *pos,
            ExpressionBase::ArrayDef(ref _exprs, ref pos) => *pos,
            ExpressionBase::ArrayDupDef(ref _expr1, ref _expr2, ref pos) => pos[0],
        }
    }
}

#[derive(Eq, PartialEq)]
pub enum ExpressionOperator {
    MemberAccess(String, StringPosition),               // `.xxxx`'s position 
    FunctionCall(Vec<Expression>, StringPosition),      // '(', ')''s position,
    MemberFunctionCall(String, Vec<Expression>, [StringPosition; 2]),   // .xxx and () 's position  
    GetIndex(Vec<Expression>, StringPosition),          // '[', ']''s position,
    MemberGetIndex(String, Vec<Expression>, [StringPosition; 2]),       // .xxx and [] 's position
    TypeCast(SMType, StringPosition),                   // `as`'s position
    Unary(SeperatorKind, StringPosition),
    Binary(SeperatorKind, StringPosition, Expression),  // operator, pos, operand
}
impl fmt::Debug for ExpressionOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            ExpressionOperator::GetIndex(ref exprs, ref pos) => format!(".operator[]({}) @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionOperator::FunctionCall(ref exprs, ref pos) => format!(".operator()({}) @ {:?}", format_vector_debug(exprs, ", "), pos),
            ExpressionOperator::MemberAccess(ref name, ref pos) => format!(".{} @ {:?}", name, pos),
            ExpressionOperator::TypeCast(ref ty, ref pos) => format!(".operator {:?}() @ {:?}", ty, pos),
            ExpressionOperator::MemberFunctionCall(ref name, ref exprs, ref pos) => format!(".{} @ {:?}({}) @ {:?}", name, pos[0], format_vector_debug(exprs, ", "), pos[1]),
            ExpressionOperator::MemberGetIndex(ref name, ref exprs, ref pos) => format!(".{} @ {:?}[{}] @ {:?}", name, pos[0], format_vector_debug(exprs, ", "), pos[1]),
            ExpressionOperator::Unary(SeperatorKind::BitNot, ref pos) => format!(".operator~() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::LogicalNot, ref pos) => format!(".operator!() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::Increase, ref pos) => format!(".operator++() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::Decrease, ref pos) => format!(".operator--() @ {}", pos),
            ExpressionOperator::Unary(SeperatorKind::Sub, ref pos) => format!(".operator-() @ {}", pos),
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
            ExpressionOperator::GetIndex(ref exprs, ref _pos) => format!(".operator[]({})", format_vector_display(exprs, ", ")),
            ExpressionOperator::FunctionCall(ref exprs, ref _pos) => format!(".operator()({})", format_vector_display(exprs, ", ")),
            ExpressionOperator::MemberAccess(ref name, ref _pos) => format!(".{}", name),
            ExpressionOperator::TypeCast(ref ty, ref _pos) => format!(".operator {}()", ty),
            ExpressionOperator::MemberFunctionCall(ref name, ref exprs, ref _pos) => format!(".{}({})", name, format_vector_display(exprs, ", ")),
            ExpressionOperator::MemberGetIndex(ref name, ref exprs, ref _pos) => format!(".{}({})", name, format_vector_display(exprs, ", ")),
            ExpressionOperator::Unary(SeperatorKind::BitNot, ref _pos) => format!(".operator~()"),
            ExpressionOperator::Unary(SeperatorKind::LogicalNot, ref _pos) => format!(".operator!()"),
            ExpressionOperator::Unary(SeperatorKind::Increase, ref _pos) => format!(".operator++()"),
            ExpressionOperator::Unary(SeperatorKind::Decrease, ref _pos) => format!(".operator--()"),
            ExpressionOperator::Unary(SeperatorKind::Sub, ref _pos) => format!(".operator-()"),
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
    all_pos: StringPosition,
}
impl fmt::Debug for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}{} @ {:?}", self.base, format_vector_debug(&self.ops, ""), self.all_pos)
    }
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}{}", self.base, format_vector_display(&self.ops, ""))
    }
}

impl Expression {

    fn new(base: ExpressionBase, ops: Vec<ExpressionOperator>, pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), ops: ops, all_pos: pos }
    }

    #[cfg(test)]
    pub fn new_test(base: ExpressionBase, ops: Vec<ExpressionOperator>, all_pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), ops: ops, all_pos: all_pos }
    }

    #[cfg(test)]
    // Directly from string, only for test, may panic
    pub fn from_str(program: &str, sym_index: usize) -> Expression {
        let lexer = &mut Lexer::new_test2(program);
        Expression::parse(lexer, sym_index).0.unwrap() 
    }
}

fn d3_expr_to_expr(d3: D3Expression) -> Expression {

    let D3Expression(BinaryExpression{ 
        ops: bin_ops,
        unary: UnaryExpression{
            unaries: unary_ops,
            post: PostfixExpression{
                postfixs: postfix_ops,
                prim: primary, 
            },
        }, 
    }) = d3;

    let expr_base = match primary {
        PrimaryExpression::Unit(pos) => ExpressionBase::Unit(pos),
        PrimaryExpression::Lit(val, pos) => ExpressionBase::Lit(val, pos),
        PrimaryExpression::Ident(name, pos) => ExpressionBase::Ident(name, pos),
        PrimaryExpression::ParenExpr(d3_expr, pos) => ExpressionBase::Paren(d3_expr_to_expr(d3_expr.as_ref().clone()), pos),
        PrimaryExpression::ArrayDef(d3_exprs, pos) => ExpressionBase::ArrayDef(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), pos),
        PrimaryExpression::TupleDef(d3_exprs, pos) => ExpressionBase::TupleDef(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), pos),
        PrimaryExpression::ArrayDupDef(d3_expr1, d3_expr2, pos) =>
            ExpressionBase::ArrayDupDef(d3_expr_to_expr(d3_expr1.as_ref().clone()), d3_expr_to_expr(d3_expr2.as_ref().clone()), pos),
    };
    let mut pos_all = expr_base.pos();

    fn postfix_to_operator(postfix: Postfix) -> ExpressionOperator {
        match postfix {
            Postfix::FunctionCall(d3_exprs, pos) => ExpressionOperator::FunctionCall(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), pos),
            Postfix::Subscription(d3_exprs, pos) => ExpressionOperator::GetIndex(d3_exprs.into_iter().map(d3_expr_to_expr).collect(), pos),
            Postfix::MemberAccess(ident, pos) => ExpressionOperator::MemberAccess(ident, pos),
            Postfix::TypeCast(ty, pos) => ExpressionOperator::TypeCast(ty, pos),
        }
    }
    let mut ops = Vec::new();
    let mut postfix_ops_iter = postfix_ops.into_iter();
    loop {
        match (postfix_ops_iter.next(), postfix_ops_iter.next()) {
            (Some(Postfix::MemberAccess(ident1, pos1)), Some(Postfix::FunctionCall(params, pos2))) => {
                pos_all.end_pos = pos2.end_pos;
                ops.push(ExpressionOperator::MemberFunctionCall(
                    ident1, 
                    params.into_iter().map(d3_expr_to_expr).collect(), 
                    [pos1, pos2]
                ));
            }
            (Some(Postfix::MemberAccess(ident1, pos1)), Some(Postfix::Subscription(indexers, pos2))) => {
                pos_all.end_pos = pos2.end_pos;
                ops.push(ExpressionOperator::MemberGetIndex(
                    ident1, 
                    indexers.into_iter().map(d3_expr_to_expr).collect(),
                    [pos1, pos2]
                ));
            }
            (Some(other_postfix1), Some(other_postfix2)) => {
                pos_all.end_pos = other_postfix2.pos().end_pos;
                ops.push(postfix_to_operator(other_postfix1));
                ops.push(postfix_to_operator(other_postfix2));
            }
            (Some(other_postfix), None) => { 
                pos_all.end_pos = other_postfix.pos().end_pos;
                ops.push(postfix_to_operator(other_postfix));
                break;
            }
            (None, _) => break,
        }
    }

    for prefix in unary_ops.into_iter().rev() { // they are applied reversely
        pos_all.start_pos = prefix.pos.start_pos;
        ops.push(ExpressionOperator::Unary(prefix.op, prefix.pos));
    }

    for binary in bin_ops {
        pos_all.end_pos = binary.oprand.pos_all().end_pos;
        ops.push(ExpressionOperator::Binary(binary.operator, binary.pos, d3_expr_to_expr(binary.oprand)));
    }

    Expression::new(expr_base, ops, pos_all)
}

impl IASTItem for Expression {

    fn pos_all(&self) -> StringPosition { self.all_pos }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Expression>, usize) {
        
        match D3Expression::parse(lexer, index) {
            (Some(d3), d3_length) => (Some(d3_expr_to_expr(d3)), d3_length),
            (None, length) => (None, length),
        }
    } 
}

#[cfg(test)]
mod tests {
    use super::Expression;
    use super::ExpressionBase;
    use super::ExpressionOperator;

    use common::StringPosition;
    use message::SyntaxMessage;
    use message::MessageEmitter;
    use lexical::Lexer;
    use lexical::NumLitValue;
    use lexical::SeperatorKind;
    use lexical::LexicalLiteral;
    use syntax::ast_item::IASTItem;

    #[test]
    fn ast_expr_all() {

        macro_rules! test_case {
            ($program: expr, $len: expr, $pos_all: expr, $expr: expr) => ({
                let lexer = &mut Lexer::new_test2($program);
                if let (Some(expr), len) = Expression::parse(lexer, 0) {
                    assert_eq!(expr, $expr);
                    assert_eq!(len, $len);
                    assert_eq!(expr.pos_all(), $pos_all);
                } else {
                    panic!("expr is not some")
                }
            });
            ($program: expr, $len: expr, $pos_all: expr, $expr: expr, [$($msg: expr)*]) => ({
                let lexer = &mut Lexer::new_test2($program);
                if let (Some(expr), len) = Expression::parse(lexer, 0) {
                    assert_eq!(expr, $expr);
                    assert_eq!(len, $len);
                    assert_eq!(expr.pos_all(), $pos_all);
                    let messages = &mut MessageEmitter::new();
                    $(
                        messages.push($msg);
                    )*
                    assert_eq!(lexer.messages(), messages);
                } else {
                    panic!("expr is not some")
                }
            });
            ($program: expr, $len: expr, [$($msg: expr)*]) => ({
                let lexer = &mut Lexer::new_test2($program);
                let (expr, len) = Expression::parse(lexer, 0);
                assert_eq!(expr, None);
                assert_eq!(len, $len);

                let messages = &mut MessageEmitter::new();
                $(
                    messages.push($msg);
                )*
                assert_eq!(lexer.messages(), messages);
            });
        }

        // Features
        // Literal forward
        //            1234 5
        test_case!{ "\"abc\"", 1, make_str_pos!(1, 1, 1, 5),
            Expression::new_test(
                ExpressionBase::Lit(LexicalLiteral::from("abc"), make_str_pos!(1, 1, 1, 5)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 5),
            )
        }        //  12345678
        test_case!{ "0xfffu64", 1, make_str_pos!(1, 1, 1, 8),
            Expression::new_test(
                ExpressionBase::Lit(LexicalLiteral::from(0xfffu64), make_str_pos!(1, 1, 1, 8)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 8),
            )
        }        //  123
        test_case!{ "'f'", 1, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Lit(LexicalLiteral::from('f'), make_str_pos!(1, 1, 1, 3)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 3),
            )
        }
        test_case!{ "true", 1, make_str_pos!(1, 1, 1, 4),
            Expression::new_test(
                ExpressionBase::Lit(LexicalLiteral::from(true), make_str_pos!(1, 1, 1, 4)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 4),
            )
        }

        // Identifier forward
        //           0        1         2         3
        //           12345678901234567890123456789012
        test_case!{ "_very_long_var_name_to_avoid_use", 1, make_str_pos!(1, 1, 1, 32),
            Expression::new_test(
                ExpressionBase::Ident("_very_long_var_name_to_avoid_use".to_owned(), make_str_pos!(1, 1, 1, 32)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 32),
            )
        }

        // Unit Literal
        test_case!{ "()", 2, make_str_pos!(1, 1, 1, 2),
            Expression::new_test(
                ExpressionBase::Unit(make_str_pos!(1, 1, 1, 2)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 2),
            )
        }

        // Paren expr
        //           1234567
        test_case!{ "(1)", 3, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Paren(
                    Expression::new_test(
                        ExpressionBase::Lit(LexicalLiteral::from(1), make_str_pos!(1, 2, 1, 2)),
                        Vec::new(),
                        make_str_pos!(1, 2, 1, 2),
                    ),
                    make_str_pos!(1, 1, 1, 3),
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 3)
            )
        }
        // I can see future of Ok(())! 
        test_case!{ "(())", 4, make_str_pos!(1, 1, 1, 4),
            Expression::new_test(
                ExpressionBase::Paren(
                    Expression::new_test(
                        ExpressionBase::Unit(make_str_pos!(1, 2, 1, 3)),
                        Vec::new(),
                        make_str_pos!(1, 2, 1, 3),
                    ),
                    make_str_pos!(1, 1, 1, 4),
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 4)
            )
        }

        // Tuple def
        //           123456
        test_case!{ "(a, b)", 5, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::TupleDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(),
                            make_str_pos!(1, 2, 1, 2),
                        ),
                        Expression::new_test(
                            ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 5, 1, 5)),
                            Vec::new(),
                            make_str_pos!(1, 5, 1, 5),
                        )
                    ],
                    make_str_pos!(1, 1, 1, 6)
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 6),
            ) 
        }        //  12345678901
        test_case!{ "(1, 2, 3, )", 8, make_str_pos!(1, 1, 1, 11),
            Expression::new_test(
                ExpressionBase::TupleDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Lit(LexicalLiteral::from(1), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(), 
                            make_str_pos!(1, 2, 1, 2),
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LexicalLiteral::from(2), make_str_pos!(1, 5, 1, 5)),
                            Vec::new(),
                            make_str_pos!(1, 5, 1, 5),
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LexicalLiteral::from(3), make_str_pos!(1, 8, 1, 8)),
                            Vec::new(),
                            make_str_pos!(1, 8, 1, 8),
                        )
                    ],
                    make_str_pos!(1, 1, 1, 11),
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 11),
            )
        }

        // Array def
        test_case!{ "[a]", 3, make_str_pos!(1, 1, 1, 3), 
            Expression::new_test(
                ExpressionBase::ArrayDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(),
                            make_str_pos!(1, 2, 1, 2),
                        )
                    ],
                    make_str_pos!(1, 1, 1, 3)
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 3),
            )
        }        //  12345678
        test_case!{ "[1, 2, ]", 6, make_str_pos!(1, 1, 1, 8),
            Expression::new_test(
                ExpressionBase::ArrayDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Lit(LexicalLiteral::from(1), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(), 
                            make_str_pos!(1, 2, 1, 2)
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LexicalLiteral::from(2), make_str_pos!(1, 5, 1, 5)),
                            Vec::new(),
                            make_str_pos!(1, 5, 1, 5)
                        )
                    ],
                    make_str_pos!(1, 1, 1, 8)
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 8)
            )
        }
        test_case!{ "[]", 2, make_str_pos!(1, 1, 1, 2),
            Expression::new_test(
                ExpressionBase::ArrayDef(Vec::new(), make_str_pos!(1, 1, 1, 2)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 2),
            )
            // Temp error is not here
        }

        // Array dup def
        //           123456
        test_case!{ "[1; 2]", 5, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::ArrayDupDef(
                    Expression::new_test(
                        ExpressionBase::Lit(LexicalLiteral::from(1), make_str_pos!(1, 2, 1, 2)),
                        Vec::new(), 
                        make_str_pos!(1, 2, 1, 2)
                    ),
                    Expression::new_test(
                        ExpressionBase::Lit(LexicalLiteral::from(2), make_str_pos!(1, 5, 1, 5)),
                        Vec::new(),
                        make_str_pos!(1, 5, 1, 5)
                    ),
                    [
                        make_str_pos!(1, 1, 1, 6),
                        make_str_pos!(1, 3, 1, 3),
                    ]
                ),
                Vec::new(),
                make_str_pos!(1, 1, 1, 6),
            )
        }        //  1234567890
        test_case!{ "[[1]; [2]]", 9, make_str_pos!(1, 1, 1, 10), 
            Expression::new_test(
                ExpressionBase::ArrayDupDef(
                    Expression::new_test(
                        ExpressionBase::ArrayDef(
                            vec![
                                Expression::new_test(
                                    ExpressionBase::Lit(LexicalLiteral::from(1), make_str_pos!(1, 3, 1, 3)),
                                    Vec::new(),
                                    make_str_pos!(1, 3, 1, 3)
                                )
                            ],
                            make_str_pos!(1, 2, 1, 4),
                        ),
                        Vec::new(),
                        make_str_pos!(1, 2, 1, 4)
                    ),
                    Expression::new_test(
                        ExpressionBase::ArrayDef(
                            vec![
                                Expression::new_test(
                                    ExpressionBase::Lit(LexicalLiteral::from(2), make_str_pos!(1, 8, 1, 8)),
                                    Vec::new(),
                                    make_str_pos!(1, 8, 1, 8),
                                )
                            ],
                            make_str_pos!(1, 7, 1, 9)
                        ),
                        Vec::new(),
                        make_str_pos!(1, 7, 1, 9),
                    ),
                    [
                        make_str_pos!(1, 1, 1, 10),
                        make_str_pos!(1, 5, 1, 5),
                    ]
                ), 
                Vec::new(),
                make_str_pos!(1, 1, 1, 10),
            )
        }

        // Member access
        test_case!{ "a.b", 3, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::MemberAccess("b".to_owned(), make_str_pos!(1, 2, 1, 3)),
                ],
                make_str_pos!(1, 1, 1, 3)
            )
        }

        // function call
        test_case!{ "defg()", 3, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("defg".to_owned(), make_str_pos!(1, 1, 1, 4)),
                vec![
                    ExpressionOperator::FunctionCall(Vec::new(), make_str_pos!(1, 5, 1, 6)),
                ],
                make_str_pos!(1, 1, 1, 6),
            )
        }        //  123456
        test_case!{ "deg(a)", 4, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("deg".to_owned(), make_str_pos!(1, 1, 1, 3)),
                vec![
                    ExpressionOperator::FunctionCall(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 5, 1, 5)),
                                Vec::new(),
                                make_str_pos!(1, 5, 1, 5)
                            ),
                        ],
                        make_str_pos!(1, 4, 1, 6),
                    )
                ],
                make_str_pos!(1, 1, 1, 6),
            )
        }
        // test_case!{ "degg(a, b, )", 7,

        // }
        // test_case!{ "de(, )", 4,

        // }

        // // member function call
        // //           1234567
        // test_case!{ "abc.defg()", 3,  

        // }
        // test_case!{ "abc.deg(a)", 4,

        // }
        // test_case!{ "1.degg(a, b, )", 7,

        // }
        // test_case!{ "\"\".de(, )", 4,

        // }

        // // get index
        // test_case!{ "defg[]", 3,  

        // }
        // test_case!{ "deg[a]", 4,

        // }
        // test_case!{ "degg[a, b, ]", 7,

        // }
        // test_case!{ "de[, ]", 4,

        // }

        // // member get index
        // //           1234567
        // test_case!{ "abc.defg[]", 3,  

        // }
        // test_case!{ "abc.deg[a]", 4,

        // }
        // test_case!{ "1.degg[a, b, ]", 7,

        // }
        // test_case!{ "\"\".de[, ]", 4,

        // }

        // explicit type cast
        // logical not and bit not
        // increase and decrease
        // binary operators priority
    }

    #[test]
    #[ignore] // strange interactive test
    fn ast_expr_interactive() {
        use std::io::stdin;

        loop {
            let mut buf = String::new();

            perrorln!("Input:");
            match stdin().read_line(&mut buf) {
                Ok(_) => (),
                Err(_) => break,
            }

            if buf != "break\r\n" {
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