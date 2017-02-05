
// Expression interface
// to make 3d expression like Expression{ Expression, some other }, that is, flattened 3DExpression

use std::fmt;

use codemap::StringPosition;
use util::format_vector_display;
use util::format_vector_debug;

use lexical::Lexer;
use lexical::SeperatorKind;
use lexical::LitValue;

use syntax::SMType;
use syntax::ast_item::IASTItem;

mod primary;
mod postfix;
mod unary;
mod binary;
mod d3;

use self::d3::D3Expression;
use self::binary::BinaryExpression;
use self::unary::UnaryExpression;
use self::postfix::Postfix;
use self::postfix::PostfixExpression;
use self::primary::PrimaryExpression;

#[derive(Eq, PartialEq, Clone)]
pub enum ExpressionBase {
    Lit(LitValue, StringPosition),                // literal's postion
    Ident(String, StringPosition),                      // ident's position
    Paren(Expression, StringPosition),                  // '(', ')''s position
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
            ExpressionBase::TupleDef(ref _exprs, ref pos) => *pos,
            ExpressionBase::ArrayDef(ref _exprs, ref pos) => *pos,
            ExpressionBase::ArrayDupDef(ref _expr1, ref _expr2, ref pos) => pos[0],
        }
    }

    pub fn is_lit(&self) -> bool {
        match *self {
            ExpressionBase::Lit(_, _) => true,
            _ => false,
        }
    }
    pub fn get_lit(&self) -> Option<&LitValue> {
        match *self {
            ExpressionBase::Lit(ref val, ref _pos) => Some(val),
            _ => None,
        }
    }
}

#[derive(Eq, PartialEq, Clone)]
pub enum ExpressionOperator {
    MemberAccess(String, StringPosition),               // `.xxxx`'s position 
    FunctionCall(Vec<Expression>, StringPosition),      // '(', ')''s position,
    MemberFunctionCall(String, Vec<Expression>, [StringPosition; 2]),   // .xxx and () 's position  
    GetIndex(Vec<Expression>, StringPosition),          // '[', ']''s position,
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
#[derive(Eq, PartialEq, Clone)]
pub struct Expression {
    pub base: Box<ExpressionBase>,
    pub ops: Vec<ExpressionOperator>,
    pub all_pos: StringPosition,
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

    pub fn new(base: ExpressionBase, ops: Vec<ExpressionOperator>, pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), ops: ops, all_pos: pos }
    }

    #[cfg(test)]
    pub fn new_test(base: ExpressionBase, ops: Vec<ExpressionOperator>, all_pos: StringPosition) -> Expression {
        Expression{ base: Box::new(base), ops: ops, all_pos: all_pos }
    }

    #[cfg(test)] // Directly from string, only for test, may panic
    pub fn from_str(program: &str, sym_index: usize) -> Expression {
        let lexer = &mut Lexer::new(program);
        Expression::parse(lexer, sym_index).0.unwrap() 
    }

    pub fn pub_pos_all(&self) -> StringPosition { self.pos_all() }
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
        PrimaryExpression::Unit(pos) => ExpressionBase::Lit(LitValue::Unit, pos),
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
    // Big bug here, iter next **TWICE** every time!!!
    let mut previous = postfix_ops_iter.next();
    loop {
        match (previous, postfix_ops_iter.next()) {
            (Some(Postfix::MemberAccess(ident1, pos1)), Some(Postfix::FunctionCall(params, pos2))) => {
                pos_all.end_pos = pos2.end_pos;
                ops.push(ExpressionOperator::MemberFunctionCall(
                    ident1, 
                    params.into_iter().map(d3_expr_to_expr).collect(), 
                    [pos1, pos2]
                ));
                previous = postfix_ops_iter.next(); // skip one
            }
            (Some(other_postfix1), Some(other_postfix2)) => {
                pos_all.end_pos = other_postfix2.pos().end_pos;
                ops.push(postfix_to_operator(other_postfix1));
                // ops.push(postfix_to_operator(other_postfix2)); 
                previous = Some(other_postfix2);   // **IMPORTANT**, fix here, cannot skip one here
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

    fn is_first_final(lexer: &mut Lexer, index: usize) -> bool {
        BinaryExpression::is_first_final(lexer, index)
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Expression>, usize) {
        
        match D3Expression::parse(lexer, index) {
            (Some(d3), d3_length) => (Some(d3_expr_to_expr(d3)), d3_length),
            (None, length) => (None, length),
        }
    } 
}

impl Expression {

    pub fn is_pure_base(&self) -> bool { // ops == empty
        self.ops.is_empty()
    }

    pub fn get_base(&self) -> &ExpressionBase {
        self.base.as_ref()
    }

    // mainly for loop name specifier
    pub fn is_pure_str_lit(&self) -> bool {
        match self.base.as_ref() {
            &ExpressionBase::Lit(LitValue::Str(_), _) => true,
            _ => false, 
        }
    }
    // ignored lexically invalid string literal because all kinds of error will be denied after syntax parse
    pub fn into_pure_str_lit(self) -> Option<String> {
        match self.base.as_ref() {
            &ExpressionBase::Lit(LitValue::Str(Some(ref val)), _) => Some(val.clone()),
            &ExpressionBase::Lit(LitValue::Str(None), _) => None,
            _ => None,
        }
    }

    // Only last of ops is function call can be independent statement
    pub fn is_function_call(&self) -> bool {
        match self.ops.iter().last() {
            Some(&ExpressionOperator::FunctionCall(_, _))
            | Some(&ExpressionOperator::MemberFunctionCall(_, _, _)) => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Expression;
    use super::ExpressionBase;
    use super::ExpressionOperator;

    use codemap::StringPosition;
    use message::SyntaxMessage;
    use message::Message;
    use lexical::Lexer;
    use lexical::SeperatorKind;
    use lexical::LitValue;
    use syntax::ast_item::IASTItem;
    use syntax::SMType;
    use syntax::ast_item::TestCase;

    #[test]
    fn ast_expr_all() {

        // Features
        // Literal forward
        //            1234 5
        ast_test_case!{ "\"abc\"", 1, make_str_pos!(1, 1, 1, 5),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from("abc"), make_str_pos!(1, 1, 1, 5)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 5),
            )
        }        //  12345678
        ast_test_case!{ "0xfffu64", 1, make_str_pos!(1, 1, 1, 8),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(0xfffu64), make_str_pos!(1, 1, 1, 8)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 8),
            )
        }        //  123
        ast_test_case!{ "'f'", 1, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from('f'), make_str_pos!(1, 1, 1, 3)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 3),
            )
        }
        ast_test_case!{ "true", 1, make_str_pos!(1, 1, 1, 4),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(true), make_str_pos!(1, 1, 1, 4)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 4),
            )
        }

        // Identifier forward
        //           0        1         2         3
        //           12345678901234567890123456789012
        ast_test_case!{ "_very_long_var_name_to_avoid_use", 1, make_str_pos!(1, 1, 1, 32),
            Expression::new_test(
                ExpressionBase::Ident("_very_long_var_name_to_avoid_use".to_owned(), make_str_pos!(1, 1, 1, 32)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 32),
            )
        }

        // Unit Literal
        ast_test_case!{ "()", 2, make_str_pos!(1, 1, 1, 2),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::Unit, make_str_pos!(1, 1, 1, 2)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 2),
            )
        }

        // Paren expr
        //           1234567
        ast_test_case!{ "(1)", 3, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Paren(
                    Expression::new_test(
                        ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
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
        ast_test_case!{ "(())", 4, make_str_pos!(1, 1, 1, 4),
            Expression::new_test(
                ExpressionBase::Paren(
                    Expression::new_test(
                        ExpressionBase::Lit(LitValue::Unit, make_str_pos!(1, 2, 1, 3)),
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
        ast_test_case!{ "(a, b)", 5, make_str_pos!(1, 1, 1, 6),
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
        ast_test_case!{ "(1, 2, 3, )", 8, make_str_pos!(1, 1, 1, 11),
            Expression::new_test(
                ExpressionBase::TupleDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(), 
                            make_str_pos!(1, 2, 1, 2),
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
                            Vec::new(),
                            make_str_pos!(1, 5, 1, 5),
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 8, 1, 8)),
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
        ast_test_case!{ "[a]", 3, make_str_pos!(1, 1, 1, 3), 
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
        ast_test_case!{ "[1, 2, ]", 6, make_str_pos!(1, 1, 1, 8),
            Expression::new_test(
                ExpressionBase::ArrayDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(), 
                            make_str_pos!(1, 2, 1, 2)
                        ),
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
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
        ast_test_case!{ "[]", 2, make_str_pos!(1, 1, 1, 2),
            Expression::new_test(
                ExpressionBase::ArrayDef(Vec::new(), make_str_pos!(1, 1, 1, 2)),
                Vec::new(),
                make_str_pos!(1, 1, 1, 2),
            )
            // Temp error is not here
        }

        // Array dup def
        //           123456
        ast_test_case!{ "[1; 2]", 5, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::ArrayDupDef(
                    Expression::new_test(
                        ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
                        Vec::new(), 
                        make_str_pos!(1, 2, 1, 2)
                    ),
                    Expression::new_test(
                        ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 5, 1, 5)),
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
        ast_test_case!{ "[[1]; [2]]", 9, make_str_pos!(1, 1, 1, 10), 
            Expression::new_test(
                ExpressionBase::ArrayDupDef(
                    Expression::new_test(
                        ExpressionBase::ArrayDef(
                            vec![
                                Expression::new_test(
                                    ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 3, 1, 3)),
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
                                    ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 8, 1, 8)),
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
        ast_test_case!{ "a.b", 3, make_str_pos!(1, 1, 1, 3),
            Expression::new_test(
                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::MemberAccess("b".to_owned(), make_str_pos!(1, 2, 1, 3)),
                ],
                make_str_pos!(1, 1, 1, 3)
            )
        }

        // function call
        ast_test_case!{ "defg()", 3, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("defg".to_owned(), make_str_pos!(1, 1, 1, 4)),
                vec![
                    ExpressionOperator::FunctionCall(Vec::new(), make_str_pos!(1, 5, 1, 6)),
                ],
                make_str_pos!(1, 1, 1, 6),
            )
        }        //  123456
        ast_test_case!{ "deg(a)", 4, make_str_pos!(1, 1, 1, 6),
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
        }        //  1234567890123
        ast_test_case!{ "degg(a, b, )", 7, make_str_pos!(1, 1, 1, 12),
            Expression::new_test(
                ExpressionBase::Ident("degg".to_owned(), make_str_pos!(1, 1, 1, 4)),
                vec![
                    ExpressionOperator::FunctionCall(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 6, 1, 6)),
                                Vec::new(),
                                make_str_pos!(1, 6, 1, 6)
                            ),
                            Expression::new_test(
                                ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 9, 1, 9)),
                                Vec::new(),
                                make_str_pos!(1, 9, 1, 9),
                            )
                        ],
                        make_str_pos!(1, 5, 1, 12),
                    )
                ],
                make_str_pos!(1, 1, 1, 12),
            )
        }        //  123456
        ast_test_case!{ "de(, )", 4, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("de".to_owned(), make_str_pos!(1, 1, 1, 2)),
                vec![
                    ExpressionOperator::FunctionCall(
                        Vec::new(),
                        make_str_pos!(1, 3, 1, 6)
                    )
                ],
                make_str_pos!(1, 1, 1, 6),
            ),
            [
                Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_str_pos!(1, 3, 1, 6), comma_pos: make_str_pos!(1, 4, 1, 4).start_pos })
            ]
        }

        // member function call
        //           1234567890
        ast_test_case!{ "abc.defg()", 5, make_str_pos!(1, 1, 1, 10),
            Expression::new_test(
                ExpressionBase::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
                vec![
                    ExpressionOperator::MemberFunctionCall(
                        "defg".to_owned(),
                        Vec::new(),
                        [
                            make_str_pos!(1, 4, 1, 8),
                            make_str_pos!(1, 9, 1, 10),
                        ]
                    )
                ],
                make_str_pos!(1, 1, 1, 10),
            )
        }        //  1234567890
        ast_test_case!{ "abc.deg(a)", 6, make_str_pos!(1, 1, 1, 10),
            Expression::new_test(
                ExpressionBase::Ident("abc".to_owned(), make_str_pos!(1, 1, 1, 3)),
                vec![
                    ExpressionOperator::MemberFunctionCall(
                        "deg".to_owned(),
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 9, 1, 9)),
                                Vec::new(),
                                make_str_pos!(1, 9, 1, 9)
                            )
                        ],
                        [
                            make_str_pos!(1, 4, 1, 7),
                            make_str_pos!(1, 8, 1, 10),
                        ]
                    )
                ],
                make_str_pos!(1, 1, 1, 10)
            )
        }        //  12345678901234
        ast_test_case!{ "1.degg(a, b, )", 9, make_str_pos!(1, 1, 1, 14),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::MemberFunctionCall(
                        "degg".to_owned(),
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 8, 1, 8)),
                                Vec::new(),
                                make_str_pos!(1, 8, 1, 8),
                            ),
                            Expression::new_test(
                                ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 11, 1, 11)),
                                Vec::new(),
                                make_str_pos!(1, 11, 1, 11),
                            )
                        ],
                        [
                            make_str_pos!(1, 2, 1, 6),
                            make_str_pos!(1, 7, 1, 14),
                        ]
                    )
                ],
                make_str_pos!(1, 1, 1, 14)
            )
        }        //   1 23456789
        ast_test_case!{ "\"\".de(, )", 6, make_str_pos!(1, 1, 1, 9),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(""), make_str_pos!(1, 1, 1, 2)),
                vec![
                    ExpressionOperator::MemberFunctionCall(
                        "de".to_owned(),
                        Vec::new(),
                        [
                            make_str_pos!(1, 3, 1, 5),
                            make_str_pos!(1, 6, 1, 9),
                        ]
                    )
                ],
                make_str_pos!(1, 1, 1, 9)
            ),
            [
                Message::Syntax(SyntaxMessage::SingleCommaInFunctionCall{ call_pos: make_str_pos!(1, 6, 1, 9), comma_pos: make_str_pos!(1, 7, 1, 7).start_pos })
            ]
        }

        // get index
        ast_test_case!{ "defg[]", 3, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("defg".to_owned(), make_str_pos!(1, 1, 1, 4)),
                vec![
                    ExpressionOperator::GetIndex(Vec::new(), make_str_pos!(1, 5, 1, 6)),
                ],
                make_str_pos!(1, 1, 1, 6),
            )
        }        //  123456
        ast_test_case!{ "deg[a]", 4, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("deg".to_owned(), make_str_pos!(1, 1, 1, 3)),
                vec![
                    ExpressionOperator::GetIndex(
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
        }        //  123456789012
        ast_test_case!{ "degg[a, b, ]", 7, make_str_pos!(1, 1, 1, 12),
            Expression::new_test(
                ExpressionBase::Ident("degg".to_owned(), make_str_pos!(1, 1, 1, 4)),
                vec![
                    ExpressionOperator::GetIndex(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 6, 1, 6)),
                                Vec::new(),
                                make_str_pos!(1, 6, 1, 6)
                            ),
                            Expression::new_test(
                                ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 9, 1, 9)),
                                Vec::new(),
                                make_str_pos!(1, 9, 1, 9),
                            )
                        ],
                        make_str_pos!(1, 5, 1, 12),
                    )
                ],
                make_str_pos!(1, 1, 1, 12),
            )
        }        //  123456
        ast_test_case!{ "de[, ]", 4, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Ident("de".to_owned(), make_str_pos!(1, 1, 1, 2)),
                vec![
                    ExpressionOperator::GetIndex(
                        Vec::new(),
                        make_str_pos!(1, 3, 1, 6)
                    )
                ],
                make_str_pos!(1, 1, 1, 6),
            ),
            [
                Message::Syntax(SyntaxMessage::SingleCommaInSubscription{ sub_pos: make_str_pos!(1, 3, 1, 6), comma_pos: make_str_pos!(1, 4, 1, 4).start_pos })
            ]
        }

        // explicit type cast
        //           12345678
        ast_test_case!{ "1 as u32", 3, make_str_pos!(1, 1, 1, 8),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::TypeCast(
                        SMType::Base("u32".to_owned(), make_str_pos!(1, 6, 1, 8)),
                        make_str_pos!(1, 3, 1, 4),
                    )
                ],
                make_str_pos!(1, 1, 1, 8),
            )
        }        //  123456789012
        ast_test_case!{ "[1] as [f32]", 7, make_str_pos!(1, 1, 1, 12),
            Expression::new_test(
                ExpressionBase::ArrayDef(
                    vec![
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 2, 1, 2)),
                            Vec::new(), 
                            make_str_pos!(1, 2, 1, 2),
                        )
                    ],
                    make_str_pos!(1, 1, 1, 3),
                ),
                vec![
                    ExpressionOperator::TypeCast(
                        SMType::Array(Box::new(
                            SMType::Base("f32".to_owned(), make_str_pos!(1, 9, 1, 11))
                        ), make_str_pos!(1, 8, 1, 12)),
                        make_str_pos!(1, 5, 1, 6),
                    )
                ],
                make_str_pos!(1, 1, 1, 12),
            )
        }

        // Multi postfixes
        // member at prev, check 
        // sub at next, check
        // call at next, check
        // sub at prev, member at next, check
        // call at prev, cast at next, check
        // cast at prev, member at next check
        //           123456
        ast_test_case!{ "2[3].a", 6, make_str_pos!(1, 1, 1, 6),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::GetIndex(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 3, 1, 3)),
                                Vec::new(),
                                make_str_pos!(1, 3, 1, 3),
                            )
                        ], 
                        make_str_pos!(1, 2, 1, 4),
                    ),
                    ExpressionOperator::MemberAccess(
                        "a".to_owned(),
                        make_str_pos!(1, 5, 1, 6),
                    ),
                ],
                make_str_pos!(1, 1, 1, 6),
            )    //  0         1          2         
        }        //  123456 789012 345678901
        ast_test_case!{ "write(\"hello\") as i32", 6, make_str_pos!(1, 1, 1, 21), 
            Expression::new_test(
                ExpressionBase::Ident("write".to_owned(), make_str_pos!(1, 1, 1, 5)),
                vec![
                    ExpressionOperator::FunctionCall(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Lit(LitValue::from("hello"), make_str_pos!(1, 7, 1, 13)),
                                Vec::new(),
                                make_str_pos!(1, 7, 1, 13),
                            )
                        ],
                        make_str_pos!(1, 6, 1, 14),
                    ),
                    ExpressionOperator::TypeCast(
                        SMType::Base("i32".to_owned(), make_str_pos!(1, 19, 1, 21)),
                        make_str_pos!(1, 16, 1, 17),
                    )
                ],
                make_str_pos!(1, 1, 1, 21),
            )
        }        //  1234567890123456
        ast_test_case!{ "print(233, ).bit", 7, make_str_pos!(1, 1, 1, 16),
            Expression::new_test(
                ExpressionBase::Ident("print".to_owned(), make_str_pos!(1, 1, 1, 5)),
                vec![
                    ExpressionOperator::FunctionCall(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Lit(LitValue::from(233), make_str_pos!(1, 7, 1, 9)),
                                Vec::new(),
                                make_str_pos!(1, 7, 1, 9)
                            )
                        ],
                        make_str_pos!(1, 6, 1, 12),
                    ),
                    ExpressionOperator::MemberAccess(
                        "bit".to_owned(),
                        make_str_pos!(1, 13, 1, 16),
                    )
                ],
                make_str_pos!(1, 1, 1, 16),
            )
        }            //  12345678901234
        ast_test_case!{ "1.degg[a, b, ]", 9, make_str_pos!(1, 1, 1, 14),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::MemberAccess(
                        "degg".to_owned(),
                        make_str_pos!(1, 2, 1, 6),
                    ),
                    ExpressionOperator::GetIndex(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Ident("a".to_owned(), make_str_pos!(1, 8, 1, 8)),
                                Vec::new(),
                                make_str_pos!(1, 8, 1, 8),
                            ),
                            Expression::new_test(
                                ExpressionBase::Ident("b".to_owned(), make_str_pos!(1, 11, 1, 11)),
                                Vec::new(),
                                make_str_pos!(1, 11, 1, 11),
                            )
                        ],
                        make_str_pos!(1, 7, 1, 14),
                    )
                ],
                make_str_pos!(1, 1, 1, 14)
            )
        }        

        // logical not and bit not
        //           1234567
        ast_test_case!{ "!~!1[1]", 7, make_str_pos!(1, 1, 1, 7),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 4, 1, 4)),
                vec![
                    ExpressionOperator::GetIndex(
                        vec![
                            Expression::new_test(
                                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 6, 1, 6)),
                                Vec::new(),
                                make_str_pos!(1, 6, 1, 6),
                            )
                        ],
                        make_str_pos!(1, 5, 1, 7),
                    ),
                    ExpressionOperator::Unary(
                        SeperatorKind::LogicalNot,
                        make_str_pos!(1, 3, 1, 3),
                    ),
                    ExpressionOperator::Unary(
                        SeperatorKind::BitNot,
                        make_str_pos!(1, 2, 1, 2),
                    ),
                    ExpressionOperator::Unary(
                        SeperatorKind::LogicalNot,
                        make_str_pos!(1, 1, 1, 1),
                    )
                ],
                make_str_pos!(1, 1, 1, 7),
            )
        }

        // increase and decrease
        //           1234567
        ast_test_case!{ "!++--!1", 5, make_str_pos!(1, 1, 1, 7),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 7, 1, 7)),
                vec![
                    ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_str_pos!(1, 6, 1, 6)),
                    ExpressionOperator::Unary(SeperatorKind::Decrease, make_str_pos!(1, 4, 1, 5)),
                    ExpressionOperator::Unary(SeperatorKind::Increase, make_str_pos!(1, 2, 1, 3)),
                    ExpressionOperator::Unary(SeperatorKind::LogicalNot, make_str_pos!(1, 1, 1, 1)),
                ],
                make_str_pos!(1, 1, 1, 7),
            )
        }

        // binary operators priority
        //           0        1         2         3         4     
        //           123456789012345678901234567890123456789012345678
        ast_test_case!{ "1 || 2 && 3 == 4 ^ 5 | 6 & 7 >= 8 >> 9 + 10 * 11", 21,  make_str_pos!(1, 1, 1, 48),
            Expression::new_test(
                ExpressionBase::Lit(LitValue::from(1), make_str_pos!(1, 1, 1, 1)),
                vec![
                    ExpressionOperator::Binary(
                        SeperatorKind::LogicalOr,
                        make_str_pos!(1, 3, 1, 4),
                        Expression::new_test(
                            ExpressionBase::Lit(LitValue::from(2), make_str_pos!(1, 6, 1, 6)),
                            vec![
                                ExpressionOperator::Binary(
                                    SeperatorKind::LogicalAnd,
                                    make_str_pos!(1, 8, 1, 9),
                                    Expression::new_test(
                                        ExpressionBase::Lit(LitValue::from(3), make_str_pos!(1, 11, 1, 11)),
                                        vec![
                                            ExpressionOperator::Binary(
                                                SeperatorKind::Equal,
                                                make_str_pos!(1, 13, 1, 14),
                                                Expression::new_test(
                                                    ExpressionBase::Lit(LitValue::from(4), make_str_pos!(1, 16, 1, 16)),
                                                    vec![
                                                        ExpressionOperator::Binary(
                                                            SeperatorKind::BitXor,
                                                            make_str_pos!(1, 18, 1, 18),
                                                            Expression::new_test(
                                                                ExpressionBase::Lit(LitValue::from(5), make_str_pos!(1, 20, 1, 20)),
                                                                vec![
                                                                    ExpressionOperator::Binary(
                                                                        SeperatorKind::BitOr,
                                                                        make_str_pos!(1, 22, 1, 22),
                                                                        Expression::new_test(
                                                                            ExpressionBase::Lit(LitValue::from(6), make_str_pos!(1, 24, 1, 24)),
                                                                            vec![
                                                                                ExpressionOperator::Binary(
                                                                                    SeperatorKind::BitAnd,
                                                                                    make_str_pos!(1, 26, 1, 26),
                                                                                    Expression::new_test(
                                                                                        ExpressionBase::Lit(LitValue::from(7), make_str_pos!(1, 28, 1, 28)),
                                                                                        vec![
                                                                                            ExpressionOperator::Binary(
                                                                                                SeperatorKind::GreatEqual,
                                                                                                make_str_pos!(1, 30, 1, 31),
                                                                                                Expression::new_test(
                                                                                                    ExpressionBase::Lit(LitValue::from(8), make_str_pos!(1, 33, 1, 33)),
                                                                                                    vec![
                                                                                                        ExpressionOperator::Binary(
                                                                                                            SeperatorKind::ShiftRight,
                                                                                                            make_str_pos!(1, 35, 1, 36),
                                                                                                            Expression::new_test(
                                                                                                                ExpressionBase::Lit(LitValue::from(9), make_str_pos!(1, 38, 1, 38)),
                                                                                                                vec![
                                                                                                                    ExpressionOperator::Binary(
                                                                                                                        SeperatorKind::Add,
                                                                                                                        make_str_pos!(1, 40, 1, 40),
                                                                                                                        Expression::new_test(
                                                                                                                            ExpressionBase::Lit(LitValue::from(10), make_str_pos!(1, 42, 1, 43)),
                                                                                                                            vec![
                                                                                                                                ExpressionOperator::Binary(
                                                                                                                                    SeperatorKind::Mul,
                                                                                                                                    make_str_pos!(1, 45, 1, 45),
                                                                                                                                    Expression::new_test(
                                                                                                                                        ExpressionBase::Lit(LitValue::from(11), make_str_pos!(1, 47, 1, 48)),
                                                                                                                                        Vec::new(),
                                                                                                                                        make_str_pos!(1, 47, 1, 48),
                                                                                                                                    ),
                                                                                                                                ),
                                                                                                                            ],
                                                                                                                            make_str_pos!(1, 42, 1, 48),
                                                                                                                        ),
                                                                                                                    ),
                                                                                                                ],
                                                                                                                make_str_pos!(1, 38, 1, 48),
                                                                                                            ),
                                                                                                        ),
                                                                                                    ],
                                                                                                    make_str_pos!(1, 33, 1, 48),
                                                                                                ),
                                                                                            ),
                                                                                        ],
                                                                                        make_str_pos!(1, 28, 1, 48),
                                                                                    ),
                                                                                ),
                                                                            ],
                                                                            make_str_pos!(1, 24, 1, 48),
                                                                        ),
                                                                    ),
                                                                ],
                                                                make_str_pos!(1, 20, 1, 48),
                                                            ),
                                                        ),
                                                    ],
                                                    make_str_pos!(1, 16, 1, 48),
                                                ),
                                            ),
                                        ],
                                        make_str_pos!(1, 11, 1, 48),
                                    ),
                                ),
                            ],
                            make_str_pos!(1, 6, 1, 48),
                        ),
                    ),
                ],
                make_str_pos!(1, 1, 1, 48),
            )
        }
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
                let lexer = &mut Lexer::new(&buf);
                let (result, length) = Expression::parse(lexer, 0);
                perrorln!("Debug: ({:?}, {})", result, length);
                match result {
                    Some(result) => perrorln!("Display: {}, is_function_call: {}", result, result.is_function_call()),
                    None => perrorln!("messages: {:?}", lexer.messages()),
                }
            } else {
                break;
            }
        }
    }
}