
// ConstDecl = fConst Type fIdentifier [fAssign Expression] fSemiColon
// VarDecl = fVar Type fIdentifier [fAssign Expression] fSemiColon

use std::fmt;

use common::From2;
use common::StringPosition;

use lexical::Lexer;
use lexical::IToken;
use lexical::KeywordKind;
use lexical::SeperatorKind;

use syntax::ast_item::IASTItem;
use syntax::Expression;
use syntax::SMType;

#[derive(Eq, PartialEq)]
pub struct VarDeclStatement {
    pub id: usize,
    pub is_const: bool,
    pub ty: SMType,
    pub name: String,
    pub init_expr: Option<Expression>,
    pub pos: [StringPosition; 4],     // position for 'const' or 'var', name, assign, and semicolon
}

impl fmt::Debug for VarDeclStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{} @ {:?} {:?} {} @ {:?}{}; @ {:?}",
            self.id,
            if self.is_const { format!("const") } else { format!("var") },
            self.pos[0],
            self.ty,
            self.name,
            self.pos[1],
            match self.init_expr { Some(ref expr) => format!(" = @ {:?} {:?}", self.pos[2], expr), None => String::new(), },
            self.pos[3],
        )
    }
}
impl fmt::Display for VarDeclStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}>{} {} {}{};",
            self.id,
            if self.is_const { format!("const") } else { format!("var") },
            self.ty,
            self.name,
            match self.init_expr { Some(ref expr) => format!(" = {}", expr), None => String::new(), },
        )
    }
}

impl IASTItem for VarDeclStatement {

    fn pos_all(&self) -> StringPosition { StringPosition::from2(self.pos[0].start_pos, self.pos[2].end_pos) }

    /// It is special that the given index is index of 'const' or 'var' not the next
    fn parse(lexer: &mut Lexer, index: usize) -> (Option<VarDeclStatement>, usize) {

        let is_const = match lexer.nth(index).get_keyword() {
            Some(&KeywordKind::Const) => true, 
            Some(&KeywordKind::Var) => false,
            _ => unreachable!(), 
        };
        let mut current_len = 1;
        let mut poss = [StringPosition::new(); 4];
        poss[0] = lexer.pos(index);

        let ty = match SMType::parse(lexer, index + current_len) {
            (None, length) => return (None, length),
            (Some(ty), ty_len) => {
                current_len += ty_len;
                ty
            } 
        };

        let (name, name_pos) = match lexer.nth(index + current_len).get_identifier() {
            Some(name) => {
                current_len += 1;
                (name.clone(), lexer.pos(index + current_len - 1))
            } 
            None => return lexer.push_expect("identifier", index + current_len, current_len),
        };
        poss[1] = name_pos;

        match lexer.nth(index + current_len).get_seperator() {
            Some(&SeperatorKind::SemiColon) => {
                poss[3] = lexer.pos(index + current_len);
                current_len += 1;
                return (Some(VarDeclStatement{ 
                    id: 0, 
                    is_const: is_const, 
                    ty: ty, 
                    name: name, 
                    init_expr: None,
                    pos: poss,
                }), current_len);
            },
            Some(&SeperatorKind::Assign) => {
                poss[2] = lexer.pos(index + current_len);
                current_len += 1;
                match Expression::parse(lexer, index + current_len) {
                    (None, length) => return (None, current_len + length),
                    (Some(expr), expr_len) => {
                        current_len += expr_len;
                        if lexer.nth(index + current_len).is_seperator(SeperatorKind::SemiColon) {
                            poss[3] = lexer.pos(index + current_len);
                            current_len += 1;
                            return (Some(VarDeclStatement{
                                id: 0,
                                is_const: is_const,
                                ty: ty,
                                name: name,
                                init_expr: Some(expr),
                                pos: poss,
                            }), current_len);
                        } else {
                            return lexer.push_expect("semicolon", index + current_len, current_len);
                        }
                    }
                }
            }
            _ => return lexer.push_expects(vec!["assignment and initial expr", "semicolon"], index + current_len, current_len),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::VarDeclStatement;
    use lexical::Lexer;
    use syntax::ast_item::IASTItem;
    use syntax::SMType;
    use syntax::SMTypeBase;
    use syntax::Expression;
    use syntax::ExpressionBase;
    use syntax::ExpressionOperator;
    use common::StringPosition;
    use lexical::NumericLiteralValue;
    use lexical::SeperatorKind;

    #[test]
    fn ast_stmt_var_decl() {
        
        //                           123456789012345678
        let lexer = &mut Lexer::new("const i32 abc = 0;".to_owned());
        assert_eq!(
            VarDeclStatement::parse(lexer, 0),
            (Some(VarDeclStatement {
                id: 0,
                is_const: true,
                ty: SMType::make_base(SMTypeBase::I32, make_str_pos!(1, 7, 1, 9)),
                name: "abc".to_owned(),
                init_expr: Some(Expression::new_test(
                    ExpressionBase::NumericLiteral(NumericLiteralValue::I32(0)), 
                    make_str_pos!(1, 17, 1, 17),
                    Vec::new(),
                    make_str_pos!(1, 17, 1, 17)
                )),
                pos: [
                    make_str_pos!(1, 1, 1, 5),
                    make_str_pos!(1, 11, 1, 13),
                    make_str_pos!(1, 15, 1, 15),
                    make_str_pos!(1, 18, 1, 18),
                ],
            }), 6)
        );
        //                           0        1         2
        //                           1234567890123456789012
        let lexer = &mut Lexer::new("var [i32] abc = 1 + 1;".to_owned());
        assert_eq!(
            VarDeclStatement::parse(lexer, 0),
            (Some(VarDeclStatement {
                id: 0,
                is_const: false,
                ty: SMType::make_array(SMType::make_base(SMTypeBase::I32, make_str_pos!(1, 6, 1, 8)), make_str_pos!(1, 5, 1, 9)),
                name: "abc".to_owned(),
                init_expr: Some(Expression::new_test(
                    ExpressionBase::NumericLiteral(NumericLiteralValue::I32(1)), 
                    make_str_pos!(1, 17, 1, 17),
                    vec![
                        ExpressionOperator::Binary(
                            SeperatorKind::Add,
                            make_str_pos!(1, 19, 1, 19),
                            Expression::new_test(
                                ExpressionBase::NumericLiteral(NumericLiteralValue::I32(1)),
                                make_str_pos!(1, 21, 1, 21),
                                Vec::new(),
                                make_str_pos!(1, 21, 1, 21),
                            ),
                        ),
                    ],
                    make_str_pos!(1, 17, 1, 21)
                )),
                pos: [
                    make_str_pos!(1, 1, 1, 3),
                    make_str_pos!(1, 11, 1, 13),
                    make_str_pos!(1, 15, 1, 15),
                    make_str_pos!(1, 22, 1, 22),
                ],
            }), 10)
        );
        
        //                           1234567890123456789
        let lexer = &mut Lexer::new("const string input;".to_owned());
        assert_eq!(
            VarDeclStatement::parse(lexer, 0),
            (Some(VarDeclStatement {
                id: 0,
                is_const: true,
                ty: SMType::make_base(SMTypeBase::SMString, make_str_pos!(1, 7, 1, 12)),
                name: "input".to_owned(),
                init_expr: None,
                pos: [
                    make_str_pos!(1, 1, 1, 5),
                    make_str_pos!(1, 14, 1, 18),
                    StringPosition::new(), 
                    make_str_pos!(1, 19, 1, 19),
                ],
            }), 4)
        );
        
        //                           1234567890123
        let lexer = &mut Lexer::new("var [u8] buf;".to_owned());
        assert_eq!(
            VarDeclStatement::parse(lexer, 0),
            (Some(VarDeclStatement {
                id: 0,
                is_const: false,
                ty: SMType::make_array(SMType::make_base(SMTypeBase::U8, make_str_pos!(1, 6, 1, 7)), make_str_pos!(1, 5, 1, 8)),
                name: "buf".to_owned(),
                init_expr: None,
                pos: [
                    make_str_pos!(1, 1, 1, 3),
                    make_str_pos!(1, 10, 1, 12),
                    StringPosition::new(), 
                    make_str_pos!(1, 13, 1, 13),
                ],
            }), 6)
        );
    }
}