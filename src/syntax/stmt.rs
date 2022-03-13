///! fff-lang
///!
///! syntax/stmt
///! stmt = const_decl | var_decl | if_stmt | while_stmt | for_stmt | loop_stmt 
///!        | simple_expr_stmt | assign_expr_stmt | continue_stmt | break_stmt | fn_def | type_def
 
use super::prelude::*;
use super::{FnDef, TypeDef, VarDeclStatement, BreakStatement, ContinueStatement, ReturnStatement, SimpleExprStatement, 
    AssignExprStatement, LoopStatement, WhileStatement, ForStatement, IfStatement, BlockStatement, UseStatement, ImportStatement};

macro_rules! define_statement {
    ($name: ident, $($subty: ty => $enum_name: ident,)+) => (
        #[cfg_attr(test, derive(PartialEq))]
        pub enum $name {
            $($enum_name($subty),)+
        }
        impl ISyntaxFormat for $name {
            fn format(&self, f: Formatter) -> String {
                match self {
                    $(&$name::$enum_name(ref inner) => f.apply(inner).finish(),)+
                }
            }
        }
        impl fmt::Debug for $name {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "\n{}", self.format(Formatter::empty())) }
        }

        $( impl From<$subty> for $name { // impl from to flatten difference between return detail XXXStatement or return Statement
                fn from(s: $subty) -> $name { $name::$enum_name(s) }
        } )+

        impl Node for $name {
            type ParseOutput = $name;
            
            fn matches(current: &Token) -> bool {
                false 
                $(|| <$subty>::matches(current) )+
            }
            fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
                false 
                $(|| <$subty>::matches3(current, peek, peek2) )+
            }

            fn parse<F: FileSystem>(sess: &mut ParseSession<F>) -> ParseResult<$name> {
                $( if sess.matches::<$subty>() {
                    Ok($name::from(<$subty>::parse(sess)?))
                } else )+ {
                    sess.push_unexpect("statement")
                }
            }
        }
    )
}

define_statement!{ Statement,
    TypeDef => Type,
    FnDef => Fn,
    BlockStatement => Block,
    BreakStatement => Break,
    ContinueStatement => Continue,
    SimpleExprStatement => SimpleExpr,
    AssignExprStatement => AssignExpr,
    ForStatement => For,
    IfStatement => If,
    LoopStatement => Loop,
    ReturnStatement => Return,
    VarDeclStatement => VarDecl,
    WhileStatement => While,
    UseStatement => Use,
}

// global item
define_statement!{ Item,
    TypeDef => Type,
    FnDef => Fn,
    BlockStatement => Block,
    SimpleExprStatement => SimpleExpr,
    AssignExprStatement => AssignExpr,
    ForStatement => For,
    IfStatement => If,
    LoopStatement => Loop,
    VarDeclStatement => VarDecl,
    WhileStatement => While,
    UseStatement => Use,
    ImportStatement => Import,
}
 
// hack
impl From<Statement> for Item {
    fn from(s: Statement) -> Item {
        match s {
            Statement::AssignExpr(a) => Item::AssignExpr(a),
            Statement::SimpleExpr(s) => Item::SimpleExpr(s),
            _ => unreachable!(),
        }
    }
}
