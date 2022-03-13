///! fff-lang
///!
///! syntax/stmt
///! stmt = const_decl | var_decl | if_stmt | while_stmt | for_stmt | loop_stmt 
///!        | simple_expr_stmt | assign_expr_stmt | continue_stmt | break_stmt | fn_def | type_def
 
use crate::syntax::prelude::*;
use super::{FnDef, TypeDef};

mod block_stmt;
mod expr_stmt;
mod for_stmt;
mod if_stmt;
mod import_stmt;
mod jump_stmt;
mod loop_stmt;
mod ret_stmt;
mod use_stmt;
mod var_decl;
mod while_stmt;
pub use var_decl::VarDeclStatement;
pub use jump_stmt::BreakStatement;
pub use jump_stmt::ContinueStatement;
pub use ret_stmt::ReturnStatement;
pub use expr_stmt::SimpleExprStatement;
pub use expr_stmt::AssignExprStatement;
pub use loop_stmt::LoopStatement;
pub use while_stmt::WhileStatement;
pub use for_stmt::ForStatement;
pub use if_stmt::IfClause;
pub use if_stmt::ElseIfClause;
pub use if_stmt::ElseClause;
pub use if_stmt::IfStatement;
pub use block_stmt::BlockStatement;
pub use use_stmt::UseStatement;
pub use import_stmt::ImportStatement;

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
        impl ISyntaxGrammar for $name {
            fn matches_first(tokens: [&Token; 3]) -> bool {
                false 
                $(|| <$subty as ISyntaxGrammar>::matches_first(tokens) )+
            }
        }

        $( impl From<$subty> for $name { // impl from to flatten difference between return detail XXXStatement or return Statement
                fn from(s: $subty) -> $name { $name::$enum_name(s) }
        } )+

        impl<'ecx, 'scx, F> ISyntaxParse<'ecx, 'scx, F> for $name where F: FileSystem {
            type Output = $name;

            fn parse(sess: &mut ParseSession<'ecx, 'scx, F>) -> ParseResult<$name> {
                $( if <$subty as ISyntaxGrammar>::matches_first(sess.current_tokens()) {
                    return Ok($name::from(<$subty as ISyntaxParse<'ecx, 'scx, F>>::parse(sess)?));
                } else )+ {
                    return sess.push_unexpect("statement");
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
