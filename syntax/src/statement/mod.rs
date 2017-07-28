///! fff-lang
///!
///! syntax/stmt
///! stmt = const_decl | var_decl | if_stmt | while_stmt | for_stmt | loop_stmt 
///!        | simple_expr_stmt | assign_expr_stmt | continue_stmt | break_stmt | fn_def | type_def
 
use std::fmt;

use lexical::Token;

use super::FnDef;
use super::TypeDef;
use super::Formatter;
use super::ParseResult;
use super::ParseSession;
use super::ISyntaxParse;
use super::ISyntaxFormat;
use super::ISyntaxGrammar;

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

pub use self::var_decl::VarDeclStatement;
pub use self::jump_stmt::BreakStatement;
pub use self::jump_stmt::ContinueStatement;
pub use self::ret_stmt::ReturnStatement;
pub use self::expr_stmt::SimpleExprStatement;
pub use self::expr_stmt::AssignExprStatement;
pub use self::loop_stmt::LoopStatement;
pub use self::while_stmt::WhileStatement;
pub use self::for_stmt::ForStatement;
pub use self::if_stmt::IfClause;
pub use self::if_stmt::ElseIfClause;
pub use self::if_stmt::ElseClause;
pub use self::if_stmt::IfStatement;
pub use self::block_stmt::BlockStatement;
pub use self::use_stmt::UseStatement;
pub use self::import_stmt::ImportStatement;

macro_rules! define_statement {
    ($name: ident, $($subty: ty => $enum_name: ident,)+) => (
        #[cfg_attr(test, derive(Eq, PartialEq))]
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
            fn matches_first(tokens: &[&Token]) -> bool {
                false 
                $(|| <$subty as ISyntaxGrammar>::matches_first(tokens) )+
            }
        }

        $( impl From<$subty> for $name { // impl from to flatten difference between return detail XXXStatement or return Statement
                fn from(s: $subty) -> $name { $name::$enum_name(s) }
        } )+

        impl ISyntaxParse for $name {
            type Output = $name;

            fn parse(sess: &mut ParseSession) -> ParseResult<$name> {
                $( if <$subty as ISyntaxGrammar>::matches_first(sess.current_tokens()) {
                    return Ok($name::from(<$subty as ISyntaxParse>::parse(sess)?));
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
#[cfg(remove_this_after_stupid_racer_and_rls_can_identify_type_defined_in_macro)]
pub enum Statement {
    Type(TypeDef),
    Fn(FnDef),
    Block(BlockStatement),
    Break(BreakStatement),
    Continue(ContinueStatement),
    SimpleExpr(SimpleExprStatement),
    AssignExpr(AssignExprStatement),
    For(ForStatement),
    If(IfStatement),
    Loop(LoopStatement),
    Return(ReturnStatement),
    VarDecl(VarDeclStatement),
    While(WhileStatement),
    Use(UseStatement),
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
#[cfg(remove_this_after_stupid_racer_and_rls_can_identify_type_defined_in_macro)]
pub enum Item {
    Type(TypeDef),
    Fn(FnDef),
    Block(BlockStatement),
    SimpleExpr(SimpleExprStatement),
    AssignExpr(AssignExprStatement),
    For(ForStatement),
    If(IfStatement),
    Loop(LoopStatement),
    VarDecl(VarDeclStatement),
    While(WhileStatement),
    Use(UseStatement),
    Import(ImportStatement),
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
