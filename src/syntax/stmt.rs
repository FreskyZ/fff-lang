///! fff-lang
///!
///! syntax/stmt
///! stmt = const_decl | var_decl | if_stmt | while_stmt | for_stmt | loop_stmt 
///!        | simple_expr_stmt | assign_expr_stmt | continue_stmt | break_stmt | fn_def | type_def
 
use super::prelude::*;
use super::{FnDef, TypeDef, VarDeclStatement, BreakStatement, ContinueStatement, ReturnStatement, SimpleExprStatement, 
    AssignExprStatement, LoopStatement, WhileStatement, ForStatement, IfStatement, BlockStatement, UseStatement, ModuleStatement};

macro_rules! define_statement {
    ($name:ident, $visit_this:ident, $($subty:ty => $variant:ident, $visit:ident,)+) => (
        #[cfg_attr(test, derive(PartialEq))]
        #[derive(Debug)]
        pub enum $name {
            $($variant($subty),)+
        }

        $( impl From<$subty> for $name { // impl from to flatten difference between return detail XXXStatement or return Statement
                fn from(s: $subty) -> $name { $name::$variant(s) }
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
            
            fn accept<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
                v.$visit_this(self)
            }
            fn walk<T: Default, E, V: Visitor<T, E>>(&self, v: &mut V) -> Result<T, E> {
                match self {
                $(
                    $name::$variant(e) => v.$visit(e),
                )+
                }
            }
        }
    )
}

define_statement!{ Statement, visit_stmt,
    TypeDef => Type, visit_type_def,
    FnDef => Fn, visit_fn_def,
    BlockStatement => Block, visit_block_stmt,
    BreakStatement => Break, visit_break_stmt,
    ContinueStatement => Continue, visit_continue_stmt,
    SimpleExprStatement => SimpleExpr, visit_simple_expr_stmt,
    AssignExprStatement => AssignExpr, visit_assign_expr_stmt,
    ForStatement => For, visit_for_stmt,
    IfStatement => If, visit_if_stmt,
    LoopStatement => Loop, visit_loop_stmt,
    ReturnStatement => Return, visit_ret_stmt,
    VarDeclStatement => VarDecl, visit_var_decl,
    WhileStatement => While, visit_while_stmt,
    UseStatement => Use, visit_use_stmt,
}

// global item
define_statement!{ Item, visit_item,
    TypeDef => Type, visit_type_def,
    FnDef => Fn, visit_fn_def,
    BlockStatement => Block, visit_block_stmt,
    SimpleExprStatement => SimpleExpr, visit_simple_expr_stmt,
    AssignExprStatement => AssignExpr, visit_assign_expr_stmt,
    ForStatement => For, visit_for_stmt,
    IfStatement => If, visit_if_stmt,
    LoopStatement => Loop, visit_loop_stmt,
    VarDeclStatement => VarDecl, visit_var_decl,
    WhileStatement => While, visit_while_stmt,
    UseStatement => Use, visit_use_stmt,
    ModuleStatement => Import, visit_module_stmt,
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
