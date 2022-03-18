///! syntax::abc: abstract base class for stmt/item/type_ref, not expr
 
use super::prelude::*;
use super::*;

// all variants has same priority
// nearly all variants are N == <N as Node>::ParseOutput
// nearly all variants have different first terminal symbol
// exception is simple-expr-stmt and assign-expr-stmt, simple-expr-stmt forwards all implementation to assign-expr-stmt
//
// expr is not here because expressions has complex priority, and a lot of N != <N as Node>::ParseOutput for that

macro_rules! define_abc {
    ($name:ident, $desc:literal, $visit_this:ident, $($subty:ty => $variant:ident, $visit:ident,)+) => (
#[cfg_attr(test, derive(PartialEq))]
#[derive(Debug)]
pub enum $name {
    $($variant($subty),)+
}

$( impl From<$subty> for $name {
    fn from(s: $subty) -> $name { $name::$variant(s) }
} )+

impl Parser for $name {
    type Output = $name;
    
    fn matches(current: &Token) -> bool {
        false 
        $(|| <$subty>::matches(current) )+
    }
    fn matches3(current: &Token, peek: &Token, peek2: &Token) -> bool {
        false 
        $(|| <$subty>::matches3(current, peek, peek2) )+
    }

    fn parse(cx: &mut ParseContext) -> Result<$name, Unexpected> {
        $( if cx.matches::<$subty>() {
            Ok($name::from(cx.expect::<$subty>()?))
        } else )+ {
            cx.push_unexpect($desc)
        }
    }
}

impl Node for $name {
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
    );
}

define_abc!{ Statement, "statement", visit_stmt,
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
define_abc!{ Item, "item", visit_item, 
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

// assign-expr-stmt parse output is statement (to include simple-expr-stmt)
// so need to allow to convert to item for Item::parse
impl From<Statement> for Item {
    fn from(s: Statement) -> Item {
        match s {
            Statement::AssignExpr(a) => Item::AssignExpr(a),
            Statement::SimpleExpr(s) => Item::SimpleExpr(s),
            _ => unreachable!(),
        }
    }
}
