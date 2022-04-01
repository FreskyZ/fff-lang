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
    EnumDef => Enum, visit_enum_def,
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
    EnumDef => Enum, visit_enum_def,
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

define_abc!{ TypeRef, "type ref", visit_type_ref,
    PrimitiveType => Primitive, visit_primitive_type,
    ArrayType => Array, visit_array_type,
    FnType => Fn, visit_fn_type,
    RefType => Ref, visit_ref_type,
    TupleType => Tuple, visit_tuple_type,
    PlainType => Plain, visit_plain_type,
}

impl TypeRef {
    pub fn get_all_span(&self) -> Span {
        match self {
            Self::Primitive(t) => t.span,
            Self::Array(t) => t.span,
            Self::Fn(t) => t.all_span,
            Self::Ref(t) => t.span,
            Self::Tuple(t) => t.span,
            Self::Plain(t) => t.all_span,
        }
    }
}

#[cfg(test)]
macro_rules! make_stmt {
    (for $start:literal:$end:literal for $for_start:literal:$for_end:literal var #$iter_var:literal $iter_var_start:literal:$iter_var_end:literal $iter_expr:expr, $body:expr) => (
        crate::syntax::ForStatement{
            loop_name: None,
            for_span: Span::new($for_start, $for_end),
            iter_name: IsId::new($iter_var),
            iter_span: Span::new($iter_var_start, $iter_var_end),
            iter_expr: $iter_expr,
            body: $body,
            all_span: Span::new($start, $end),
        }
    );
    (for $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal for $for_start:literal:$for_end:literal var #$iter_var:literal $iter_var_start:literal:$iter_var_end:literal $iter_expr:expr, $body:expr) => (
        crate::syntax::ForStatement{
            loop_name: Some(crate::syntax::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            for_span: Span::new($for_start, $for_end),
            iter_name: IsId::new($iter_var),
            iter_span: Span::new($iter_var_start, $iter_var_end),
            iter_expr: $iter_expr,
            body: $body,
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        crate::syntax::LoopStatement{
            name: None,
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        crate::syntax::LoopStatement{
            name: Some(crate::syntax::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        crate::syntax::WhileStatement{
            name: None,
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        crate::syntax::WhileStatement{
            name: Some(crate::syntax::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (break $start:literal:$end:literal) => (
        crate::syntax::BreakStatement(crate::syntax::JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (break $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        crate::syntax::BreakStatement(crate::syntax::JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal) => (
        crate::syntax::ContinueStatement(crate::syntax::JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        crate::syntax::ContinueStatement(crate::syntax::JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (var $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        crate::syntax::VarDeclStatement{
            is_const: false,
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            r#type: $type,
            init_expr: $init,
            all_span: Span::new($start, $end),
        }
    );
    (const $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        crate::syntax::VarDeclStatement{
            is_const: true,
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            r#type: $type,
            init_expr: $init,
            all_span: Span::new($start, $end),
        }
    );
}
#[cfg(test)]
pub(crate) use make_stmt;
