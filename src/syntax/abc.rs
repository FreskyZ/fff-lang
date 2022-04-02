///! syntax::abc: abstract base class for stmt/item/type_ref, not expr

#[cfg(test)]
macro_rules! make_stmt {
    (for $start:literal:$end:literal for $for_start:literal:$for_end:literal var #$iter_var:literal $iter_var_start:literal:$iter_var_end:literal $iter_expr:expr, $body:expr) => (
        crate::syntax::ast::ForStatement{
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
        crate::syntax::ast::ForStatement{
            loop_name: Some(crate::syntax::ast::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            for_span: Span::new($for_start, $for_end),
            iter_name: IsId::new($iter_var),
            iter_span: Span::new($iter_var_start, $iter_var_end),
            iter_expr: $iter_expr,
            body: $body,
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        crate::syntax::ast::LoopStatement{
            name: None,
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (loop $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal loop $loop_start:literal:$loop_end:literal $body:expr) => (
        crate::syntax::ast::LoopStatement{
            name: Some(crate::syntax::ast::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            body: $body,
            loop_span: Span::new($loop_start, $loop_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        crate::syntax::ast::WhileStatement{
            name: None,
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (while $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal while $while_start:literal:$while_end:literal $expr:expr, $body:expr) => (
        crate::syntax::ast::WhileStatement{
            name: Some(crate::syntax::ast::LabelDef{ name: IsId::new($label), all_span: Span::new($label_start, $label_end) }),
            loop_expr: $expr,
            body: $body,
            while_span: Span::new($while_start, $while_end),
            all_span: Span::new($start, $end),
        }
    );
    (break $start:literal:$end:literal) => (
        crate::syntax::ast::BreakStatement(crate::syntax::ast::JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (break $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        crate::syntax::ast::BreakStatement(crate::syntax::ast::JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal) => (
        crate::syntax::ast::ContinueStatement(crate::syntax::ast::JumpStatement{
            target: None,
            all_span: Span::new($start, $end),
        })
    );
    (continue $start:literal:$end:literal label #$label:literal $label_start:literal:$label_end:literal) => (
        crate::syntax::ast::ContinueStatement(crate::syntax::ast::JumpStatement{
            target: Some((IsId::new($label), Span::new($label_start, $label_end))),
            all_span: Span::new($start, $end),
        })
    );
    (var $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        crate::syntax::ast::VarDeclStatement{
            is_const: false,
            name: IsId::new($name),
            name_span: Span::new($name_start, $name_end),
            r#type: $type,
            init_expr: $init,
            all_span: Span::new($start, $end),
        }
    );
    (const $start:literal:$end:literal #$name:literal $name_start:literal:$name_end:literal $type:expr, $init:expr) => (
        crate::syntax::ast::VarDeclStatement{
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
