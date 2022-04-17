///! ast memory usage profiler

use std::collections::HashMap;
use super::super::visit::{Visitor, Node};
use super::*;

#[derive(Default)]
struct MemoryUsage {
    count: usize,
    // in bytes, same as result of mem::size_of
    // for normal node,
    //   size is size of all owned fields, which contains
    //     all "terminal" fields (include IsId, Span, IdSpan, Separator, Keyword, LitValue)
    //     and box and vec's shallow size (sizeof!(Box<T>) (or 1 size_t) and sizeof!(Vec<T>>) (or 3 size_t))
    //     and option tag (e.g. sizeof!(Option<Expr>) - sizeof!(Expr)), because rust is not smart/aggresive enough to fold union tags
    //     without directly owned node (e.g. ForStatement::condition)
    // for abc node,
    //   size is tag + pad for largest variant, which is sizeof!(Self) - sizeof!(CurrentVariant)
    total_size: usize,
    // in bytes, same as result of mem::size_of
    // for normal node,
    //   theoretical size is size of all owned fields, with help of unified object pool, which contains
    //     all "terminal" fields
    //     for Box and directly owned node, u32 for object index
    //     for Vec, u32 for vec length and u32 for each item's object index
    //     for Option, plus u32 for option tag
    // for abc node,
    //   theoretical size is u32, avoid this padding is one of the major targets of unified object pool
    total_theoretical_size: usize,
}

#[allow(dead_code)] // size of parameter to make semantic more clear compare to ssizeof(u32)
struct ObjectIndex(u32);
#[allow(dead_code)]
struct OptionTag(u32);

macro_rules! sizeof {
    ($ty:ty) => (std::mem::size_of::<$ty>());
}

impl MemoryUsage {

    fn count_impl(&mut self, size: usize, theoretical_size: usize) -> &mut Self {
        self.count += 1;
        self.total_size += size;
        self.total_theoretical_size += theoretical_size;
        self
    }

    // count terminal, used frequently so name is short
    fn count_t<T>(&mut self, _: &T) -> &mut Self {
        self.count_impl(sizeof!(T), sizeof!(T))
    }
    // label occupies Option<IdSpan> (2 size_t), and will become IsId+Span (1.5 size_t) theoretically
    fn count_label(&mut self, _: &Option<IdSpan>) -> &mut Self {
        self.count_impl(sizeof!(Option<IdSpan>), sizeof!(IsId) + sizeof!(Span))
    }
    fn count_box<T>(&mut self, _: &Box<T>) -> &mut Self {
        self.count_impl(sizeof!(Box<T>), sizeof!(ObjectIndex))
    }
    fn count_vec<T>(&mut self, v: &Vec<T>) -> &mut Self {
        self.count_impl(sizeof!(Vec<T>), sizeof!(ObjectIndex) + sizeof!(ObjectIndex) * v.len())
    }
    fn count_option<T>(&mut self, _: &Option<T>) -> &mut Self {
        self.count_impl(sizeof!(Option<T>) - sizeof!(T), sizeof!(OptionTag))
    }
    fn count_option_box<T>(&mut self, _: &Option<Box<T>>) -> &mut Self {
        self.count_impl(sizeof!(Option<Box<T>>), sizeof!(ObjectIndex) + sizeof!(OptionTag))
    }
    // count directly owned node
    fn count_owned<N: Node>(&mut self, _: &N) -> &mut Self {
        self.count_impl(0, sizeof!(ObjectIndex))
    }
}

pub struct MemoryProfiler {
    // key is node name
    //   for normal node, key is node name
    //   for abc node, will use key for each variant (e.g. expr (array-expr), expr (tuple-expr))
    u: HashMap<&'static str, MemoryUsage>,
}

impl MemoryProfiler {

    pub fn new() -> Self {
        Self{ u: HashMap::new() }
    }

    pub fn profile<N: Node>(&mut self, node: &N) {
        let _ = node.accept(self);
    }

    pub fn dump(&self) {
        let total_total_size: usize = self.u.values().map(|v| v.total_size).sum();
        let total_total_theoretical_size: usize = self.u.values().map(|v| v.total_theoretical_size).sum();
        let mut ordered_items = self.u.iter().collect::<Vec<_>>();
        ordered_items.sort_by_key(|i| i.1.total_size);
        ordered_items.reverse();
        println!("total size {total_total_size} > {total_total_theoretical_size}");
        for (&key, MemoryUsage{ count, total_size, total_theoretical_size }) in ordered_items {
            let header = format!("[{key}({count})]");
            let total_size_percent = *total_size as f64 * 100f64 / total_total_size as f64;
            let total_theoretical_size_percent = *total_theoretical_size as f64 * 100f64 / total_total_theoretical_size as f64;
            println!("{header:>28} {total_size} ({total_size_percent:.2}%) > {total_theoretical_size} ({total_theoretical_size_percent:.2}%)");
        }
    }
}

macro_rules! impl_visit {
    ($ty:ty, $fn:ident, $name:literal $(,)? $($countf:ident(.$field:ident),)+) => (
        fn $fn(&mut self, node: &$ty) -> Result<(), ()> {
            self.u.entry($name).or_default()$(.$countf(&node.$field))+;
            node.walk(self)
        }
    );
    (enum $ty:ty, $fn:ident, $name:literal $(,)? $($variant:ident($subty:ty), $subname:literal,)+) => (
        fn $fn(&mut self, node: &$ty) -> Result<(), ()> {
            use $ty::*;
            match node {
            $(
                $variant(_) => self.u.entry(concat!($name, "(", $subname, ")"))
                    .or_default().count_impl(sizeof!($ty), sizeof!(OptionTag) + sizeof!(ObjectIndex)),
            )+
            };
            node.walk(self)
        }
    );
}

impl Visitor for MemoryProfiler {

    impl_visit!{ ArrayExpr, visit_array_expr, "array-expr",
        count_t(.span), count_vec(.items),
    }
    impl_visit!{ ArrayIndexExpr, visit_array_index_expr, "array-index-expr",
        count_t(.span), count_box(.base), count_vec(.parameters), count_t(.quote_span),
    }
    impl_visit!{ ArrayType, visit_array_type, "array-type",
        count_t(.span), count_box(.base), count_owned(.size),
    }
    impl_visit!{ AssignExprStatement, visit_assign_expr_stmt, "assign-expr-stmt",
        count_t(.span), count_owned(.left), count_owned(.right), count_t(.op), count_t(.op_span),
    }
    impl_visit!{ BinaryExpr, visit_binary_expr, "binary-expr",
        count_t(.span), count_box(.left), count_box(.right), count_t(.op), count_t(.op_span),
    }
    impl_visit!{ Block, visit_block, "block",
        count_t(.span), count_vec(.items),
    }
    impl_visit!{ BlockStatement, visit_block_stmt, "block-stmt",
        count_t(.span), count_label(.label), count_owned(.body),
    }
    impl_visit!{ BreakStatement, visit_break_stmt, "break-stmt",
        count_t(.span), count_label(.label),
    }
    impl_visit!{ CallExpr, visit_call_expr, "call-expr",
        count_t(.span), count_box(.base), count_vec(.parameters), count_t(.quote_span),
    }
    impl_visit!{ ClassDef, visit_class_def, "class-def",
        count_t(.span), count_owned(.name), count_t(.quote_span), count_vec(.types), count_vec(.functions),
    }
    impl_visit!{ ContinueStatement, visit_continue_stmt, "continue-stmt",
        count_t(.span), count_label(.label),
    }
    impl_visit!{ ElseClause, visit_else_clause, "else-clause",
        count_t(.span), count_owned(.body),
    }
    impl_visit!{ EnumDef, visit_enum_def, "enum-def",
        count_t(.span), count_t(.name), count_option(.base_type), count_t(.quote_span), count_vec(.variants),
    }
    impl_visit!{ EnumDefVariant, visit_enum_def_variant, "enum-def-variant",
        count_t(.span), count_t(.name), count_option(.value),
    }
    impl_visit!{ enum Expr, visit_expr, "expr",
        Lit(LitExpr), "lit",
        Path(super::Path), "path",
        Paren(ParenExpr), "paren",
        Tuple(TupleExpr), "tuple",
        Array(ArrayExpr), "array",
        Call(CallExpr), "call",
        ArrayIndex(ArrayIndexExpr), "array-index",
        TupleIndex(TupleIndexExpr), "tuple-index",
        Member(MemberExpr), "member",
        Object(ObjectExpr), "object",
        Unary(UnaryExpr), "unary",
        Binary(BinaryExpr), "binary",
        RangeBoth(RangeBothExpr), "range-both",
        RangeFull(RangeFullExpr), "range-full",
        RangeLeft(RangeLeftExpr), "range-left",
        RangeRight(RangeRightExpr), "range-right",
    }
    impl_visit!{ FieldDef, visit_field_def, "field-def",
        count_t(.span), count_t(.name), count_t(.colon_span), count_owned(.r#type),
    }
    impl_visit!{ FnDef, visit_fn_def, "fn-def",
        count_t(.span), count_owned(.name), count_t(.span), count_vec(.parameters), count_option(.ret_type), count_vec(.wheres), count_option(.body),
    }
    impl_visit!{ FnDefParameter, visit_fn_def_parameter, "fn-def-parameter",
        count_t(.span), count_t(.name), count_owned(.r#type),
    }
    impl_visit!{ FnType, visit_fn_type, "fn-type",
        count_t(.span), count_t(.quote_span), count_vec(.parameters), count_option_box(.ret_type),
    }
    impl_visit!{ FnTypeParameter, visit_fn_type_parameter, "fn-type-parameter",
        count_t(.span), count_t(.name), count_owned(.r#type),
    }
    impl_visit!{ ForStatement, visit_for_stmt, "for-stmt",
        count_t(.span), count_label(.label), count_t(.iter_name), count_owned(.iter_expr), count_owned(.body),
    }
    impl_visit!{ GenericName, visit_generic_name, "generic-name",
        count_t(.span), count_t(.base), count_t(.quote_span), count_vec(.parameters),
    }
    impl_visit!{ GenericParameter, visit_generic_parameter, "generic-parameter",
        count_t(.span), count_t(.name),
    }
    impl_visit!{ IfClause, visit_if_clause, "if-clause",
        count_t(.span), count_owned(.condition), count_owned(.body),
    }
    impl_visit!{ IfStatement, visit_if_stmt, "if-stmt",
        count_t(.span), count_owned(.if_clause), count_vec(.elseif_clauses), count_option(.else_clause),
    }
    impl_visit!{ Implementation, visit_impl, "impl-block",
        count_t(.span), count_vec(.parameters), count_option(.class), count_owned(.r#type), count_vec(.wheres), count_t(.quote_span), count_vec(.types), count_vec(.functions),
    }
    impl_visit!{ enum Item, visit_item, "item",
        Struct(StructDef), "struct",
        Enum(EnumDef), "enum",
        Fn(FnDef), "fn",
        Impl(Implementation), "impl",
        Type(TypeDef), "type",
        Class(ClassDef), "class",
        Block(BlockStatement), "block",
        SimpleExpr(SimpleExprStatement), "simple-expr",
        AssignExpr(AssignExprStatement), "assign-expr",
        For(ForStatement), "for",
        If(IfStatement), "if",
        Loop(LoopStatement), "loop",
        VarDecl(VarDeclStatement), "var-decl",
        While(WhileStatement), "while",
        Use(UseStatement), "use",
        Import(ModuleStatement), "import",
    }
    impl_visit!{ LitExpr, visit_lit_expr, "lit-expr",
        count_t(.span), count_t(.value),
    }
    impl_visit!{ LoopStatement, visit_loop_stmt, "loop-stmt",
        count_t(.span), count_label(.label), count_owned(.body),
    }
    impl_visit!{ MemberExpr, visit_member_expr, "member-expr",
        count_t(.span), count_box(.base), count_t(.op_span), count_t(.name), count_option(.parameters),
    }
    // impl_visit!{ Module }
    impl_visit!{ ModuleStatement, visit_module_stmt, "module-stmt",
        count_t(.span), count_t(.name), count_t(.path),
    }
    impl_visit!{ ObjectExpr, visit_object_expr, "object-expr",
        count_t(.span), count_box(.base), count_t(.span), count_vec(.fields),
    }
    impl_visit!{ ObjectExprField, visit_object_expr_field, "object-expr-field",
        count_t(.span), count_t(.name), count_owned(.value),
    }
    impl_visit!{ ParenExpr, visit_paren_expr, "paren-expr",
        count_t(.span), count_box(.base),
    }
    impl_visit!{ Path, visit_path, "path",
        count_t(.span), count_vec(.segments),
    }

    // this is combination of abc and normal
    fn visit_path_segment(&mut self, node: &PathSegment) -> Result<(), ()> {
        match node {
            PathSegment::Global => {
                self.u.entry("path-segment-global").or_default().count_impl(0, 0);
                self.u.entry("path-segment(global)").or_default().count_impl(sizeof!(PathSegment), sizeof!(OptionTag));
            },
            PathSegment::Simple(id) => {
                self.u.entry("path-segment-simple").or_default().count_t(id);
                self.u.entry("path-segment(simple)").or_default().count_impl(sizeof!(PathSegment), sizeof!(OptionTag));
            },
            PathSegment::TypeCast{ span, left, right } => {
                self.u.entry("path-segment-cast").or_default().count_t(span).count_owned(left).count_owned(right);
                self.u.entry("path-segment(cast)").or_default()
                    .count_impl(sizeof!(PathSegment), sizeof!(OptionTag));
            },
            PathSegment::Generic{ span, base, parameters } => {
                self.u.entry("path-segment-generic").or_default().count_t(span).count_t(base).count_owned(parameters);
                self.u.entry("path-segment(generic)").or_default()
                    .count_impl(sizeof!(PathSegment), sizeof!(OptionTag));
            },
        }
        node.walk(self)
    }

    impl_visit!{ PrimitiveType, visit_primitive_type, "primitive-type",
        count_t(.span), count_t(.base),
    }
    impl_visit!{ RangeBothExpr, visit_range_both_expr, "range-both-expr",
        count_t(.span), count_box(.left), count_t(.op_span), count_box(.right),
    }
    impl_visit!{ RangeFullExpr, visit_range_full_expr, "range-full-expr",
        count_t(.span),
    }
    impl_visit!{ RangeLeftExpr, visit_range_left_expr, "range-left-expr",
        count_t(.span), count_box(.base),
    }
    impl_visit!{ RangeRightExpr, visit_range_right_expr, "range-right-expr",
        count_t(.span), count_box(.base),
    }
    impl_visit!{ RefType, visit_ref_type, "ref-type"
        count_t(.span), count_box(.base),
    }
    impl_visit!{ ReturnStatement, visit_ret_stmt, "ret-stmt"
        count_t(.span), count_option(.value),
    }
    impl_visit!{ SimpleExprStatement, visit_simple_expr_stmt, "simple-expr-stmt"
        count_t(.span), count_owned(.expr),
    }
    impl_visit!{ enum Statement, visit_stmt, "stmt" 
        Struct(StructDef), "struct",
        Enum(EnumDef), "enum",
        Fn(FnDef), "fn",
        Impl(Implementation), "impl",
        Type(TypeDef), "type",
        Class(ClassDef), "class",
        Block(BlockStatement), "block",
        Break(BreakStatement), "break",
        Continue(ContinueStatement), "continue",
        SimpleExpr(SimpleExprStatement), "simple-expr",
        AssignExpr(AssignExprStatement), "assign-expr",
        For(ForStatement), "for",
        If(IfStatement), "if",
        Loop(LoopStatement), "loop",
        Return(ReturnStatement), "ret",
        VarDecl(VarDeclStatement), "var-decl",
        While(WhileStatement), "while",
        Use(UseStatement), "use",
    }
    impl_visit!{ StructDef, visit_struct_def, "struct-def"
        count_t(.span), count_owned(.name), count_vec(.fields),
    }
    impl_visit!{ TupleExpr, visit_tuple_expr, "tuple-expr"
        count_t(.span), count_vec(.items),
    }
    impl_visit!{ TupleIndexExpr, visit_tuple_index_expr, "tuple-index-expr"
        count_t(.span), count_box(.base), count_t(.op_span), count_t(.value),
    }
    impl_visit!{ TupleType, visit_tuple_type, "tuple-type"
        count_t(.span), count_vec(.parameters),
    }
    impl_visit!{ TypeDef, visit_type_def, "type-def",
        count_t(.span), count_owned(.name), count_option(.from),
    }
    impl_visit!{ TypeList, visit_type_list, "type-list"
        count_t(.span), count_vec(.items),
    }
    impl_visit!{ enum TypeRef, visit_type_ref, "type-ref"
        Array(ArrayType), "array",
        Fn(FnType), "fn",
        Path(super::Path), "path",
        Primitive(PrimitiveType), "primitive",
        Ref(RefType), "ref",
        Tuple(TupleType), "tuple",
    }
    impl_visit!{ UnaryExpr, visit_unary_expr, "unary-expr"
        count_t(.span), count_box(.base), count_t(.op), count_t(.op_span),
    }
    impl_visit!{ UseStatement, visit_use_stmt, "use-stmt"
        count_t(.span), count_owned(.path), count_t(.alias),
    }
    impl_visit!{ VarDeclStatement, visit_var_decl, "var-decl"
        count_t(.span), count_t(.r#const), count_t(.name), count_option(.r#type), count_option(.init_value),
    }
    impl_visit!{ WhereClause, visit_where_clause, "where-clause"
        count_t(.span), count_t(.name), count_vec(.constraints),
    }
    impl_visit!{ WhileStatement, visit_while_stmt, "while-stmt"
        count_t(.span), count_label(.label), count_owned(.condition), count_owned(.body),
    }
}
