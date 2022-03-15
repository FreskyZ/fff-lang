///! syntax::display: default display implementation

use std::fmt::{self, Write};
use crate::source::{SourceContext, FileSystem, Span, IsId};
use super::node::Visitor;
use super::super::*;

static INDENTIONS: [[&str; 16]; 3] = [
    [
        "", "1 ", "2 | ", "3 | | ", "4 | | | ", "5 | | | | ", "6 | | | | | ", "7 | | | | | | ", "8 | | | | | | | ", "9 | | | | | | | | ", "10| | | | | | | | | ",
        "11| | | | | | | | | | ", "12| | | | | | | | | | | ", "13| | | | | | | | | | | | ", "14| | | | | | | | | | | | | ", "15| | | | | | | | | | | | "
    ], [
        "", "| ", "| | ", "| | | ", "| | | | ", "| | | | | ", "| | | | | | ", "| | | | | | | ", "| | | | | | | | ", "| | | | | | | | | ", "| | | | | | | | | | ",
        "| | | | | | | | | | | ", "| | | | | | | | | | | | ", "| | | | | | | | | | | | | ", "| | | | | | | | | | | | | | ", "| | | | | | | | | | | | | "
    ], [
        "", "  ", "    ", "      ", "        ", "          ", "            ", "              ", "                ", "                  ", "                    ",
        "                      ", "                        ", "                          ", "                            ", "                          "
    ]
];

pub struct FormatVisitor<'scx, 'f1, 'f2, F> {
    level: usize,
    scx: &'scx SourceContext<F>,
    f: &'f1 mut fmt::Formatter<'f2>,
}

impl<'scx, 'f1, 'f2, F> FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    pub fn new(scx: &'scx SourceContext<F>, f: &'f1 mut fmt::Formatter<'f2>) -> Self {
        Self{ level: 0, scx, f }
    }

    fn write_str(&mut self, v: &str) -> fmt::Result {
        self.f.write_str(v)
    }
    fn write_space(&mut self) -> fmt::Result {
        self.f.write_char(' ')
    }

    fn write_indent(&mut self) -> fmt::Result { 
        self.f.write_str(INDENTIONS[2][self.level])
    }

    fn write_span(&mut self, span: Span) -> fmt::Result {
        let (_, start_line, end_line, start_column, end_column) = self.scx.map_span_to_line_column(span);
        write!(self.f, "<{}:{}-{}:{}>", start_line, end_line, start_column, end_column)
    }
    fn write_isid(&mut self, id: IsId) -> fmt::Result {
        if id.unwrap() == 1 {
            self.f.write_str("<empty>")
        } else {
            let content = self.scx.resolve_string(id);
            self.f.write_str(if content.len() > 24 { &content[..24] } else { content })
        }
    }

    fn invoke_walk<N: Node>(&mut self, node: &N) -> fmt::Result {
        self.f.write_char('\n')?;
        self.level += 1;
        let result = node.walk(self);
        self.level -= 1;
        result
    }
}

impl<'scx, 'f1, 'f2, F> Visitor<(), fmt::Error> for FormatVisitor<'scx, 'f1, 'f2, F> where F: FileSystem {

    // default implementation correctly transparents this type of node
    // fn visit_expr_list(&mut self, node: &ExprList) -> fmt::Result;
    // fn visit_expr(&mut self, node: &ExprList) -> fmt::Result;
    // fn visit_stmt(&mut self, node: &ExprList) -> fmt::Result;
    // fn visit_item(&mut self, node: &ExprList) -> fmt::Result;

    fn visit_array_def(&mut self, node: &ArrayDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("array-def ")?;
        self.write_span(node.bracket_span)?;
        self.invoke_walk(node)
    }

    fn visit_assign_expr_stmt(&mut self, node: &AssignExprStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("assign-expr-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_str(" operator ")?;
        self.write_str(node.assign_op.display())?;
        self.write_space()?;
        self.write_span(node.assign_op_span)?;
        self.invoke_walk(node)
    }

    fn visit_binary_expr(&mut self, node: &BinaryExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("binary-expr ")?;
        self.write_span(node.all_span)?;
        self.write_str(" operator ")?;
        self.write_str(node.operator.display())?;
        self.write_space()?;
        self.write_span(node.operator_span)?;
        self.invoke_walk(node)
    }

    fn visit_block_stmt(&mut self, node: &BlockStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("block-stmt ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_block(&mut self, node: &Block) -> fmt::Result {
        self.write_indent()?;
        self.write_str("block ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_break_stmt(&mut self, node: &BreakStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("break-stmt ")?;
        self.write_span(node.0.all_span)?;
        if let Some(label) = &node.0.target {
            self.write_space()?;
            self.write_str(" @")?;
            self.write_isid(*label)?;
            self.write_space()?;
            self.write_span(node.0.target_span)?;
        }
        self.invoke_walk(node)
    }

    fn visit_continue_stmt(&mut self, node: &ContinueStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("continue-stmt ")?;
        self.write_span(node.0.all_span)?;
        if let Some(label) = &node.0.target {
            self.write_space()?;
            self.write_str(" @")?;
            self.write_isid(*label)?;
            self.write_space()?;
            self.write_span(node.0.target_span)?;
        }
        self.invoke_walk(node)
    }

    fn visit_fn_call_expr(&mut self, node: &FnCallExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("fn-call ")?;
        self.write_span(node.all_span)?;
        self.write_str(" paren ")?;
        self.write_span(node.paren_span)?;
        self.invoke_walk(node)
    }

    fn visit_fn_def(&mut self, node: &FnDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("fn ")?;
        self.write_span(node.all_span)?;
        self.write_space()?;
        self.write_isid(node.name)?;
        self.write_space()?;
        self.write_span(node.name_span)?;
        self.write_str(" paren ")?;
        self.write_span(node.params_paren_span)?;
        self.invoke_walk(node)
    }

    fn visit_fn_param(&mut self, node: &FnParam) -> fmt::Result {
        self.write_indent()?;
        self.write_str("fn-param ")?;
        self.write_isid(node.name)?;
        self.write_space()?;
        self.write_span(node.name_span)?;
        self.invoke_walk(node)
    }

    fn visit_for_stmt(&mut self, node: &ForStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("for-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_space()?;
        self.write_str("for ")?;
        self.write_span(node.for_span)?;
        self.write_str(" iter-var ")?;
        self.write_isid(node.iter_name)?;
        self.write_space()?;
        self.write_span(node.iter_span)?;
        self.invoke_walk(node)
    }

    fn visit_if_stmt(&mut self, node: &IfStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("if-stmt ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_if_clause(&mut self, node: &IfClause) -> fmt::Result {
        self.write_indent()?;
        self.write_str("if-clause ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_else_if_clause(&mut self, node: &ElseIfClause) -> fmt::Result {
        self.write_indent()?;
        self.write_str("else-if-clause ")?;
        self.write_span(node.all_span)?;
        self.write_str(" else if ")?;
        self.write_span(node.elseif_span)?;
        self.invoke_walk(node)
    }

    fn visit_else_clause(&mut self, node: &ElseClause) -> fmt::Result {
        self.write_indent()?;
        self.write_str("else-clause ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_index_call_expr(&mut self, node: &IndexCallExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("index-call ")?;
        self.write_span(node.all_span)?;
        self.write_str(" bracket ")?;
        self.write_span(node.bracket_span)?;
        self.invoke_walk(node)
    }

    fn visit_label_def(&mut self, node: &LabelDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("label @")?;
        self.write_isid(node.name)?;
        self.write_space()?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }
    
    fn visit_lit_expr(&mut self, node: &LitExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("literal ")?;
        match node.value {
            LitValue::Unit => self.f.write_str("unit")?,
            LitValue::Bool(v) => write!(self.f, "bool {v}")?,
            LitValue::Char(v) => write!(self.f, "char {v:?}")?,
            LitValue::Str(id) => { self.write_str("str ")?; self.write_isid(id)? },
            LitValue::Num(v) => write!(self.f, "{v}")?,
        }
        self.write_space()?;
        self.write_span(node.span)?;
        self.invoke_walk(node)
    }

    fn visit_loop_stmt(&mut self, node: &LoopStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("loop-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_str(" loop ")?;
        self.write_span(node.loop_span)?;
        self.invoke_walk(node)
    }

    fn visit_member_access(&mut self, node: &MemberAccessExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("member-access ")?;
        self.write_span(node.all_span)?;
        self.write_str(" dot ")?;
        self.write_span(node.dot_span)?;
        self.invoke_walk(node)
    }

    fn visit_module(&mut self, node: &Module) -> fmt::Result {
        self.write_indent()?;
        self.write_str("module ")?;
        self.write_str("<TODO FILE NAME>")?;
        self.invoke_walk(node)
    }

    fn visit_module_stmt(&mut self, node: &ModuleStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("import-stmt ")?;
        self.write_span(node.all_span)?;
        if node.target.is_some() {
            self.write_str(" as ")?;
            self.write_span(node.as_span)?;
        }
        self.invoke_walk(node)
    }

    fn visit_name(&mut self, node: &Name) -> fmt::Result {
        self.write_indent()?;
        self.write_str("name ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_paren_expr(&mut self, node: &ParenExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("paren-expr ")?;
        self.write_span(node.span)?;
        self.invoke_walk(node)
    }
    
    fn visit_range_both_expr(&mut self, node: &RangeBothExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("range-both-expr ")?;
        self.write_span(node.all_span)?;
        self.write_str(" dotdot ")?;
        self.write_span(node.op_span)?;
        self.invoke_walk(node)
    }
    
    fn visit_range_full_expr(&mut self, node: &RangeFullExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("range-full-expr ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_range_left_expr(&mut self, node: &RangeLeftExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("range-left-expr ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_range_right_expr(&mut self, node: &RangeRightExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("range-right-expr ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_ret_stmt(&mut self, node: &ReturnStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("ret-stmt ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_simple_expr_stmt(&mut self, node: &SimpleExprStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("simple-expr-stmt ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_simple_name(&mut self, node: &SimpleName) -> fmt::Result {
        self.write_indent()?;
        self.write_str("simple-name ")?;
        self.write_isid(node.value)?;
        self.write_space()?;
        self.write_span(node.span)?;
        self.invoke_walk(node)
    }

    fn visit_tuple_def(&mut self, node: &TupleDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("tuple-def ")?;
        self.write_span(node.paren_span)?;
        self.invoke_walk(node)
    }

    fn visit_type_def(&mut self, node: &TypeDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("type ")?;
        self.write_span(node.all_span)?;
        self.invoke_walk(node)
    }

    fn visit_type_field_def(&mut self, node: &TypeFieldDef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("field ")?;
        self.write_span(node.all_span)?;
        self.write_str(" colon ")?;
        self.write_span(node.colon_span)?;
        self.invoke_walk(node)
    }

    fn visit_type_ref(&mut self, node: &TypeRef) -> fmt::Result {
        self.write_indent()?;
        self.write_str("type-ref ")?;
        self.write_span(node.all_span)?;
        self.write_space()?;
        self.write_isid(node.base)?;
        self.write_space()?;
        self.write_span(node.base_span)?;
        if !node.params.is_empty() {
            self.write_str(" bracket ")?;
            self.write_span(node.quote_span)?;
        }
        self.invoke_walk(node)
    }

    fn visit_unary_expr(&mut self, node: &UnaryExpr) -> fmt::Result {
        self.write_indent()?;
        self.write_str("unary-expr ")?;
        self.write_span(node.all_span)?;
        self.write_space()?;
        self.write_str(node.operator.display())?;
        self.write_space()?;
        self.write_span(node.operator_span)?;
        self.invoke_walk(node)
    }

    fn visit_use_stmt(&mut self, node: &UseStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("use-stmt ")?;
        self.write_span(node.all_span)?;
        if node.target.is_some() {
            self.write_str(" as ")?;
            self.write_span(node.as_span)?;
        }
        self.invoke_walk(node)
    }

    fn visit_var_decl(&mut self, node: &VarDeclStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("var-def ")?;
        self.write_span(node.all_span)?;
        if node.is_const {
            self.write_str(" const")?;
        }
        self.write_space()?;
        self.write_isid(node.name)?;
        self.write_space()?;
        self.write_span(node.name_span)?;
        self.invoke_walk(node)
    }

    fn visit_while_stmt(&mut self, node: &WhileStatement) -> fmt::Result {
        self.write_indent()?;
        self.write_str("while-stmt ")?;
        self.write_span(node.all_span)?;
        self.write_str(" while ")?;
        self.write_span(node.while_span)?;
        self.invoke_walk(node)
    }
}

pub struct NodeDisplay<'n, 'scx, N, F>(pub (in super)&'n N, pub (in super) &'scx SourceContext<F>);

impl<'n, 'scx, N: Node, F: FileSystem> fmt::Display for NodeDisplay<'n, 'scx, N, F> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut visitor = FormatVisitor::new(self.1, f);
        self.0.accept(&mut visitor)
    }
}

// check node equal by PartialEq, if not, compare display text, or else I'll blind comparing them by human eyes
#[cfg(test)]
pub fn print_diff<N: Node>(actual: &N, expect: &N, backtrace: u32) {
    let mut scx = SourceContext::new_file_system(crate::source::VirtualFileSystem{
        cwd: "1".into(),
        files: [("1".into(), std::iter::repeat(' ').take(10000).collect())].into_iter().collect(),
    });
    let mut chars = scx.entry("1");
    for i in 2..100 {
        chars.intern(&format!("#{i}")); // this maps string id to #id
    }
    chars.finish();

    let mut buf = format!("line {backtrace} node not same\n");
    let (actual_display, expect_display) = (actual.display(&scx).to_string(), expect.display(&scx).to_string());
    let (actual_lines, expect_lines) = (actual_display.lines().collect::<Vec<_>>(), expect_display.lines().collect::<Vec<_>>());

    let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
    for line in 0..common_line_count {
        if actual_lines[line] != expect_lines[line] {
            write!(buf, "{: >3} |- {}\n", line + 1, actual_lines[line]).unwrap();
            write!(buf, "    |+ {}\n", expect_lines[line]).unwrap();
        } else {
            write!(buf, "{: >3} |  {}\n", line + 1, actual_lines[line]).unwrap();
        }
    }
    if actual_lines.len() > common_line_count {
        for line in common_line_count..actual_lines.len() {
            write!(buf, "{: >3} |- {}\n", line + 1, actual_lines[line]).unwrap();
        }
    }
    if expect_lines.len() > common_line_count {
        for line in common_line_count..expect_lines.len() {
            write!(buf, "{: >3} |+ {}\n", line + 1, expect_lines[line]).unwrap();
        }
    }
    assert!(false, "{}", buf);
}

#[cfg(test)]
macro_rules! assert_node_eq {
    ($actual:expr, $expect:expr) => {{
        let (actual, expect) = ($actual, $expect);
        if actual != expect {
            print_diff(&actual, &expect, line!());
        }
    }}
}
#[cfg(test)]
pub(crate) use assert_node_eq;

#[cfg(test)]
mod tests {
    use crate::source::Span;
    use crate::lexical::{Numeric, Separator};
    // 3 supers are too long, note that macro export is not in star
    use crate::syntax::{make_source, make_exprs, make_lit};
    use crate::syntax::*;

    macro_rules! assert_text_eq {
        ($left:expr, $right:expr) => {
            if $left != $right {
                let left_lines = $left.lines();
                let right_lines = $right.lines();
                for (index, (left_line, right_line)) in left_lines.zip(right_lines).enumerate() {
                    if left_line != right_line {
                        panic!("assertion failed at index {}\nleft: {}\nright: {}", index, $left, $right);
                    }
                }
                panic!("assertion failed, but cannot detected by compare each line\nleft: {}\nright: {}", $left, $right);
            }
        }
    }
    
    #[test]
    fn array_def() {

        let mut scx = make_source!("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
        scx.entry("1").finish();
        assert_eq!{
            ArrayDef::new(Span::new(0, 42), make_exprs![]).display(&scx).to_string(),
            "array-def <1:1-1:43>\n"
        }

        let mut scx = make_source!("abcde\nfg\nhi\njklm");
        scx.entry("1").finish();
        assert_eq!{
            ArrayDef::new(Span::new(0, 9), make_exprs![
                make_lit!(1, 1, 1),
                make_lit!(2, 4, 4),
                make_lit!(48, 7, 8),
            ]).display(&scx).to_string(),
            "array-def <1:1-3:1>
  literal i32 1 <1:2-1:2>
  literal i32 2 <1:5-1:5>
  literal i32 48 <2:2-2:3>
"
        }
    }

    #[test]
    fn binary_expr() {

        let mut scx = make_source!("ascasconwoeicnqw");
        scx.entry("1").finish();
        assert_eq!{ 
            BinaryExpr::new(
                make_lit!(1, 0, 0),
                Separator::Add, Span::new(2, 2),
                make_lit!(2, 4, 4)
            ).display(&scx).to_string(),
            "binary-expr <1:1-1:5> operator + <1:3-1:3>
  literal i32 1 <1:1-1:1>
  literal i32 2 <1:5-1:5>
"
        }
    }

    #[test]
    fn expr_list() {
    
        let mut scx = make_source!("123123234123");
        scx.entry("1").finish();
        assert_eq!{
            ExprList::new(vec![
                Expr::Lit(make_lit!(1, 1, 2)),
                Expr::Lit(make_lit!(2, 3, 4)),
                Expr::Lit(make_lit!(3, 5, 6)),
            ]).display(&scx).to_string(),
            "literal i32 1 <1:2-1:3>\nliteral i32 2 <1:4-1:5>\nliteral i32 3 <1:6-1:7>\n"
        }
    }

    #[test]
    fn tuple_def() {
    
        let mut scx = make_source!("1231241241231412341234");
        scx.entry("1").finish();
        assert_eq!{
            TupleDef::new(Span::new(0, 21), make_exprs![]).display(&scx).to_string(),
            "tuple-def <1:1-1:22>\n"
        }
    
        let mut scx = make_source!("1231241241231412341234");
        scx.entry("1").finish();
        assert_eq!{
            TupleDef::new(Span::new(0, 8), make_exprs![
                make_lit!(1, 1, 2),
                make_lit!(2, 3, 4),
                make_lit!(48, 5, 6),
            ]).display(&scx).to_string(),
            "tuple-def <1:1-1:9>\n  literal i32 1 <1:2-1:3>\n  literal i32 2 <1:4-1:5>\n  literal i32 48 <1:6-1:7>\n"
        }
    }

    #[test]
    fn loop_stmt() {
        //                  1234567890123456789 0123 45678
        let (node, scx) = make_node!("@@: loop { println(\"233\"); }" as LoopStatement, [], ["@", "println", "233"], and source);
        assert_eq!{ node.display(&scx).to_string(), r#"loop-stmt <1:1-1:28> loop <1:5-1:8>
  label @@ <1:1-1:3>
  block <1:10-1:28>
    simple-expr-stmt <1:12-1:26>
      fn-call <1:12-1:25> paren <1:19-1:25>
        simple-name println <1:12-1:18>
        literal str 233 <1:20-1:24>
"#
        }
    }

    #[test]
    fn postfix_expr() {
        // Attention that this source code line's LF is also the string literal (test oracle)'s LF
        //                           0         1         2         3         4         5        
        //                           0123456789012345678901234567890123456789012345678901234567
        let (node, scx) = make_node!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]" as Expr, [], [], and source);
        let actual = node.display(&scx).to_string();
        assert_text_eq!{ actual, "index-call <1:1-1:58> bracket <1:49-1:58>
  index-call <1:1-1:48> bracket <1:40-1:48>
    member-access <1:1-1:39> dot <1:38-1:38>
      fn-call <1:1-1:37> paren <1:36-1:37>
        member-access <1:1-1:35> dot <1:34-1:34>
          index-call <1:1-1:33> bracket <1:31-1:33>
            member-access <1:1-1:30> dot <1:29-1:29>
              fn-call <1:1-1:28> paren <1:25-1:28>
                fn-call <1:1-1:24> paren <1:15-1:24>
                  member-access <1:1-1:14> dot <1:13-1:13>
                    fn-call <1:1-1:12> paren <1:4-1:12>
                      member-access <1:1-1:3> dot <1:2-1:2>
                        simple-name a <1:1-1:1>
                        simple-name b <1:3-1:3>
                      simple-name c <1:5-1:5>
                      simple-name d <1:8-1:8>
                      simple-name e <1:11-1:11>
                    simple-name f <1:14-1:14>
                  simple-name g <1:16-1:16>
                  simple-name h <1:19-1:19>
                  simple-name i <1:22-1:22>
                simple-name u <1:26-1:26>
              simple-name j <1:30-1:30>
            simple-name k <1:32-1:32>
          simple-name l <1:35-1:35>
      simple-name m <1:39-1:39>
    simple-name n <1:41-1:41>
    simple-name o <1:44-1:44>
    simple-name p <1:47-1:47>
  simple-name r <1:50-1:50>
  simple-name s <1:53-1:53>
  simple-name t <1:56-1:56>
"
        }
    }
}
