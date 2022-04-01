///! fff-lang
///! 
///! syntax/module, a source code file is a module
///! module = { item }

use super::prelude::*;

impl Parser for Module {
    type Output = Module;

    fn parse(cx: &mut ParseContext) -> Result<Module, Unexpected> {
        let mut items = Vec::new();
        while !cx.eof() {
            items.push(cx.expect::<Item>()?);
        }
        Ok(Module{ items, file: cx.get_file_id() })
    }
}

struct CollectImportVisitor {
    requests: Vec<(Span, IsId)>,
}
impl Visitor for CollectImportVisitor {
    fn visit_module_stmt(&mut self, node: &ModuleStatement) -> Result<(), ()> {
        self.requests.push((node.all_span, node.path.map(|(path, _)| path).unwrap_or(node.name)));
        Ok(())
    }
}

impl Module {

    pub fn collect_imports(&self) -> Vec<(Span, IsId)> {
        let mut collector = CollectImportVisitor{ requests: Vec::new() };
        self.accept(&mut collector).unwrap();
        collector.requests
    }
}

#[cfg(test)] #[test]
fn module_parse() {
    //                      0123456789012345678901234
    case!{ "use a; module b; 3; b; a;" as Module,
        Module{ file: crate::source::FileId::new(1), items: vec![
            Item::Use(UseStatement{ all_span: Span::new(0, 5), alias: None,
                name: make_name!(simple bare 4:4 #2) }),
            Item::Import(ModuleStatement{ name: IsId::new(3), name_span: Span::new(14, 14), path: None, all_span: Span::new(7, 15) }),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(17, 18), 
                make_expr!(i32 3 17:17)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(20, 21), 
                make_name!(simple 20:20 #3)
            )),
            Item::SimpleExpr(SimpleExprStatement::new(Span::new(23, 24), 
                make_name!(simple 23:23 #2)
            )),
        ] }
    }
}

#[cfg(test)]
#[test]
fn module_integration() {
    use std::fmt::Write;
    use std::fs::read_to_string;
    use crate::source::{SourceContext};
    use crate::diagnostics::Diagnostics;
    use crate::lexical::Parser as LexicalParser;

    let test_cases = read_to_string("tests/syntax/inter/index.txt").expect("cannot read index.txt");
    for line in test_cases.lines() {
        let expect_display = read_to_string(format!("tests/syntax/inter/{line}_result.txt")).expect(&format!("cannot read {line}_result.txt"));

        let mut scx: SourceContext = SourceContext::new(); // this is amazingly real file system
        let mut ecx = Diagnostics::new();
        let mut context = ParseContext::new(LexicalParser::new(scx.entry(format!("tests/syntax/inter/{line}.f3")), &mut ecx));

        let actual = Module::parse(&mut context);
        context.finish();
        if actual.is_err() {
            panic!("{}.f3 parse fail: {}", line, ecx.display(&scx));
        }
        let actual_display = actual.unwrap().display(&scx).to_string();

        if actual_display != expect_display {
            let mut buf = format!("{}.f3 display not same\n", line);
            let (actual_lines, expect_lines) = (actual_display.lines().collect::<Vec<_>>(), expect_display.lines().collect::<Vec<_>>());
            let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
            for line in 0..common_line_count {
                if actual_lines[line] != expect_lines[line] {
                    writeln!(buf, "{: >3} |- {}", line + 1, actual_lines[line]).unwrap();
                    writeln!(buf, "    |+ {}", expect_lines[line]).unwrap();
                } else {
                    writeln!(buf, "{: >3} |  {}", line + 1, actual_lines[line]).unwrap();
                }
            }
            if actual_lines.len() > common_line_count {
                for line in common_line_count..actual_lines.len() {
                    writeln!(buf, "{: >3} |- {}", line + 1, actual_lines[line]).unwrap();
                }
            }
            if expect_lines.len() > common_line_count {
                for line in common_line_count..expect_lines.len() {
                    writeln!(buf, "{: >3} |+ {}", line + 1, expect_lines[line]).unwrap();
                }
            }
            panic!("{}", buf)
        }
    }
}
