use super::*;

macro_rules! ppcase { // pretty print case
    ($left:expr, $right:expr) => { // left: NodeDisplay, right: expected &'static str
        let (left, right) = ($left.to_string(), $right);
        if left != right {
            panic!("display not same\n{}", DiffDisplay(&left, right));
        }
    }
}

#[test]
fn array_def() {

    let mut ecx = make_errors!();
    let mut scx = make_source!("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{
        make_expr!(array 0:42).display(&scx),
        "array-def <1:1-1:43>\n"
    }

    let mut scx = make_source!("abcde\nfg\nhi\njklm");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{
        make_expr!(array 0:9
            make_expr!(i32 1 1:1),
            make_expr!(i32 2 4:4),
            make_expr!(i32 48 7:8)
        ).display(&scx),
        "array-def <1:1-3:1>
  literal i32 1 <1:2-1:2>
  literal i32 2 <1:5-1:5>
  literal i32 48 <2:2-2:3>
"
    }
}

#[test]
fn binary_expr() {

    let mut ecx = make_errors!();
    let mut scx = make_source!("ascasconwoeicnqw");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{ 
        make_expr!(binary 0:4 Add 2:2
            make_expr!(i32 1 0:0),
            make_expr!(i32 2 4:4)
        ).display(&scx),
            "binary-expr <1:1-1:5> + <1:3-1:3>
  literal i32 1 <1:1-1:1>
  literal i32 2 <1:5-1:5>
"
    }
}

#[test]
fn expr_list() {

    let mut ecx = make_errors!();
    let mut scx = make_source!("123123234123");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{
        ExprList{ items: vec![
            make_expr!(i32 1 1:2),
            make_expr!(i32 2 3:4),
            make_expr!(i32 3 5:6),
        ] }.display(&scx),
        "literal i32 1 <1:2-1:3>\nliteral i32 2 <1:4-1:5>\nliteral i32 3 <1:6-1:7>\n"
    }
}

#[test]
fn tuple_def() {

    let mut ecx = make_errors!();
    let mut scx = make_source!("1231241241231412341234");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{
        make_expr!(tuple 0:21).display(&scx),
        "tuple-def <1:1-1:22>\n"
    }

    let mut scx = make_source!("1231241241231412341234");
    scx.entry("1", &mut ecx).unwrap().finish();
    ppcase!{
        make_expr!(tuple 0:8
            make_expr!(i32 1 1:2),
            make_expr!(i32 2 3:4),
            make_expr!(i32 48 5:6)
        ).display(&scx),
        "tuple-def <1:1-1:9>\n  literal i32 1 <1:2-1:3>\n  literal i32 2 <1:4-1:5>\n  literal i32 48 <1:6-1:7>\n"
    }
}

#[test]
fn loop_stmt() {
    //                  1234567890123456789 0123 45678
    let mut scx = make_source!("@@: loop { println(\"233\"); }");
    let mut ecx = crate::diagnostics::make_errors!();
    let mut context = Parser::new(crate::lexical::Parser::new(scx.entry("1", &mut ecx).unwrap(), &mut ecx));
    let node = context.parse_loop_stmt().unwrap();
    context.finish();
    ppcase!{ node.display(&scx), r#"loop-stmt <1:1-1:28> loop <1:5-1:8>
  label @@ <1:1-1:3>
  block <1:10-1:28>
    simple-expr-stmt <1:12-1:26>
      fn-call <1:12-1:25> () <1:19-1:25>
        name <1:12-1:18>
          name-segment <1:12-1:18> println
        literal str "233" <1:20-1:24>
"#
    }
}

#[test]
fn postfix_expr() {
    //                           0         1         2         3         4         5        
    //                           0123456789012345678901234567890123456789012345678901234567
    let mut scx = make_source!("a.b(c, d, e).f(g, h, i,)(u,).j[k].l().m[n, o, p][r, s, t,]");
    let mut ecx = crate::diagnostics::make_errors!();
    let mut context = Parser::new(crate::lexical::Parser::new(scx.entry("1", &mut ecx).unwrap(), &mut ecx));
    let node = context.parse_postfix_expr().unwrap();
    context.finish();
    ppcase!{ node.display(&scx), "index-call <1:1-1:58> [] <1:49-1:58>
  index-call <1:1-1:48> [] <1:40-1:48>
    member-access <1:1-1:39> . <1:38-1:38>
      fn-call <1:1-1:37> () <1:36-1:37>
        member-access <1:1-1:35> . <1:34-1:34>
          index-call <1:1-1:33> [] <1:31-1:33>
            member-access <1:1-1:30> . <1:29-1:29>
              fn-call <1:1-1:28> () <1:25-1:28>
                fn-call <1:1-1:24> () <1:15-1:24>
                  member-access <1:1-1:14> . <1:13-1:13>
                    fn-call <1:1-1:12> () <1:4-1:12>
                      member-access <1:1-1:3> . <1:2-1:2>
                        name <1:1-1:1>
                          name-segment <1:1-1:1> a
                        name <1:3-1:3>
                          name-segment <1:3-1:3> b
                      name <1:5-1:5>
                        name-segment <1:5-1:5> c
                      name <1:8-1:8>
                        name-segment <1:8-1:8> d
                      name <1:11-1:11>
                        name-segment <1:11-1:11> e
                    name <1:14-1:14>
                      name-segment <1:14-1:14> f
                  name <1:16-1:16>
                    name-segment <1:16-1:16> g
                  name <1:19-1:19>
                    name-segment <1:19-1:19> h
                  name <1:22-1:22>
                    name-segment <1:22-1:22> i
                name <1:26-1:26>
                  name-segment <1:26-1:26> u
              name <1:30-1:30>
                name-segment <1:30-1:30> j
            name <1:32-1:32>
              name-segment <1:32-1:32> k
          name <1:35-1:35>
            name-segment <1:35-1:35> l
      name <1:39-1:39>
        name-segment <1:39-1:39> m
    name <1:41-1:41>
      name-segment <1:41-1:41> n
    name <1:44-1:44>
      name-segment <1:44-1:44> o
    name <1:47-1:47>
      name-segment <1:47-1:47> p
  name <1:50-1:50>
    name-segment <1:50-1:50> r
  name <1:53-1:53>
    name-segment <1:53-1:53> s
  name <1:56-1:56>
    name-segment <1:56-1:56> t
"
    }
}
