use super::*;
use crate::diagnostics::make_errors;

macro_rules! make_source {
    () => {
        crate::source::make_source!("")
    };
    ($content:literal) => {
        crate::source::SourceContext::new_file_system(
            crate::source::VirtualFileSystem{ cwd: "/".into(), files: [("1".into(), $content.into())].into_iter().collect() })
    };
    ($($content:literal as $name:literal),+$(,)?) => {{
        let vfs = [$((std::path::PathBuf::from($name), String::from($content)),)*];
        crate::source::SourceContext::new_file_system(
            crate::source::VirtualFileSystem{ cwd: std::env::current_dir().unwrap(), files: vfs.into_iter().collect() })
    }};
    ($($content:literal as $name:literal,)+ $scx:ident in $init:block) => {{
        let vfs = [$((std::path::PathBuf::from($name), String::from($content)),)*];
        let mut $scx = crate::source::SourceContext::new_file_system(
            crate::source::VirtualFileSystem{ cwd: std::env::current_dir().unwrap(), files: vfs.into_iter().collect() });
        $init
        $scx
    }};
}

// // now it should be exported like this
pub(crate) use make_source;

#[test]
fn intern_values() {

    let mut scx = make_source!();
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    let id1 = chars.intern("abc");
    let id2 = chars.intern("123");
    let id3 = chars.intern("abc");
    chars.finish();
    assert!(id1 != id2);
    assert!(id2 != id3);
    assert!(id1 == id3);
    println!("{:?}", scx);
    assert_eq!(scx.resolve_string(id1), "abc");
    assert_eq!(scx.resolve_string(id2), "123");
    assert_eq!(scx.resolve_string(id3), "abc");
}

#[test]
fn intern_spans() {
    //                          012345678901234567890
    let mut scx = make_source!("eiwubvoqwincleiwubaslckhwoaihecbqvqvqwoliecn");
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    let id1 = chars.intern_span(Span::new(0, 3));
    let id2 = chars.intern_span(Span::new(1, 4));
    let id3 = chars.intern_span(Span::new(13, 16));
    chars.finish();
    assert!(id1 != id2);
    assert!(id2 != id3);
    assert!(id1 == id3);
    assert_eq!(scx.resolve_string(id1), "eiwu");
    assert_eq!(scx.resolve_string(id2), "iwub");
    assert_eq!(scx.resolve_string(id3), "eiwu");
}

#[test]
fn intern_spans2() {
    // bug from lexical v2_base
    //                                    1          2         3   
    //                          01234567890123 456789012345678901234 5678
    let mut scx = make_source!("var a = true;\nvar b = 789_123.456;\ndefg");
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    let id1 = chars.intern_span(Span::new(4, 4));
    let id2 = chars.intern_span(Span::new(18, 18));
    let id3 = chars.intern_span(Span::new(35, 38));
    chars.finish();
    assert!(id1 != id2);
    assert!(id1 != id3);
    assert!(id2 != id3);
    assert_eq!(scx.resolve_string(id1), "a");
    assert_eq!(scx.resolve_string(id2), "b");
    assert_eq!(scx.resolve_string(id3), "defg");

    // and this
    
    let mut scx = make_source!("一个chinese变量, a_中文_var");
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    let id1 = chars.intern_span(Span::new(0, 16));
    let id2 = chars.intern_span(Span::new(21, 32));
    chars.finish();
    assert!(id1 != id2);
    assert!(id1 != id3);
    assert_eq!(scx.resolve_string(id1), "一个chinese变量");
    assert_eq!(scx.resolve_string(id2), "a_中文_var");
}

#[test]
#[cfg(debug_assertions)]
#[should_panic(expected = "not this file span")]
fn not_this_file_span() {
    let mut scx = make_source!("module 2" as "src/main.f3", "" as "src/2.f3");
    let mut ecx = make_errors!();
    let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
    let sym = chars.intern_span(Span::new(7, 7));
    chars.finish();
    let mut chars = scx.import(Span::new(0, 7), sym, None, &mut ecx).unwrap();
    chars.intern_span(Span::new(7, 7));
}

#[test]
#[should_panic(expected = "invalid string id")]
fn invalid_string_id() {
    let mut scx = make_source!();
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    chars.intern("abc");
    chars.finish();
    assert_eq!(scx.resolve_string(IsId::new(2)), "abc");
    let _ = scx.resolve_string(IsId::new(100));
}

#[test]
#[should_panic(expected = "invalid span")]
fn empty_span1() {
    let mut scx = make_source!();
    let mut ecx = make_errors!();
    let mut chars = scx.entry("1", &mut ecx).unwrap();
    chars.intern_span(Span::new(1, 0));
}

#[test]
#[should_panic(expected = "invalid span")]
fn empty_span2() {
    let scx = make_source!();
    scx.map_span_to_content(Span::new(4242, 42));
}

#[test]
fn relative_path() {
    // ... very complex to cross platform, acutally only 2 points: root and path separator
    // 17/8/1: congratulations: first correct use of 'separator'!

    macro_rules! test_case {
        ([$($c1: expr),*], [$($c2: expr),*] => [$($expect: expr),+]) => (
            let mut path1 = PathBuf::from(if cfg!(windows) { "C:\\" } else { "/" });
            let mut path2 = PathBuf::from(if cfg!(windows) { "C:\\" } else { "/" });
            path1.extend(vec![$($c1,)*]);
            path2.extend(vec![$($c2,)*]);
            assert_eq!{ get_relative_path(&path1, &path2), vec![$($expect,)+].into_iter().collect::<PathBuf>() }
        )
    }

    test_case!(["Fresky", "fff-lang"], ["Fresky", "a", "b.txt"] => ["..", "a", "b.txt"]);
    test_case!(["Fresky"], ["Fresky", "c", "ddd.eee"] => ["c", "ddd.eee"]);
    test_case!(["Fresky", "a", "bcd", "efg"], ["d.ff"] => ["..", "..", "..", "..", "d.ff"]);
}

macro_rules! ptlc_test_case {
    ($scx:expr, $([#$caseid:literal: $position:expr => $file:expr, $row:expr, $col:expr],)+) => (
        $(
            assert_eq!{ $scx.map_position_to_line_column(Position::new($position)), (FileId::new($file), $row, $col), "#{}", $caseid }
        )+
    )
}

#[test]
fn position_to_line_column1() {

    let mut ecx = make_errors!();
    ptlc_test_case!{ make_source!("0123\n56\r8\n01234567\n9" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#1: 0 => 1, 1, 1],
        [#2: 2 => 1, 1, 3],
        [#3: 14 => 1, 3, 5],
    }
    ptlc_test_case!{ make_source!("012345678901234567" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#4: 0 => 1, 1, 1],
        [#5: 15 => 1, 1, 16],
    }
    ptlc_test_case!{ make_source!("" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#6: 0 => 1, 1, 1], // both 'EOF is next char of last char' and 'first position is (1, 1)' requires this to be (1, 1)
    }
    ptlc_test_case!{ make_source!("var 你好 =\n 世界;" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        //     src, row, col, byte
        //       v,   1,   1,    0
        //       a,   1,   2,    1
        //       r,   1,   3,    2
        //     ' ',   1,   4,    3,
        //      你,   1,   5,    4, 5, 6
        //      好,   1,   6,    7, 8, 9,
        //     ' ',   1,   7,    10,
        //       =,   1,   8,    11,
        //      \n,   1,   9,    12,
        //     ' ',   2,   1,    13
        //      世,   2,   2,    14, 15, 16,
        //      界,   2,   3,    17, 18, 19,
        //       ;,   2,   4,    20
        [#7: 0 => 1, 1, 1],
        [#8: 1 => 1, 1, 2],
        [#9: 4 => 1, 1, 5],
        [#10: 7 => 1, 1, 6],
        [#11: 14 => 1, 2, 2],
        [#12: 17 => 1, 2, 3],
        [#13: 20 => 1, 2, 4],
        [#14: 21 => 1, 2, 5],
    }

    ptlc_test_case!{ make_source!("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#15: 2 => 1, 1, 1],
        [#16: 3 => 1, 1, 2],
        [#17: 4 => 1, 1, 3],
        [#18: 11 => 1, 2, 4],
        [#19: 26 => 1, 4, 2],
        [#20: 30 => 1, 4, 6],
    }

    ptlc_test_case!{ make_source!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#21: 0 => 1, 1, 1],
        [#22: 6 => 1, 2, 3],
        [#23: 9 => 1, 2, 4],
        [#24: 10 => 1, 3, 1],
        [#25: 11 => 1, 4, 1],
        [#26: 29 => 1, 6, 7],
    }
}

#[test]
fn position_to_line_column2() {

    // this 2 empty file program should only allow 2 positions: 0 for EOF in file 1, 1 for EOF in file 2
    let mut scx = make_source!("" as "/1.f3", "" as "/2.f3");
    let mut ecx = make_errors!();
    let mut chars = scx.entry("/1.f3", &mut ecx).unwrap();
    let module_name = chars.intern("2.f3");
    chars.finish();
    scx.import(Span::new(0, 0), module_name, Some(module_name), &mut ecx).expect(&format!("{:?}", ecx)).finish();
    ptlc_test_case!{ scx,
        [#27: 0 => 1, 1, 1],
        [#28: 1 => 2, 1, 1],
    }

    //                          1    2        3 4    5         6       7                  1    2       3                 4
    //                          1234 123    4 1 1234 12  45678 1234567 1                  1234 123    4123456      67890 123456
    //                                        1            2           3                          4              5            6
    //                          0123 4567 8 9 0 1234 567 89012 3456789 0              1 2 3456 7890 1 2 345678 9 0 12345 6789012 3 4 5
    let mut scx = make_source!("abc\nd2f\r\r\n\nasd\nwe\rq1da\nawsedq\n" as "/1.f3", "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r" as "/2.f3");
    let mut chars = scx.entry("/1.f3", &mut ecx).unwrap();
    let module_name = chars.intern_span(Span::new(5, 5));
    chars.finish();
    scx.import(Span::new(4, 6), module_name, None, &mut ecx).unwrap().finish();
    ptlc_test_case!{ scx,
        [#29: 0 => 1, 1, 1],
        [#30: 6 => 1, 2, 3],
        [#31: 9 => 1, 2, 4],
        [#32: 10 => 1, 3, 1],
        [#33: 11 => 1, 4, 1],
        [#34: 29 => 1, 6, 7],
        [#35: 30 => 1, 7, 1], // if last char is \n, the EOF char will be at next line
        [#36: 33 => 2, 1, 1],
        [#37: 34 => 2, 1, 2],
        [#38: 35 => 2, 1, 3],
        [#39: 42 => 2, 2, 4],
        [#40: 51 => 2, 3, 6],
        [#41: 55 => 2, 3, 10],
        [#42: 57 => 2, 4, 2],
        [#43: 61 => 2, 4, 6],
        [#44: 65 => 2, 4, 7],
    }
}

#[test]
#[should_panic(expected = "position overflow")]
fn position_overflow() {
    ptlc_test_case!(make_source!(), [#1: 1 => 1, 1, 1],);
}

#[test]
#[should_panic(expected = "span cross file")]
fn span_cross_file1() {

    let mut ecx = make_errors!();
    let scx = make_source!("module 2;" as "src/main.f3", "fn f(){}" as  "src/2.f3", scx in {
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym, None, &mut ecx).unwrap().finish();
    });
    scx.map_span_to_line_column(Span::new(1, 10));
}

#[test]
#[should_panic(expected = "span cross file")]
fn span_cross_file2() {

    let mut ecx = make_errors!();
    let scx = make_source!("module 2;" as "src/main.f3", "fn f(){}" as  "src/2.f3", scx in {
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym, None, &mut ecx).unwrap().finish();
    });

    scx.map_span_to_content(Span::new(1, 10));
}

#[test]
fn span_to_content() {

    macro_rules! test_case {
        ($scx:expr, $([#$caseid:literal: $start_id:expr, $end_id:expr => $expect:expr], )+) => (
            let mut scx = $scx;
            let mut ecx = make_errors!();
            scx.entry("1", &mut ecx).unwrap().finish();
            $(
                assert_eq!{ scx.map_span_to_content(Span::new($start_id, $end_id)), $expect, "#{}", $caseid }
            )+
        )
    }

    test_case!{ make_source!("01234567890"),
        [#1: 0, 2 => "012"],
        [#2: 3, 5 => "345"],
        [#3: 8, 8 => "8"],
        [#4: 0, 10 => "01234567890"],
        [#5: 0, 11 => "01234567890"],
    }

    test_case!{ make_source!("var 你好 =\n 世界;；"),
        //     src, row, col, byte
        //       v,   1,   1,    0
        //       a,   1,   2,    1
        //       r,   1,   3,    2
        //     ' ',   1,   4,    3,
        //      你,   1,   5,    4, 5, 6
        //      好,   1,   6,    7, 8, 9,
        //     ' ',   1,   7,    10,
        //       =,   1,   8,    11,
        //      \n,   1,   9,    12,
        //     ' ',   2,   1,    13
        //      世,   2,   2,    14, 15, 16,
        //      界,   2,   3,    17, 18, 19,
        //       ;,   2,   4,    20
        //      ；,   2,   5,    21, 22, 23
        [#7: 0, 3 => "var "],
        [#8: 3, 4 => " 你"],
        [#9: 4, 10 => "你好 "],
        [#10: 4, 14 => "你好 =\n 世"],
        [#11: 14, 21 => "世界;；"],
        [#13: 14, 24 => "世界;；"],
        [#14: 24, 24 => ""],
    }
}

#[test]
#[should_panic(expected = "position overflow")]
fn span_overflow() {
    let mut ecx = make_errors!();
    let mut scx = make_source!();
    scx.entry("1", &mut ecx).unwrap().finish();
    scx.map_span_to_content(Span::new(0, 1));
}

#[test]
fn line_to_content() {
    
    let mut ecx = make_errors!();
    let mut scx = make_source!("0123\n56\r8\n01234567\n9");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "0123");
    assert_eq!(scx.map_line_to_content(file_id, 2), "56\r8");
    assert_eq!(scx.map_line_to_content(file_id, 3), "01234567");
    assert_eq!(scx.map_line_to_content(file_id, 4), "9");

    let mut scx = make_source!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "abc");
    assert_eq!(scx.map_line_to_content(file_id, 2), "def\r\r");
    assert_eq!(scx.map_line_to_content(file_id, 3), "");
    assert_eq!(scx.map_line_to_content(file_id, 4), "asd");
    assert_eq!(scx.map_line_to_content(file_id, 5), "we\rq1da");
    assert_eq!(scx.map_line_to_content(file_id, 6), "awsedq");

    let mut scx = make_source!("\nabc\ndef\n");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "");
    assert_eq!(scx.map_line_to_content(file_id, 2), "abc");
    assert_eq!(scx.map_line_to_content(file_id, 3), "def");
    assert_eq!(scx.map_line_to_content(file_id, 4), "");

    let mut scx = make_source!("abcdef");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "abcdef");

    let mut scx = make_source!();
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "");

    // if last charater is non ascii,
    // ending byte index was not correct, found by auto generated test
    let mut scx = make_source!("var a: abc::def\n绦");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    assert_eq!(scx.map_line_to_content(file_id, 2), "绦");
}

#[test]
#[should_panic(expected = "line number overflow")]
fn line_number_overflow1() {
    let mut ecx = make_errors!();
    let mut scx = make_source!("abc");
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    scx.map_line_to_content(file_id, 0);
}

#[test]
#[should_panic(expected = "line number overflow")]
fn line_number_overflow2() {
    let mut ecx = make_errors!();
    let mut scx = make_source!();
    let chars = scx.entry("1", &mut ecx).unwrap();
    let file_id = chars.get_file_id();
    chars.finish();
    scx.map_line_to_content(file_id, 2);
}

#[test]
fn v0() {

    macro_rules! test_case {
        ($input: expr, $($ch: expr, $char_id: expr,)*) => (
            let mut ecx = make_errors!();
            let mut scx = make_source!($input);
            let mut chars = scx.entry("1", &mut ecx).unwrap();
            let mut ret_chars = Vec::new();
            loop {
                match chars.next() {
                    (EOF, _) => break,
                    v0 => ret_chars.push(v0), // memory for v0
                }
            }

            let expect_chars = &mut Vec::new();
            $(
                expect_chars.push(($ch, Position::new($char_id)));
            )*
            assert_eq!(&ret_chars, expect_chars);
        )
    }

    //           0             1              2          3
    //           0 1 2345 6789 0 1 234567 8 9 01234 5678901 2 3
    test_case!{ "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        'a', 2, 'b', 3, 'c', 4, '\n', 5,
        'd', 6, 'e', 7, 'f', 8, '\n', 11,
        'a', 12, 's', 13, 'd', 14, 'w', 15, 'e', 16, 'q', 20, '1', 21, 'd', 22, 'a', 23, '\n', 24,
        'a', 25, 'w', 26, 's', 27, 'e', 28, 'd', 29, 'q', 30,
    } //         0             1            2
    //           0123 4567 8 9 0 1234 567 89012 3456789
    test_case!{ "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        'a', 0, 'b', 1, 'c', 2, '\n', 3,
        'd', 4, 'e', 5, 'f', 6, '\n', 9,
        '\n', 10,
        'a', 11, 's', 12, 'd', 13, '\n', 14,
        'w', 15, 'e', 16, 'q', 18, '1', 19, 'd', 20, 'a', 21, '\n', 22,
        'a', 23, 'w', 24, 's', 25, 'e', 26, 'd', 27, 'q', 28, '\n', 29,
    }

    test_case!{ "\nabc\ndef\n",
        '\n', 0,
        'a', 1, 'b', 2, 'c', 3, '\n', 4,
        'd', 5, 'e', 6, 'f', 7, '\n', 8,
    }

    test_case!{ "一个chinese变量, a_中文_var",
        '一', 0, '个', 3, 'c', 6, 'h', 7, 'i', 8, 'n', 9, 'e', 10, 's', 11, 'e', 12, '变', 13, '量', 16, ',', 19, ' ', 20, 
        'a', 21, '_', 22, '中', 23, '文', 26, '_', 29, 'v', 30, 'a', 31, 'r', 32,
    }
    test_case!{ "", }
}

#[test]
fn v0_last_non_ascii() {
    // when last char is not ascii, eof position is not last position + 1

    let mut ecx = make_errors!();
    let mut scx = make_source!("我");
    let mut chars = scx.entry("1", &mut ecx).unwrap();

    assert_eq!(chars.next(), ('我', Position::new(0)));
    assert_eq!(chars.next(), (EOF, Position::new(3)));
}

#[test]
fn span_to_line_column() {

    let mut ecx = make_errors!();
    macro_rules! test_case {
        ($scx:expr, $([#$caseid:literal: $start_id:expr, $end_id:expr => $result:expr],)+) => (
            let scx = $scx;
            $(
                assert_eq!{ scx.map_span_to_line_column(Span::new($start_id, $end_id)), $result, "#{}", $caseid }
            )+
        )
    }

    test_case!{
        make_source!("0123\n56\r8\n01234567\n9" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#1: 0, 2 => (FileId::new(1), 1, 1, 1, 3)],
        [#2: 2, 20 => (FileId::new(1), 1, 3, 4, 2)],
        [#3: 3, 14 => (FileId::new(1), 1, 4, 3, 5)],
        [#4: 2, 19 => (FileId::new(1), 1, 3, 4, 1)],
    }
    test_case!{
        make_source!("012345678901234567" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#5: 0, 18 => (FileId::new(1), 1, 1, 1, 19)],
        [#6: 13, 15 => (FileId::new(1), 1, 14, 1, 16)],
        [#7: 0, 0 => (FileId::new(1), 1, 1, 1, 1)],
    }
    test_case!{
        make_source!("" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        [#8: 0, 0 => (FileId::new(1), 1, 1, 1, 1)],
    }
    test_case!{
        make_source!("var 你好 =\n 世界;" as "1", scx in { scx.entry("1", &mut ecx).unwrap().finish(); }),
        //     src, row, col, byte
        //       v,   1,   1,    0
        //       a,   1,   2,    1
        //       r,   1,   3,    2
        //     ' ',   1,   4,    3,
        //      你,   1,   5,    4, 5, 6
        //      好,   1,   6,    7, 8, 9,
        //     ' ',   1,   7,    10,
        //       =,   1,   8,    11,
        //      \n,   1,   9,    12,
        //     ' ',   2,   1,    13
        //      世,   2,   2,    14, 15, 16,
        //      界,   2,   3,    17, 18, 19,
        //       ;,   2,   4,    20
        [#9: 0, 1 => (FileId::new(1), 1, 1, 1, 2)],
        [#10: 0, 14 => (FileId::new(1), 1, 1, 2, 2)],
        [#11: 4, 21 => (FileId::new(1), 1, 5, 2, 5)],
        [#12: 7, 17 => (FileId::new(1), 1, 6, 2, 3)],
    }

    test_case!{ make_source!("module 2;" as "src/main.f3", "fn f(){}" as  "src/2.f3", scx in {
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym, None, &mut ecx).unwrap().finish();
    }),
        [#13: 0, 5 => (FileId::new(1), 1, 1, 1, 6)],
        [#14: 0, 7 => (FileId::new(1), 1, 1, 1, 8)],
        [#15: 10, 10 => (FileId::new(2), 1, 1, 1, 1)],
        [#16: 10, 17 => (FileId::new(2), 1, 1, 1, 8)],
        [#17: 18, 18 => (FileId::new(2), 1, 9, 1, 9)],
    }
}

// module resolve test case
macro_rules! mr_test_case {
    ([$($content:literal as $name:literal),+$(,)?] import $module:expr; from $span:expr => err $error:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name = chars.intern_span($module);
        chars.finish();
        assert!(scx.import($span, module_name, None, &mut ecx).is_none());
        assert_eq!(ecx, $error);
    }};
    ([$($content:literal as $name:literal),+$(,)?] import $module:expr; from $span:expr => $path:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name = chars.intern_span($module);
        chars.finish();
        let import_chars = scx.import($span, module_name, None, &mut ecx).unwrap();
        let file_id = import_chars.get_file_id();
        import_chars.finish();
        assert_eq!(scx.files[file_id.0 as usize - 1].path, PathBuf::from($path));
    }};

    ([$($content:literal as $name:literal),+$(,)?] import explicit $module:literal from $span:expr => err $error:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name = chars.intern($module);
        chars.finish();
        assert!(scx.import($span, module_name, Some(module_name), &mut ecx).is_none());
        assert_eq!(ecx, $error);
    }};
    ([$($content:literal as $name:literal),+$(,)?] import explicit $module:literal from $span:expr => $path:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name = chars.intern($module);
        chars.finish();
        let import_chars = scx.import($span, module_name, Some(module_name), &mut ecx).unwrap();
        let file_id = import_chars.get_file_id();
        import_chars.finish();
        assert_eq!(scx.files[file_id.0 as usize - 1].path, PathBuf::from($path));
    }};

    ([$($content:literal as $name:literal),+$(,)?] import $module1:expr; from entry then import $module2:expr; from $span:expr => err $error:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name1 = chars.intern_span($module1);
        chars.finish();
        let mut chars2 = scx.import(Span::new(0, 0), module_name1, None, &mut ecx).unwrap();
        let module_name2 = chars2.intern_span($module2);
        chars2.finish();
        assert!(scx.import($span, module_name2, None, &mut ecx).is_none());
        assert_eq!(ecx, $error);
    }};
    ([$($content:literal as $name:literal),+$(,)?] import $module1:expr; from entry then import $module2:expr; from $span:expr => $path:expr) => {{
        let mut ecx = make_errors!();
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3", &mut ecx).unwrap();
        let module_name1 = chars.intern_span($module1);
        chars.finish();
        let mut chars2 = scx.import(Span::new(0, 0), module_name1, None, &mut ecx).unwrap();
        let module_name2 = chars2.intern_span($module2);
        chars2.finish();
        let chars3 = scx.import($span, module_name2, None, &mut ecx).unwrap();
        let file_id = chars3.get_file_id();
        chars3.finish();
        assert_eq!(scx.files[file_id.0 as usize - 1].path, PathBuf::from($path));
    }}
}

#[test]
fn resolve_entry() {

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module-name.f3",
        "" as "src/module-name/index.f3",
        "" as "src/module_name.f3",
        "" as "src/module_name/index.f3",
    ] import Span::new(7, 17); from Span::new(0, 0) => "src/module-name.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module-name/index.f3",
        "" as "src/module_name.f3",
        "" as "src/module_name/index.f3",
    ] import Span::new(7, 17); from Span::new(0, 0) => "src/module-name/index.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module_name.f3",
        "" as "src/module_name/index.f3",
    ] import Span::new(7, 17); from Span::new(0, 0) => "src/module_name.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module_name/index.f3",
    ] import Span::new(7, 17); from Span::new(0, 0) => "src/module_name/index.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
    ] import Span::new(7, 17); from Span::new(0, 0) => err make_errors!{
        e: e.emit(strings::FailedToReadAllCandidates)
            .detail(Span::new(0, 0), strings::OriginatedHere)
            .help(format!("{} [\"src/module-name.f3\", \"src/module-name/index.f3\", \"src/module_name.f3\", \"src/module_name/index.f3\"]", strings::Candidates))
    });
}

#[test]
fn resolve_explicit() {

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "" as "src/strange file name.strange ext",
    ] import explicit "strange file name.strange ext" from Span::new(0, 0) => "src/strange file name.strange ext");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "" as "src/../abc/def/ghi/f3", // because vfs does not have real canonicalize
    ] import explicit "../abc/def/ghi/f3" from Span::new(0, 0) => "src/../abc/def/ghi/f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
    ] import explicit "../abc/def/ghi/f3" from Span::new(0, 0) => err make_errors!{
        e: e.emit(strings::FailedToReadAllCandidates)
            .detail(Span::new(0, 0), strings::OriginatedHere).help(format!("{} [\"src/../abc/def/ghi/f3\"]", strings::Candidates))
    });
}

#[test]
fn resolve_index() {

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module-name.f3",
        "" as "src/some-module/module-name/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module-name.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module-name/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3"
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module-name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module_name.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module_name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => err make_errors!{
        e: e.emit(strings::FailedToReadAllCandidates)
            .detail(Span::new(30, 30), strings::OriginatedHere)
            .help(format!("{} [\"src/some-module/module-name.f3\", \"src/some-module/module-name/index.f3\", \"src/some-module/module_name.f3\", \"src/some-module/module_name/index.f3\"]", strings::Candidates))
    });
}

#[test]
fn resolve_other() {

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module-name.f3",
        "" as "src/some-module/module-name/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module-name.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module-name/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module-name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module_name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some-module/module_name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some_module/module-name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some_module/module-name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some_module/module_name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => "src/some_module/module_name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
    ] import Span::new(7, 17); from entry then import Span::new(27, 37); from Span::new(30, 30) => err make_errors!{
        e: e.emit(strings::FailedToReadAllCandidates)
            .detail(Span::new(30, 30), strings::OriginatedHere)
            .help(format!("{} {}", strings::Candidates, concat!("[\"src/some-module/module-name.f3\", \"src/some-module/module-name/index.f3\", ",
                "\"src/some-module/module_name.f3\", \"src/some-module/module_name/index.f3\", \"src/some_module/module-name.f3\", ",
                "\"src/some_module/module-name/index.f3\", \"src/some_module/module_name.f3\", \"src/some_module/module_name/index.f3\"]")))
    });
}
