use super::*;

macro_rules! make_source {
    () => {
        make_source!("" as "1")
    };
    ($content:literal) => {
        make_source!($content as "1")
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
    let mut chars = scx.entry("1");
    let id1 = chars.intern_str("abc");
    let id2 = chars.intern_str("123");
    let id3 = chars.intern_str("abc");
    chars.finish();
    assert!(id1 != id2);
    assert!(id2 != id3);
    assert!(id1 == id3);
    assert_eq!(scx.resolve_symbol(id1), "abc");
    assert_eq!(scx.resolve_symbol(id2), "123");
    assert_eq!(scx.resolve_symbol(id3), "abc");
}

#[test]
fn intern_spans() {
    let mut scx = make_source!("eiwubvoqwincleiwubaslckhwoaihecbqvqvqwoliecn");
    let mut chars = scx.entry("1");
    let id1 = chars.intern_span(Span::new(0, 3));
    let id2 = chars.intern_span(Span::new(1, 4));
    let id3 = chars.intern_span(Span::new(13, 16));
    chars.finish();
    assert!(id1 != id2);
    assert!(id2 != id3);
    assert!(id1 == id3);
    assert_eq!(scx.resolve_symbol(id1), "eiwu");
    assert_eq!(scx.resolve_symbol(id2), "iwub");
    assert_eq!(scx.resolve_symbol(id3), "eiwu");
}

#[test]
#[should_panic(expected = "not this file span")]
fn not_this_file_span() {
    let mut scx = make_source!("module 2" as "src/main.f3", "" as "src/2.f3");
    let mut chars = scx.entry("src/main.f3");
    let sym = chars.intern_span(Span::new(7, 7));
    chars.finish();
    let mut chars = scx.import(Span::new(0, 7), sym).unwrap();
    chars.intern_span(Span::new(7, 7));
}

#[test]
#[should_panic(expected = "invalid symbol")]
fn invalid_symbol_id() {
    let mut scx = make_source!();
    let mut chars = scx.entry("1");
    chars.intern_str("abc");
    chars.finish();
    assert_eq!(scx.resolve_symbol(Sym::new(1 << 31)), "abc");
    let _ = scx.resolve_symbol(Sym::new(100));
}

#[test]
#[should_panic(expected = "invalid span")]
fn empty_span1() {
    let mut scx = make_source!();
    let mut chars = scx.entry("1");
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

    ptlc_test_case!{ make_source!("0123\n56\r8\n01234567\n9" as "1", scx in { scx.entry("1").finish(); }),
        [#1: 0 => 1, 1, 1],
        [#2: 2 => 1, 1, 3],
        [#3: 14 => 1, 3, 5],
    }
    ptlc_test_case!{ make_source!("012345678901234567" as "1", scx in { scx.entry("1").finish(); }),
        [#4: 0 => 1, 1, 1],
        [#5: 15 => 1, 1, 16],
    }
    ptlc_test_case!{ make_source!("" as "1", scx in { scx.entry("1").finish(); }),
        [#6: 0 => 1, 1, 1], // both 'EOF is next char of last char' and 'first position is (1, 1)' requires this to be (1, 1)
    }
    ptlc_test_case!{ make_source!("var 你好 =\n 世界;" as "1", scx in { scx.entry("1").finish(); }),
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

    ptlc_test_case!{ make_source!("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r" as "1", scx in { scx.entry("1").finish(); }),
        [#15: 2 => 1, 1, 1],
        [#16: 3 => 1, 1, 2],
        [#17: 4 => 1, 1, 3],
        [#18: 11 => 1, 2, 4],
        [#19: 26 => 1, 4, 2],
        [#20: 30 => 1, 4, 6],
    }

    ptlc_test_case!{ make_source!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n" as "1", scx in { scx.entry("1").finish(); }),
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
    let mut chars = scx.entry("/1.f3");
    let module_name = chars.intern_str("2");
    chars.finish();
    scx.import(Span::new(0, 0), module_name).unwrap().finish();
    ptlc_test_case!{ scx,
        [#27: 0 => 1, 1, 1],
        [#28: 1 => 2, 1, 1],
    }

    //                          1    2        3 4    5         6       7                  1    2       3                 4
    //                          1234 123    4 1 1234 12  45678 1234567 1                  1234 123    4123456      67890 123456
    //                                        1            2           3                          4              5            6
    //                          0123 4567 8 9 0 1234 567 89012 3456789 0              1 2 3456 7890 1 2 345678 9 0 12345 6789012 3 4 5
    let mut scx = make_source!("abc\nd2f\r\r\n\nasd\nwe\rq1da\nawsedq\n" as "/1.f3", "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r" as "/2.f3");
    let mut chars = scx.entry("/1.f3");
    let module_name = chars.intern_span(Span::new(5, 5));
    chars.finish();
    scx.import(Span::new(4, 6), module_name).unwrap().finish();
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

    let scx = make_source!("module 2;" as "src/main.f3", "fn f(){}" as  "src/2.f3", scx in {
        let mut chars = scx.entry("src/main.f3");
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym).unwrap().finish();
    });
    scx.map_span_to_line_column(Span::new(1, 10));
}

#[test]
#[should_panic(expected = "span cross file")]
fn span_cross_file2() {

    let scx = make_source!("module 2;" as "src/main.f3", "fn f(){}" as  "src/2.f3", scx in {
        let mut chars = scx.entry("src/main.f3");
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym).unwrap().finish();
    });

    scx.map_span_to_content(Span::new(1, 10));
}

#[test]
fn span_to_content() {

    macro_rules! test_case {
        ($scx:expr, $([#$caseid:literal: $start_id:expr, $end_id:expr => $expect:expr], )+) => (
            let mut scx = $scx;
            scx.entry("1").finish();
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
    let mut scx = make_source!();
    scx.entry("1").finish();
    scx.map_span_to_content(Span::new(0, 1));
}

#[test]
fn line_to_content() {

    let mut scx = make_source!("0123\n56\r8\n01234567\n9");
    let file_id = scx.entry("1").finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "0123");
    assert_eq!(scx.map_line_to_content(file_id, 2), "56\r8");
    assert_eq!(scx.map_line_to_content(file_id, 3), "01234567");
    assert_eq!(scx.map_line_to_content(file_id, 4), "9");

    let mut scx = make_source!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n");
    let file_id = scx.entry("1").finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "abc");
    assert_eq!(scx.map_line_to_content(file_id, 2), "def\r\r");
    assert_eq!(scx.map_line_to_content(file_id, 3), "");
    assert_eq!(scx.map_line_to_content(file_id, 4), "asd");
    assert_eq!(scx.map_line_to_content(file_id, 5), "we\rq1da");
    assert_eq!(scx.map_line_to_content(file_id, 6), "awsedq");

    let mut scx = make_source!("\nabc\ndef\n");
    let file_id = scx.entry("1").finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "");
    assert_eq!(scx.map_line_to_content(file_id, 2), "abc");
    assert_eq!(scx.map_line_to_content(file_id, 3), "def");
    assert_eq!(scx.map_line_to_content(file_id, 4), "");

    let mut scx = make_source!("abcdef");
    let file_id = scx.entry("1").finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "abcdef");

    let mut scx = make_source!();
    let file_id = scx.entry("1").finish();
    assert_eq!(scx.map_line_to_content(file_id, 1), "");
}

#[test]
#[should_panic(expected = "line number overflow")]
fn line_number_overflow1() {
    let mut scx = make_source!("abc");
    let file_id = scx.entry("1").finish();
    scx.map_line_to_content(file_id, 0);
}

#[test]
#[should_panic(expected = "line number overflow")]
fn line_number_overflow2() {
    let mut scx = make_source!();
    let file_id = scx.entry("1").finish();
    scx.map_line_to_content(file_id, 2);
}

#[test]
fn v0() {

    macro_rules! test_case {
        ($input: expr, $($ch: expr, $char_id: expr,)*) => (
            let mut scx = make_source!($input);
            let mut chars = scx.entry("1");
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

    test_case!{ "", }
}

#[test]
fn span_to_line_column() {

    macro_rules! test_case {
        ($scx:expr, $([#$caseid:literal: $start_id:expr, $end_id:expr => $result:expr],)+) => (
            let scx = $scx;
            $(
                assert_eq!{ scx.map_span_to_line_column(Span::new($start_id, $end_id)), $result, "#{}", $caseid }
            )+
        )
    }

    test_case!{
        make_source!("0123\n56\r8\n01234567\n9" as "1", scx in { scx.entry("1").finish(); }),
        [#1: 0, 2 => (FileId::new(1), 1, 1, 1, 3)],
        [#2: 2, 20 => (FileId::new(1), 1, 3, 4, 2)],
        [#3: 3, 14 => (FileId::new(1), 1, 4, 3, 5)],
        [#4: 2, 19 => (FileId::new(1), 1, 3, 4, 1)],
    }
    test_case!{
        make_source!("012345678901234567" as "1", scx in { scx.entry("1").finish(); }),
        [#5: 0, 18 => (FileId::new(1), 1, 1, 1, 19)],
        [#6: 13, 15 => (FileId::new(1), 1, 14, 1, 16)],
        [#7: 0, 0 => (FileId::new(1), 1, 1, 1, 1)],
    }
    test_case!{
        make_source!("" as "1", scx in { scx.entry("1").finish(); }),
        [#8: 0, 0 => (FileId::new(1), 1, 1, 1, 1)],
    }
    test_case!{
        make_source!("var 你好 =\n 世界;" as "1", scx in { scx.entry("1").finish(); }),
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
        let mut chars = scx.entry("src/main.f3");
        let sym = chars.intern_span(Span::new(7, 7));
        chars.finish();
        scx.import(Span::new(1, 7), sym).unwrap().finish();
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
    ([$($content:literal as $name:literal),+$(,)?] import $module:literal from $span:expr => err) => {{
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3");
        let symbol = chars.intern_str($module);
        chars.finish();
        assert!(scx.import($span, symbol).is_none());
    }};
    ([$($content:literal as $name:literal),+$(,)?] import $module:literal from $span:expr => $path:expr) => {{
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3");
        let symbol = chars.intern_str($module);
        chars.finish();
        let file_id = scx.import($span, symbol).unwrap().finish();
        assert_eq!(scx.files[file_id.0 as usize - 1].path, PathBuf::from($path));
    }};
    ([$($content:literal as $name:literal),+$(,)?] import $module1:literal from entry then import $module2:literal from $span:expr => err) => {{
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3");
        let module_name1 = chars.intern_str($module1);
        let module_name2 = chars.intern_str($module2);
        chars.finish();
        scx.import(Span::new(0, 0), module_name1).unwrap().finish();
        assert!(scx.import($span, module_name2).is_none());
    }};
    ([$($content:literal as $name:literal),+$(,)?] import $module1:literal from entry then import $module2:literal from $span:expr => $path:expr) => {{
        let mut scx = make_source!($($content as $name),+);
        let mut chars = scx.entry("src/main.f3");
        let module_name1 = chars.intern_str($module1);
        let module_name2 = chars.intern_str($module2);
        chars.finish();
        scx.import(Span::new(0, 0), module_name1).unwrap().finish();
        let file_id = scx.import($span, module_name2).unwrap().finish();
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
    ] import "module_name" from Span::new(0, 0) => "src/module-name.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module-name/index.f3",
        "" as "src/module_name.f3",
        "" as "src/module_name/index.f3",
    ] import "module_name" from Span::new(0, 0) => "src/module-name/index.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module_name.f3",
        "" as "src/module_name/index.f3",
    ] import "module_name" from Span::new(0, 0) => "src/module_name.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
        "" as "src/module_name/index.f3",
    ] import "module_name" from Span::new(0, 0) => "src/module_name/index.f3");

    mr_test_case!([
        "module module_name;" as "src/main.f3",
    ] import "module_name" from Span::new(0, 0) => err);
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
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module-name.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module-name/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3"
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module-name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module_name.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
        "" as "src/some-module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module_name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => err);
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
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module-name.f3");
    
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
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module-name/index.f3");
    
    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module_name.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module_name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some-module/module_name/index.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some-module/module_name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module-name.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some_module/module-name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module-name/index.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some_module/module-name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module_name.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some_module/module_name.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
        "" as "src/some_module/module_name/index.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => "src/some_module/module_name/index.f3");

    mr_test_case!([
        "module some_module;" as "src/main.f3",
        "module module_name;" as "src/some-module.f3",
    ] import "some_module" from entry then import "module_name" from Span::new(30, 30) => err);
}
