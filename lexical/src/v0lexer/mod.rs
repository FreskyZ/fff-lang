
// Level0 parser, input file, output exact every char, record line and column

use std::str::Chars;
use codepos::Position;
use codepos::StringPosition;
use message::MessageCollection;
use super::buf_lexer::ILexer;

// pos, column and row and next char's
pub struct V0Lexer<'chs> {
    chars: Chars<'chs>,
    text_pos: Position,

    previous_is_new_line: bool,
    completed: bool,
}

pub const EOFCHAR: char = 0u8 as char;

impl<'chs> V0Lexer<'chs> {
    
    // get next char not CR
    // Make called completely cannot see \r
    fn next_not_carriage_return(&mut self) -> Option<char> {

        loop {
            match self.chars.next() {
                Some('\r') => continue,
                Some(ch) => return Some(ch),
                None => return None,
            }
        }
    }
}
impl<'chs> ILexer<'chs, char> for V0Lexer<'chs> {

    fn new(content_chars: Chars<'chs>, _: &mut MessageCollection) -> V0Lexer<'chs> {
        V0Lexer {
            chars: content_chars,
            text_pos: make_pos!(1, 0),
            previous_is_new_line: false,
            completed: false,
        }
    }

    // Exact next char, LF and CRLF are acceptable line end
    // So CR is always ignored and LF is returned and position fields are updated
    //
    // Because need next preview, so actually is getting next next char
    //
    // Provide None|EOF a special virtual pos 
    fn next(&mut self, _: &mut MessageCollection) -> (char, StringPosition) {
        
        // next not cr will return None if buf index at end
        match self.next_not_carriage_return() {
            Some(ch) => {
                if self.previous_is_new_line {
                    self.text_pos = self.text_pos.next_row();
                } else {
                    self.text_pos = self.text_pos.next_col();
                }

                self.previous_is_new_line = ch == '\n';
                (ch, StringPosition::double(self.text_pos))
            }
            None => {
                if !self.completed {
                    self.text_pos = self.text_pos.next_col();
                    self.completed = true;
                }
                (EOFCHAR, StringPosition::double(self.text_pos))
            }
        }
    }
}

#[cfg(test)]
#[test]
fn v0_not_cr() {

    let mut v0lexer = V0Lexer::new("\rabc\r\ref\r".chars(), &mut MessageCollection::new());
    let mut result = Vec::new();
    loop {
        match v0lexer.next_not_carriage_return() {
            None => break,
            Some(ch) => result.push(ch),
        }
    }
    assert_eq!(result, vec!['a', 'b', 'c', 'e', 'f']);

    let mut v0lexer = V0Lexer::new("abc\r\re\rf".chars(), &mut MessageCollection::new());
    let mut result = Vec::new();
    loop {
        match v0lexer.next_not_carriage_return() {
            None => break,
            Some(ch) => result.push(ch),
        }
    }
    assert_eq!(result, vec!['a', 'b', 'c', 'e', 'f']);

    let mut v0lexer = V0Lexer::new("\r\r\r\r".chars(), &mut MessageCollection::new());
    match v0lexer.next_not_carriage_return() {
        None => (),
        Some(t) => panic!("Unexpected v0token: {:?}", t),
    }
}

#[cfg(test)]
#[test]
fn v0_base() {

    macro_rules! test_case {
        ($input: expr, $($ch: expr, $row: expr, $col: expr, )*) => (
            let mut dummy = MessageCollection::new();
            let mut v0lexer = V0Lexer::new($input.chars(), &mut dummy);
            let mut v0s = Vec::new();
            loop {
                match v0lexer.next(&mut dummy) {
                    (EOFCHAR, _) => break,
                    v0 => v0s.push(v0),
                }
            }

            let expects = &mut Vec::new();
            $(
                expects.push(($ch, make_str_pos!($row, $col, $row, $col)));
            )*
            
            assert_eq!(&v0s, expects);
        )
    }

    test_case!("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
        'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
        'a', 3, 1, 's', 3, 2, 'd', 3, 3, 'w', 3, 4, 'e', 3, 5, 'q', 3, 6, '1', 3, 7, 'd', 3, 8, 'a', 3, 9, '\n', 3, 10,
        'a', 4, 1, 'w', 4, 2, 's', 4, 3, 'e', 4, 4, 'd', 4, 5, 'q', 4, 6,
    );

    test_case!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
        'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
        '\n', 3, 1,
        'a', 4, 1, 's', 4, 2, 'd', 4, 3, '\n', 4, 4,
        'w', 5, 1, 'e', 5, 2, 'q', 5, 3, '1', 5, 4, 'd', 5, 5, 'a', 5, 6, '\n', 5, 7,
        'a', 6, 1, 'w', 6, 2, 's', 6, 3, 'e', 6, 4, 'd', 6, 5, 'q', 6, 6, '\n', 6, 7,
    );

    test_case!("", );
}

#[cfg(test)]
#[test]
fn v0_buf() {

    use super::buf_lexer::BufLexer;

    enum State {
        First,
        Second((char, StringPosition)),
        Normal((char, StringPosition, char, StringPosition)),
    }

    macro_rules! test_case {
        ($program: expr, $($ch: expr, $row: expr, $col: expr, )*) => ({
            let messages = &mut MessageCollection::new();
            let mut bufv0lexer = BufLexer::<V0Lexer, char>::new($program.chars(), messages);
            let mut bufv0s = Vec::new();
            loop {
                bufv0lexer.move_next(messages);
                match bufv0lexer.current_with_preview2() {
                    (&EOFCHAR, _, _, _, _, _) => break,
                    (&ch1, pos1, &ch2, pos2, &ch3, pos3) => bufv0s.push((ch1, pos1, ch2, pos2, ch3, pos3)),
                }
            }

            let mut state = State::First;
            let mut expects = Vec::new();
            $(
                match state {
                    State::First => state = State::Second(($ch, make_str_pos!($row, $col, $row, $col))),
                    State::Second((ch, strpos)) => state = State::Normal((ch, strpos, $ch, make_str_pos!($row, $col, $row, $col))),
                    State::Normal((prev2_ch, prev2_strpos, prev_ch, prev_strpos)) => {
                        expects.push((prev2_ch, prev2_strpos, prev_ch, prev_strpos, $ch, make_str_pos!($row, $col, $row, $col)));
                        state = State::Normal((prev_ch, prev_strpos, $ch, make_str_pos!($row, $col, $row, $col)));
                    }
                }
            )*

            match state {
                State::First => (),                 // test case is empty
                State::Second((ch, strpos)) => {    // push rest
                    expects.push((ch, strpos, EOFCHAR, StringPosition::double(strpos.start_pos().next_col()), EOFCHAR, StringPosition::double(strpos.start_pos().next_col())));
                }
                State::Normal((prev2_ch, prev2_strpos, prev_ch, prev_strpos)) => { // push rest2
                    expects.push((prev2_ch, prev2_strpos, prev_ch, prev_strpos, EOFCHAR, StringPosition::double(prev_strpos.start_pos().next_col())));
                    expects.push((prev_ch, prev_strpos, EOFCHAR, StringPosition::double(prev_strpos.start_pos().next_col()), EOFCHAR, StringPosition::double(prev_strpos.start_pos().next_col())));
                }
            }
            if bufv0s != expects {
                panic!("bufv0s not expected: {:?}", bufv0s.into_iter().zip(expects.into_iter())
                    .collect::<Vec<((char, StringPosition, char, StringPosition, char, StringPosition), (char, StringPosition, char, StringPosition, char, StringPosition))>>());
            }
            assert_eq!(bufv0s, expects);
        })
    }

    test_case!{ "\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
        'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
        'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
        'a', 3, 1, 's', 3, 2, 'd', 3, 3, 'w', 3, 4, 'e', 3, 5, 'q', 3, 6, '1', 3, 7, 'd', 3, 8, 'a', 3, 9, '\n', 3, 10,
        'a', 4, 1, 'w', 4, 2, 's', 4, 3, 'e', 4, 4, 'd', 4, 5, 'q', 4, 6,
    }    
    
    test_case!{ "abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
        'a', 1, 1, 'b', 1, 2, 'c', 1, 3, '\n', 1, 4,
        'd', 2, 1, 'e', 2, 2, 'f', 2, 3, '\n', 2, 4,
        '\n', 3, 1,
        'a', 4, 1, 's', 4, 2, 'd', 4, 3, '\n', 4, 4,
        'w', 5, 1, 'e', 5, 2, 'q', 5, 3, '1', 5, 4, 'd', 5, 5, 'a', 5, 6, '\n', 5, 7,
        'a', 6, 1, 'w', 6, 2, 's', 6, 3, 'e', 6, 4, 'd', 6, 5, 'q', 6, 6, '\n', 6, 7,
    }
}