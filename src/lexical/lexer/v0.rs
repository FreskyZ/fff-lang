
// Level0 parser, input file, output exact every char, record line and column

use common::Position;
use lexical::ILexer;
use message::MessageEmitter;

// V0 token is next char and postion
#[cfg(test)]
#[derive(Eq, PartialEq, Clone)]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
}
#[cfg(not(test))]
#[derive(Clone)]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
}

#[cfg(test)]
use std::fmt;
#[cfg(test)]
impl fmt::Debug for V0Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Char {:?} at {:?}", self.ch, self.pos)
    }
}

// pos, column and row and next char's
pub struct V0Lexer {
    buf: String,
    buf_index: usize, 
    text_pos: Position,

    previous_is_new_line: bool,
}

impl From<String> for V0Lexer {
    fn from(content: String) -> V0Lexer {
        V0Lexer {
            buf: content,
            buf_index: 0,
            text_pos: Position{ row: 1, col: 0 },
            previous_is_new_line: false,
        }
    }
}

impl V0Lexer {
    
    pub fn position(&self) -> Position { self.text_pos }

    fn buf_index_at_end(&self) -> bool { self.buf.len() <= self.buf_index }

    // Update buf_index to get next not CR
    // Make called completely cannot see \r
    // e.g.
    // previous state:  idx
    //    after state:      idx, return 'a'
    // previous state:              idx
    //    after state:                            idx, return 'd'
    //            buf:   a | b | c | d | \r | \r | e | f | \r | EOF
    // previous state:                                idx 
    //    after state:                                         idx, return 'f'
    // previous state:                                         idx
    //    after state:                                         idx, return None
    // 
    // A special e.g.
    //            buf:  \r | b | c | d | \r | \r | e | f | \r | EOF
    // previous state: idx 
    //    after state:          idx, return *'b'*
    //
    // A more special e.g.
    //            buf:  \r | \r | \r | EOF
    // previous state: idx 
    //    after state:                 idx, return None
    fn next_not_carriage_return(&mut self) -> Option<char> {
        // `perrorln!("1: ...")`: some sort of coverage check...    

        if self.buf_index_at_end() {
            // perrorln!("1: called when index at end, return None");
            return None;
        }

        let mut ret_val = string_index!(self.buf, self.buf_index).unwrap();
        while ret_val == '\r' { // If start idx is \r, see special e.g.
            self.buf_index += 1;
            // perrorln!("2: ret_val is \\r, trying find next not \\r at index {}", self.buf_index);
            if self.buf_index_at_end() {
                // Find \r to end
                // perrorln!("3: find \\r to end, return None with index at {}", self.buf_index);
                return None;
            }
            ret_val = string_index!(self.buf, self.buf_index).unwrap();
        }

        // perrorln!("4: Find something not \\r as ret_val, ret_val is {:?}", ret_val);
        self.buf_index += 1;
        if self.buf_index_at_end() {
            // perrorln!("5: but next is end, so return {:?}", ret_val);
            return Some(ret_val);
        }
        while string_index!(self.buf, self.buf_index).unwrap() == '\r' {
            // perrorln!("6: finding next not \\r index, index {} is still \\r", self.buf_index);
            self.buf_index += 1;
            if self.buf_index_at_end() {
                // perrorln!("7: find next not \\r to end, break with buf_index: {}", self.buf_index);
                break;
            }
        }

        // perrorln!("8: next not carriage return result is {:?}", ret_val);
        Some(ret_val)
    }
}

impl ILexer<V0Token> for V0Lexer {

    // Exact next char, LF and CRLF are acceptable line end
    // So CR is always ignored and LF is returned and position fields are updated
    //
    // Because need next preview, so actually is getting next next char
    //
    // Provide None|EOF a special virtual pos 
    fn next(&mut self, _emitter: &mut MessageEmitter) -> Option<V0Token> {
        
        // next not cr will return None if buf index at end
        self.next_not_carriage_return().map(|ch|{
            if self.previous_is_new_line {
                self.text_pos = self.text_pos.next_row();
            } else {
                self.text_pos = self.text_pos.next_col();
            }

            self.previous_is_new_line = ch == '\n';
            V0Token{ ch: ch, pos: self.text_pos }
        }).or_else(||{
            self.text_pos = self.text_pos.next_col();
            None
        })
    }
}

use lexical::lexer::buf_lexer::BufToken;
use lexical::lexer::buf_lexer::BufLexer;
pub type BufV0Token = BufToken<V0Token>;
pub type BufV0Lexer = BufLexer<V0Lexer, V0Token>;

// Visitors for test
#[cfg(test)]
pub fn v0_next_no_cr_visitor(lexer: &mut V0Lexer) -> (Option<char>, usize) {
    (lexer.next_not_carriage_return(), lexer.buf_index)
}

#[cfg(test)]
mod tests {

    #[test]
    fn v0_test1() {
        use super::V0Lexer;
        use super::v0_next_no_cr_visitor;

        let mut v0lexer = V0Lexer::from("\rabc\r\ref\r".to_owned());

        let mut result = Vec::new();
        loop {
            match v0_next_no_cr_visitor(&mut v0lexer) {
                (None, _) => break,
                (Some(ch), index) => result.push((ch, index)),
            }
        }
        assert_eq!(result, vec![('a', 2), ('b', 3), ('c', 6), ('e', 7), ('f', 9)]);

        let mut v0lexer = V0Lexer::from("abc\r\re\rf".to_owned());

        let mut result = Vec::new();
        loop {
            match v0_next_no_cr_visitor(&mut v0lexer) {
                (None, _) => break,
                (Some(ch), index) => result.push((ch, index)),
            }
        }
        assert_eq!(result, vec![('a', 1), ('b', 2), ('c', 5), ('e', 7), ('f', 8)]);

        let mut v0lexer = V0Lexer::from("\r\r\r\r".to_owned());
        match v0_next_no_cr_visitor(&mut v0lexer) {
            (None, index) => assert_eq!(index, 4),
            (Some(t), _) => panic!("Unexpected v0token: {:?}", t),
        }
    }

    #[test]
    #[allow(unused_mut)]
    fn v0_test2() {
        use super::V0Lexer;
        use super::V0Token;
        use common::Position;
        use lexical::ILexer;
        use message::MessageEmitter;

        macro_rules! test_case {
            ($input: expr, $($ch: expr, $row: expr, $col: expr, )*) => (
                let mut v0lexer = V0Lexer::from($input.to_owned());
                let mut v0s = Vec::new();
                let mut dummy = MessageEmitter::new();
                loop {
                    match v0lexer.next(&mut dummy) {
                        Some(v0) => v0s.push(v0),
                        None => break,
                    }
                }

                let mut expects = Vec::new();
                $(
                    expects.push(V0Token { ch: $ch, pos: Position { row: $row, col: $col } });
                )*
                
                assert_eq!(v0s, expects);
            )
        }

        test_case!("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r",
            'a', 1, 1,
            'b', 1, 2,
            'c', 1, 3,
            '\n', 1, 4,
            'd', 2, 1,
            'e', 2, 2,
            'f', 2, 3,
            '\n', 2, 4,
            'a', 3, 1,
            's', 3, 2,
            'd', 3, 3,
            'w', 3, 4,
            'e', 3, 5,
            'q', 3, 6,
            '1', 3, 7,
            'd', 3, 8,
            'a', 3, 9,
            '\n', 3, 10,
            'a', 4, 1,
            'w', 4, 2,
            's', 4, 3,
            'e', 4, 4,
            'd', 4, 5,
            'q', 4, 6,
        );

        test_case!("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n",
            'a', 1, 1,
            'b', 1, 2,
            'c', 1, 3,
            '\n', 1, 4,
            'd', 2, 1,
            'e', 2, 2,
            'f', 2, 3,
            '\n', 2, 4,
            '\n', 3, 1,
            'a', 4, 1,
            's', 4, 2,
            'd', 4, 3,
            '\n', 4, 4,
            'w', 5, 1,
            'e', 5, 2,
            'q', 5, 3,
            '1', 5, 4,
            'd', 5, 5,
            'a', 5, 6,
            '\n', 5, 7,
            'a', 6, 1,
            'w', 6, 2,
            's', 6, 3,
            'e', 6, 4,
            'd', 6, 5,
            'q', 6, 6,
            '\n', 6, 7,
        );

        test_case!("", );
    }

    #[test]
    fn v0_buf() {
        use super::BufV0Lexer;
        use super::V0Lexer;
        use message::MessageEmitter;
        
        let mut bufv0 = BufV0Lexer::from(V0Lexer::from("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r".to_owned()));
        let mut dummy = MessageEmitter::new();
        loop {
            match bufv0.next(&mut dummy) {
                Some(bufv0) => perrorln!("{:?}", bufv0),    
                None => break,
            }
        }
        
        let mut bufv0 = BufV0Lexer::from(V0Lexer::from("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n".to_owned()));
        let mut dummy = MessageEmitter::new();
        loop {
            match bufv0.next(&mut dummy) {
                Some(bufv0) => perrorln!("{:?}", bufv0),    
                None => break,
            }
        }
    }
}