
// Level0 parser, input file, output exact every char, record line and column

use position::Position;

// V0 token is next char and postion
#[cfg(test)]
#[derive(Debug)]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
    pub next_ch: Option<char>,
    pub next_pos: Position,
}
#[cfg(not(test))]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
    pub next_ch: char,
    pub next_pos: Position,
}

// pos, column and row and next char's
pub struct V0Lexer {
    buf: String,
    buf_index: usize, 
    text_pos: Position, 

    previous_ch: Option<char>,
    previous_pos: Option<Position>,
}

impl From<String> for V0Lexer {
    fn from(content: String) -> V0Lexer {
        V0Lexer {
            buf: content,
            buf_index: 0,
            text_pos: Position::new(),
            previous_ch: None,
            previous_pos: None,
        }
    }
}

impl V0Lexer {
    
    // Text position
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

    // Exact next char, LF and CRLF are acceptable line end
    // So CR is always ignored and LF is returned and position fields are updated
    // Because need next preview, so actually is getting next next char
    pub fn next_char(&mut self) -> Option<V0Token> {

        if self.buf_index_at_end() {
            let previous_pos = match self.previous_pos {
                Some(pos) => pos,
                None => Position::new(),
            };
            match self.previous_ch {
                Some(ch) => {
                    self.previous_ch = None;
                    return Some(V0Token { ch: ch, pos: previous_pos, next_ch: None, next_pos: Position::new() });
                }
                None => return None,
            }
        }

        // next not CR, return None if next is None and iteration will finished
        let mut next_not_cr = self.next_not_carriage_return();

        if self.previous_ch.is_none() {
            match next_not_cr {
                None => {
                    // Next not CR is none and previous is none, this is just empty
                    return None;
                }
                Some(next_not_cr_unwrap) => {
                    self.previous_ch = Some(next_not_cr_unwrap);
                    self.previous_pos = Some(Position { col: 1, row: 1 });
                    self.text_pos = self.text_pos.next_col();
                    next_not_cr = self.next_not_carriage_return();
                }
            }
        }

        self.previous_ch.map(|ch| {
            if ch == '\n' {
                // new line
                self.text_pos = self.text_pos.next_row();
            }
            ch
        });

        let ret_ch = self.previous_ch.unwrap();
        let ret_pos = self.previous_pos.unwrap();
        let ret_next_pos = self.text_pos;
        self.previous_pos = Some(ret_next_pos);
        self.text_pos = self.text_pos.next_col();

        let ret_next_ch = match next_not_cr {
            Some(ch) => { self.previous_ch = Some(ch); Some(ch) },
            None => None,
        };
        
        Some(V0Token { ch: ret_ch, pos: ret_pos, next_ch: ret_next_ch, next_pos: ret_next_pos })
    }

    // Skip a next char, if top level judged to skip after reading next hint
    pub fn skip1(&mut self) {
        let _ = self.next_char();
    }
}

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
    fn v0_test2() {
        use super::V0Lexer;
        use super::V0Token;

        let mut v0lexer = V0Lexer::from("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r".to_owned());

        loop {
            match v0lexer.next_char() {
                None => { perrorln!("Finished"); break; }
                Some(V0Token { ch, pos, next_ch, next_pos }) => { perrorln!("Char {:?} at pos ({}), next is {:?} at pos ({})", ch, pos, next_ch, next_pos); }
            }
        }

        let mut v0lexer =  V0Lexer::from("".to_owned());
        match v0lexer.next_char() {
            None => (),
            Some(t) => panic!("Unexpected v0token: {:?}", t),
        }
    }
}