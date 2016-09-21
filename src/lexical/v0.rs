
// Level0 parser, input file, output exact every char, record line and column

use position::Position;

// V0 token is next char and postion
#[cfg(test)]
#[derive(Debug)]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
}
#[cfg(not(test))]
pub struct V0Token {
    pub ch: char,
    pub pos: Position,
}

// pos, column and row and next char's
pub struct V0Lexer {
    buf: String,
    line_pos: usize,
    pos2: Position,
    to_be_new_line: bool,
}

impl From<String> for V0Lexer {
    fn from(content: String) -> V0Lexer {
        V0Lexer {
            buf: content,
            line_pos: 0,
            pos2: Position::new(),
            to_be_new_line: false,
        }
    }
}

impl V0Lexer {
    
    // Text position
    pub fn position(&self) -> Position { self.pos2 }

    // Exact next char, LF and CRLF are acceptable line end
    // So CR is always ignored and LF is returned and position fields are updated
    pub fn next_char(&mut self) -> Option<V0Token> {

        if self.buf.len() <= self.line_pos {
            return None;
        }

        if self.to_be_new_line {
            // New line 
            self.pos2 = self.pos2.next_row()
        }

        let mut ret_val = string_index!(self.buf, self.line_pos).unwrap();
        let ret_pos = self.pos2;

        if ret_val == '\r' {
            if self.buf.len() <= self.line_pos + 1 {
                // Nothing more buf \r, just return None
                return None;
            }
            // Or else ignore \r
            self.line_pos += 1;
            ret_val = string_index!(self.buf, self.line_pos).unwrap();
        }

        self.line_pos += 1;
        self.pos2 = self.pos2.next_col();
        self.to_be_new_line = ret_val == '\n';

        Some(V0Token { ch: ret_val, pos: ret_pos })
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v0_test() {
        use super::V0Lexer;
        use super::V0Token;

        let mut v0lexer = V0Lexer::from("abc\ndef\r\nasdwe\rq1da\nawsedq".to_owned());

        loop {
            match v0lexer.next_char() {
                None => { perrorln!("Finished"); break; }
                Some(V0Token { ch, pos }) => { perrorln!("Char {:?} at pos ({})", ch, pos); }
            }
        }

        let mut v0lexer =  V0Lexer::from("".to_owned());
        match v0lexer.next_char() {
            None => (),
            Some(t) => panic!("Unexpected v0token: {:?}", t),
        }
    }
}