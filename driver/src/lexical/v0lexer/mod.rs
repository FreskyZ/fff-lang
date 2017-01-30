
// Level0 parser, input file, output exact every char, record line and column
// TODO: use chars instead of buf: String to index, this will support Chinese identifiers

use std::str::Chars;
use lexical_pos::Position;
use message::MessageEmitter;
use lexical::buf_lexer::IDetailLexer;
use lexical::buf_lexer::BufToken;
use lexical::buf_lexer::BufLexer;

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
pub struct V0Lexer<'chs> {
    chars: Chars<'chs>,
    text_pos: Position,

    previous_is_new_line: bool,
}

impl<'chs> From<Chars<'chs>> for V0Lexer<'chs> {
    fn from(content_chars: Chars<'chs>) -> V0Lexer {
        V0Lexer {
            chars: content_chars,
            text_pos: Position{ row: 1, col: 0 },
            previous_is_new_line: false,
        }
    }
}

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

impl<'chs> IDetailLexer<'chs, V0Token> for V0Lexer<'chs> {

    fn position(&self) -> Position { self.text_pos }

    // Exact next char, LF and CRLF are acceptable line end
    // So CR is always ignored and LF is returned and position fields are updated
    //
    // Because need next preview, so actually is getting next next char
    //
    // Provide None|EOF a special virtual pos 
    fn next(&mut self, _messages: &mut MessageEmitter) -> Option<V0Token> {
        
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

pub type BufV0Token = BufToken<V0Token>;
pub type BufV0Lexer<'chs> = BufLexer<V0Lexer<'chs>, V0Token>;

// Visitors for test
#[cfg(test)]
pub fn v0_next_no_cr_visitor(lexer: &mut V0Lexer) -> Option<char> {
    lexer.next_not_carriage_return()
}

#[cfg(test)]
mod tests {

    #[test]
    fn v0_test1() {
        use super::V0Lexer;
        use super::v0_next_no_cr_visitor;

        let mut v0lexer = V0Lexer::from("\rabc\r\ref\r".chars());
        let mut result = Vec::new();
        loop {
            match v0_next_no_cr_visitor(&mut v0lexer) {
                None => break,
                Some(ch) => result.push(ch),
            }
        }
        assert_eq!(result, vec!['a', 'b', 'c', 'e', 'f']);

        let mut v0lexer = V0Lexer::from("abc\r\re\rf".chars());
        let mut result = Vec::new();
        loop {
            match v0_next_no_cr_visitor(&mut v0lexer) {
                None => break,
                Some(ch) => result.push(ch),
            }
        }
        assert_eq!(result, vec!['a', 'b', 'c', 'e', 'f']);

        let mut v0lexer = V0Lexer::from("\r\r\r\r".chars());
        match v0_next_no_cr_visitor(&mut v0lexer) {
            None => (),
            Some(t) => panic!("Unexpected v0token: {:?}", t),
        }
    }

    #[test]
    #[allow(unused_mut)]
    fn v0_test2() {
        use super::V0Lexer;
        use super::V0Token;
        use lexical_pos::Position;
        use lexical::buf_lexer::IDetailLexer;
        use message::MessageEmitter;

        macro_rules! test_case {
            ($input: expr, $($ch: expr, $row: expr, $col: expr, )*) => (
                let mut v0lexer = V0Lexer::from($input.chars());
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
        use message::MessageEmitter;
        
        let mut bufv0 = BufV0Lexer::from("\r\rabc\ndef\r\r\nasdwe\r\r\rq1da\nawsedq\r\r\r".chars());
        let mut dummy = MessageEmitter::new();
        loop {
            match bufv0.next(&mut dummy) {
                Some(bufv0) => perrorln!("{:?}", bufv0),    
                None => break,
            }
        }
        
        let mut bufv0 = BufV0Lexer::from("abc\ndef\r\r\n\nasd\nwe\rq1da\nawsedq\n".chars());
        let mut dummy = MessageEmitter::new();
        loop {
            match bufv0.next(&mut dummy) {
                Some(bufv0) => perrorln!("{:?}", bufv0),    
                None => break,
            }
        }
    }
}