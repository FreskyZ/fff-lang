
// Level1 parser
// input v0
// remove comment
// Pass through pos and hide lineend
// output string literal, numeric literal, identifier and otherchar

// Grammar: Content
// Quote -> `"`
// Slash -> `/`
// BackSlash -> `\`
// Star -> `*`
// LineEnd -> `\n`
// OtherChar
// Comment1 -> `//` [OtherChar|Slash|BackSlash|(Star~Slash)|Quote]* LineEnd
// Comment2 -> `/*` [OtherChar|Slash|BackSlash|(Star~Slash)|Quote]* `*/`
// StringLiteral -> `"` [OtherChar|Slash|BackSlash~Quote|Star]* `"`
// NumericLiteral -> ...

use position::Position;
pub enum V1Token {
    StringLiteral { raw: String, start_pos: Position, end_pos: Position },
    NumericLiteral { raw: String, start_pos: Position, end_pos: Position },
    Identifier { raw: String, start_pos: Position, end_pos: Position },  // Any thing of [_a-zA-Z][_a-zA-Z0-9]*
    OtherChar { raw: char, pos: Position }, // space, parenthenes, comma, etc.
}

use lexical::v0::V0Lexer;
use lexical::v0::V0Token;
pub struct V1Lexer {
    v0: V0Lexer,
    when_except_comment_next_is: Option<V0Token>,
}
impl From<String> for V1Lexer {

    fn from(content: String) -> V1Lexer {
        V1Lexer { 
            v0: V0Lexer::from(content),
            when_except_comment_next_is: None, 
        }
    }
}

mod interest {
    pub const QUOTE: char = '"';
    pub const SLASH: char = '/';
    pub const BACKSLASH: char = '\\';
    pub const STAR: char = '*';
    pub const LINEEND: char = '\n';
}

// immediately return none if return none, return value if need
macro_rules! try_next {
    ($opt: expr) => (
        match $opt {
            Some(v0) => v0,
            None => { return None; }
        }
    )
}

impl V1Lexer {

    pub fn position(&self) -> Position { self.v0.position() }

    // input v0, output stringliteral or otherchar without comment
    fn next_except_comment(&mut self) -> Option<V1Token> {

        // First there is quote, and anything inside is regarded as string literal, include `\n` as real `\n`
        // and then outside of quote pair there is comments, anything inside comment, // and /n, or /* and */ is regarded as comment

        let V0Token { ch: current_char, pos: start_pos } = if self.when_except_comment_next_is.is_some() {
            let temp = self.when_except_comment_next_is.unwrap();
            self.when_except_comment_next_is = None;
            temp
        } else {
            try_next!(self.v0.next_char())
        };
        
        match (current_char == interest::SLASH, current_char == interest::QUOTE) {
            (false, false) => { // Nothing special, just return otherchar
                return Some(V1Token::OtherChar { raw: current_char, pos: start_pos });
            },
            (true, false) => { // `/`,  check `*` or `/` after
                let V0Token { ch: next_char, pos: next_pos } = try_next!(self.v0.next_char());
                match (next_char == interest::SLASH, next_char == interest::STAR) {
                    (false, false) => { // Nothing special, a single `/`
                        self.when_except_comment_next_is = V0Token { ch: next_char, pos: next_pos };
                        return Some(V1Token::OtherChar { raw: current_char, pos: start_pos });
                    }
                    (true, false) => { // Go into `//.../n`
                        loop {
                            let V0Token { ch: next_char, pos: next_pos } = try_next!(self.v0.next_char()) {
                                if next_char != interest::LINEEND {
                                    continue;
                                } else {
                                    
                                }
                            }
                        } 
                    }
                    (false, true) => { // Go into `/* ... */`

                    }
                    (true, true) => {
                        panic!("World corrupted");
                    }
                }
                None
            }
            (false, true) => { // `"`, wait for next not escaped `"`
                None
            }
            (true, true) => {
                panic!("World corrupted");
            }
        }
    }

    // input stringliteral or otherchar without comment, output all kind of V1Token
    pub fn next(&mut self) -> Option<V1Token> {

        None
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v1_test() {
        use super::V1Token;
        use super::V1Lexer;

        
        let v1lexer = V1Lexer::from();
        
    }
}