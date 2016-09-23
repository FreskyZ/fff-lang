
// Level3 parser
// input v1
// Hide spaces
// Output 
//      escaped string literal, \n\r\t\"\\, include unicode escape
//      evaluated numeric literal, rust style, _ is ignored, with type postfix, only i32
//      identifier or keyword
//      operators, +, -, *, /, %, +=, -=, *=, /=, %=, .
//      seperators, [, ], {, }, (, ), ;, ,
// May be final layer

use position::Position;
use position::StringPosition;
use lexical::types::Keyword;
use lexical::types::Operator;
use lexical::types::Seperator;

#[derive(Debug)]
pub enum V3Token {
    StringLiteral { value: String, pos: StringPosition },
    NumericLiteral { raw: String, value: i32, pos: StringPosition },
    Identifier { name: String, pos: StringPosition },
    Keyword { subtype: Keyword, pos: StringPosition },
    Operator { subtype: Operator, pos: StringPosition },
    Seperator { subtype: Seperator, pos: StringPosition, }
}

use lexical::v2::V2Lexer;
pub struct V3Lexer {
    v2: V2Lexer,
}

impl From<String> for V3Lexer {
    fn from(content: String) -> V3Lexer {
        V3Lexer { v2: V2Lexer::from(content) }
    }
}

use lexical::message::Message;
use lexical::message::MessageEmitter;

// escape \u{ABCD}
fn escape_string_literal(raw: String) -> String {
    String::new()
}

fn numeric_literal_to_value(raw: String) -> i32 {
    0
}

impl V3Lexer {

    pub fn position(&self) -> Position { self.v2.position() }

    pub fn next(&mut self, messages: &mut MessageEmitter) -> Option<V3Token> {
        use lexical::v2::V2Token;

        None
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn v3_test1() {
        use std::fs::File;
        use std::io::Read;
        use super::V3Lexer;
        use lexical::message::MessageEmitter;

        let file_name = "tests/lexical/2.sm";
        let mut file: File = File::open(file_name).expect("Open file failed");

        let mut content = String::new();
        let _read_size = file.read_to_string(&mut content).expect("Read file failed");
        let mut v3lexer = V3Lexer::from(content);

        let mut messages = MessageEmitter::new();
        loop {
            match v3lexer.next(&mut messages) {
                Some(v3) => perrorln!("{:?}", v3),
                None => break, 
            }
        }
        
        perrorln!("messages: \n{:?}", messages);
    }
}