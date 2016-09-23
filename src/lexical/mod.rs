
mod v0;
mod v1;
mod v2;
mod message;

use lexical::v1::V1Lexer;
pub struct Lexer {
    v1: V1Lexer
}

pub use lexical::message::Message;
impl Lexer {
    
    pub fn from(file_name: &str) -> Result<Lexer, Message> {
        use std::fs::File;
        use std::io::Read;

        let mut file: File = try!(File::open(file_name)
                                 .map_err(|e| Message::CannotOpenFile { file_name: file_name.to_owned(), e: e }));

        let mut content = String::new();
        let _read_size = try!(file.read_to_string(&mut content)
                             .map_err(|e| Message::CannotReadFile { file_name: file_name.to_owned(), e: e }));

        Ok(Lexer {
            v1: V1Lexer::from(content),
        })
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn lexer_new() {
        use super::Lexer;
        use super::Message;

        match Lexer::from("something strange.sm") {
            Ok(_) => panic!("Unexpected success"),
            Err(Message::CannotOpenFile { file_name, .. }) => assert_eq!("something strange.sm", file_name),
            Err(e) => panic!("Unexpected other error: {:?}", e),
        }

        match Lexer::from("tests/lexical/1.sm") {
            Ok(_) => (),
            Err(e) => panic!("Unexpected fail: {:?}", e),
        }
    }
}