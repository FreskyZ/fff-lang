
// Compiler core driver 

use file_map::InputReader;
use message::MessageEmitter;
use lexical::Lexer;
use syntax::get_ast;

// Handle and print error here
pub fn compile_input(file_name: String) {

    let content = {
        let mut file_reader = InputReader::new();
        file_reader.add_input_file(&file_name);

        match file_reader.get_result() {
            Ok(result) => result,
            Err(e) => { perrorln!("{}", e); return; } 
        }
    };

    let messages = MessageEmitter::new();
    let lexer = &mut Lexer::from(content, messages);
    let _ast = get_ast(lexer);
}

#[cfg(test)]
mod tests {

    #[test]
    #[ignore]
    fn xxxxxxxxxxxxxxxxxxx() {
        use super::compile_input;

        compile_input("tests/syntax/hello.sm".to_owned());
    }
}