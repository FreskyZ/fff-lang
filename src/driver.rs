
// Compiler core driver 

use file_map::InputReader;
use lexical::Lexer;
use syntax::get_ast;

// Handle and print error here
pub fn compile_input(file_name: String) {

    let content = {
        let mut file_reader = InputReader::new();
        file_reader.read_inputs(vec![&file_name]);

        if !file_reader.get_errors().is_empty() {
            perrorln!("Read file errors: {:?}", file_reader.get_errors());
            return;
        }

        file_reader.into_result()
    };

    let lexer = &mut Lexer::new(content);
    let _ast = get_ast(lexer);
}

#[cfg(test)]
mod tests {

    #[allow(dead_code)]
    fn xxxxxxxxxxxxxxxxxxx() {
        use super::compile_input;

        compile_input("tests/syntax/hello.sm".to_owned());
    }
}