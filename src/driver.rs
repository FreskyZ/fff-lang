
// Compiler core driver 

use file_map::InputReader;
use lexical::Lexer;
use syntax::parse as syntax_parse;
use codegen::CodeGenerater;

// Handle and print error here
pub fn compile_input(file_name: String) {

    let content = {                                 // Read file
        let mut file_reader = InputReader::new();
        file_reader.read_inputs(vec![&file_name]);

        if !file_reader.get_errors().is_empty() {
            perrorln!("Read file errors: {:?}", file_reader.get_errors());
            return;
        }

        file_reader.into_result()
    };

    let lexer = &mut Lexer::new(&content);          // Lexical parse
    let ast_program = syntax_parse(lexer);          // Syntax parse
    if !lexer.messages().is_empty() {               // Any error is not allowed to continue
        println!("{:?}", lexer.messages());
        return;
    }

    let _generater = CodeGenerater::new(ast_program.unwrap());   // Semantic parse
}

#[cfg(test)]
mod tests {

    #[allow(dead_code)]
    fn xxxxxxxxxxxxxxxxxxx() {
        use super::compile_input;

        compile_input("tests/syntax/hello.sm".to_owned());
    }
}