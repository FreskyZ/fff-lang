
// Compiler core driver 

use input_reader::InputReader;
use lexical::Lexer;

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

    let _lexer = Lexer::from(content);
}
