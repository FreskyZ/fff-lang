
use lexical::Lexer;

// Handle and print error here
pub fn compile_file(file_name: String) {

    let _lexer = match Lexer::from(&file_name) {
        Ok(lexer) => lexer,
        Err(e) => { perrorln!("Lexer error: {:?}", e); return; }
    };
}
