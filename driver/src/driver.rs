
// Compiler core driver 

use file_map::InputReader;
use lexical::Lexer;
use syntax::parse;
use codegen::generate;
use vm::run;

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

    let lexer = &mut Lexer::new(&content);           // Lexical parse
    let ast_program = parse(lexer);                  // Syntax parse
    if !lexer.messages().is_empty() {                // Any error is not allowed to continue
        println!("{:?}", lexer.messages());
        return;
    }
    let vm_program = generate(ast_program.unwrap()); // Semantic parse
    if !vm_program.msgs.is_empty() {
        println!("{:?}", vm_program.msgs);
        return;
    }

    println!("vm_program: {:?}", vm_program);
    let maybe_exception = run(vm_program);                         // run!
    match maybe_exception {
        Some(exception) => perrorln!("{:?}", exception),
        None => (),
    }

    println!("Byebye");
}

// TODO: 
// Finish lexical move out, use new message format
// Move syntax out
// Move codegen out
// Create optimize
// Move vm out
// Move main out and change this to be a lib