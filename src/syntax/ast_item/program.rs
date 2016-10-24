
// Program = [FunctionDef]*

use std::fmt;

use common::From2;
use common::StringPosition;
use common::format_vector_debug;
use common::format_vector_display;

use lexical::Lexer;
use lexical::IToken;

use syntax::ast_item::IASTItem;
use syntax::FunctionDef;
use syntax::SMTypeBase;

#[derive(Eq, PartialEq)]
pub struct Program {
    pub functions: Vec<FunctionDef>,
}
impl fmt::Debug for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_debug(&self.functions, "\n\n"))
    }
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", format_vector_display(&self.functions, "\n\n"))
    }
}

impl Program {

    pub fn get_main(&self) -> Option<&FunctionDef> {

        for func in &self.functions {
            if func.name == "main" && func.ret_type.inner() == &SMTypeBase::Unit {
                return Some(&func);
            }
        }
        None
    } 
}

impl IASTItem for Program {

    fn pos_all(&self) -> StringPosition {

        match self.functions.len() {
            0 => StringPosition::new(),
            1 => self.functions[0].pos_all(),
            _n => StringPosition::from2(self.functions[0].pos_all().start_pos, self.functions.iter().last().unwrap().pos_all().end_pos),
        }
    }

    fn parse(lexer: &mut Lexer, index: usize) -> (Option<Program>, usize) {
        // meet EOF and break, 
        // meet function get None break actually is an unrecoverable and return none
        // recover none function by find next paired '}' and expecting `fn` again
        
        let mut funcs = Vec::new();
        let mut current_len = 0_usize;
        loop {
            if lexer.nth(index + current_len).is_eof() {
                break;
            }
            match FunctionDef::parse(lexer, index + current_len) {
                (Some(func), length) => { 
                    current_len += length;
                    funcs.push(func);
                }
                (None, length) => return (None, current_len + length),
            }
        }

        (Some(Program{ functions: funcs }), current_len)
    }
}

#[cfg(test)]
mod tests {
    use file_map::InputReader;
    use lexical::Lexer;
    use syntax::ast_item::IASTItem;
    use syntax::Program;

    #[test]
    fn ast_all() {
        
        macro_rules! test_case {
            ($file_name: expr) => (
                let mut reader = InputReader::new();
                reader.read_inputs(vec![$file_name]);

                if !reader.get_errors().is_empty() {
                    panic!("errors: {:?}", reader.get_errors());
                }

                let lexer = &mut Lexer::new(reader.into_result());
                let (result, length) = Program::parse(lexer, 0);

                perrorln!("Debug: {:?}", result);
                perrorln!("errors: {:?}", lexer.messages());
                perrorln!("Display: {}, {}", result.unwrap(), length);
            )
        }

        test_case!("tests/syntax/hello.sm");
        // test_case!("tests/syntax/list.sm");
        // test_case!("tests/syntax/prime.sm");
        // test_case!("tests/syntax/string.sm");
    }

    #[test]
    #[ignore]
    fn ast_interactive() {
        use std::io::stdin;

        loop {
            let mut buf = String::new();

            perrorln!("Input:");
            match stdin().read_line(&mut buf) {
                Ok(_) => (),
                Err(_) => break,
            }

            if buf != "break\r\n" {
                let lexer = &mut Lexer::new(buf);
                let (result, length) = Program::parse(lexer, 0);
                perrorln!("Debug: ({:?}, {})", result, length);
                match result {
                    Some(result) => perrorln!("Display: {}", result),
                    None => perrorln!("messages: {:?}", lexer.messages()),
                }
            } else {
                break;
            }
        }
    }
}