
// Input reader, currently read single input file and return content 
// may be used to concat input files or process include files in the future

use std::fmt;
use std::io;

pub enum InputReaderError {
    CannotOpenFile { file_name: String, e: io::Error },
    CannotReadFile { file_name: String, e: io::Error },
}

impl fmt::Display for InputReaderError {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InputReaderError::CannotOpenFile { ref file_name, ref e } => {
                write!(f, "Error: cannot open input file `{}`: {}", file_name, e)
            }
            InputReaderError::CannotReadFile { ref file_name, ref e } => {
                write!(f, "Error: cannot read input file `{}`: {}", file_name, e)
            }
        }
    }
}

pub struct InputReader {
    result: Result<String, InputReaderError>
}

impl InputReader {

    pub fn new() -> InputReader {
        InputReader { result: Ok(String::new()) }
    }

    fn input(&self, file_name: &str) -> Result<String, InputReaderError> {        
        use std::fs::File;
        use std::io::Read;

        let mut file = try!(File::open(file_name)
            .map_err(|e| InputReaderError::CannotOpenFile { file_name: file_name.to_owned(), e: e }));
        
        let mut content = String::new();
        let _read_count = try!(file.read_to_string(&mut content) 
            .map_err(|e| InputReaderError::CannotReadFile { file_name: file_name.to_owned(), e: e }));

        Ok(content)
    }

    pub fn add_input_file(&mut self, file_name: &str) {
        self.result = self.input(file_name);
    }

    pub fn get_result(self) -> Result<String, InputReaderError> {
        self.result
    }
}

#[cfg(test)]
mod tests {
    use super::InputReader;
    use super::InputReaderError;

    #[test]
    fn input_reader() {

        macro_rules! test_case {
            ($file: expr, $err: path) => ({
                let mut reader = InputReader::new();
                reader.add_input_file($file);
                match reader.get_result() {
                    Ok(_) => panic!("Unexpectedly success"),
                    Err(e) => {
                        match e {
                            $err { file_name, .. } => assert_eq!(file_name, $file),
                            e => panic!("Unexpected error: {}", e),
                        }
                    }
                }
            })
        }

        test_case!("tests\\reader\\not_exist.sm", InputReaderError::CannotOpenFile);
        test_case!("tests\\reader\\cannot_open.sm", InputReaderError::CannotOpenFile);
    }
}