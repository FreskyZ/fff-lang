
// Input reader, currently read single input file and return content 
// may be used to concat input files or process include files in the future

use std::fmt;
use std::io;

pub enum InputReaderError {
    CannotOpenFile { file_name: String, e: io::Error },
    CannotReadFile { file_name: String, e: io::Error },
}

impl fmt::Debug for InputReaderError {

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
impl_display_by_debug!{ InputReaderError }

pub struct InputReader {
    content: String,
    errs: Vec<InputReaderError>,
}

impl InputReader {

    pub fn new() -> InputReader {
        InputReader { content: String::new(), errs: Vec::new() }
    }

    fn read_single_input(&self, file_name: &str) -> Result<String, InputReaderError> {        
        use std::fs::File;
        use std::io::Read;

        let mut file = File::open(file_name)
            .map_err(|e| InputReaderError::CannotOpenFile { file_name: file_name.to_owned(), e: e })?;
        
        let mut content = String::new();
        let _read_count = file.read_to_string(&mut content) 
            .map_err(|e| InputReaderError::CannotReadFile { file_name: file_name.to_owned(), e: e })?;

        Ok(content)
    }

    pub fn read_inputs(&mut self, file_names: Vec<&str>) {

        for file_name in file_names{
            match self.read_single_input(file_name) {
                Ok(result) => self.content += &result,
                Err(err) => self.errs.push(err),
            }
        }
    }

    pub fn get_result(&self) -> &String {
        &self.content
    }
    pub fn get_errors(&self) -> &Vec<InputReaderError> {
        &self.errs
    }
    pub fn into_result(self) -> String {
        self.content
    }
}

#[cfg(test)]
mod tests {
    // use super::InputReader;
    // use super::InputReaderError;

    #[test]
    #[ignore]
    fn input_reader() {

        // macro_rules! test_case {
        //     ($file: expr, $err: path) => ({
        //         let mut reader = InputReader::new();
        //         reader.read_inputs(vec![$file]);
        //         match reader.get_result() {
        //             Ok(_) => panic!("Unexpectedly success"),
        //             Err(e) => {
        //                 match e {
        //                     $err { file_name, .. } => assert_eq!(file_name, $file),
        //                     e => panic!("Unexpected error: {}", e),
        //                 }
        //             }
        //         }
        //     })
        // }

        // test_case!("tests\\reader\\not_exist.sm", InputReaderError::CannotOpenFile);
        // test_case!("tests\\reader\\cannot_open.sm", InputReaderError::CannotOpenFile);
    }
}