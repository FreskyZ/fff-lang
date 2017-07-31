///! fff-lang
///!
///! codemap/error
///! codemap has its own error type because messages based on codemap for locating source code string

use std::fmt;
use std::cmp;
use std::io::Error as IOError;
use std::path::PathBuf;

pub enum CodeMapError {
    CannotOpenFile(PathBuf, IOError),
    CannotReadFile(PathBuf, IOError),
}
impl fmt::Debug for CodeMapError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &CodeMapError::CannotOpenFile(ref file_name, ref ioe) => write!(f, "Cannot open file: {}: {}", file_name.display(), ioe),
            &CodeMapError::CannotReadFile(ref file_name, ref ioe) => write!(f, "Cannot read file: {}: {}", file_name.display(), ioe),
        }
    }
}
impl cmp::PartialEq for CodeMapError {
    fn eq(&self, rhs: &CodeMapError) -> bool {
        match (self, rhs) {
            (&CodeMapError::CannotOpenFile(ref name1, ref e1), &CodeMapError::CannotOpenFile(ref name2, ref e2)) => 
                name1 == name2 && format!("{:?}", e1) == format!("{:?}", e2),
            (&CodeMapError::CannotReadFile(ref name1, ref e1), &CodeMapError::CannotReadFile(ref name2, ref e2)) => 
                name1 == name2 && format!("{:?}", e1) == format!("{:?}", e2),
            _ => false,
        }
    }
}
impl cmp::Eq for CodeMapError {
}