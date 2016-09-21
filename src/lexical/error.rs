
use std::io;
pub enum Error {

    CannotOpenFile { file_name: String, e: io::Error },
    CannotReadFile { file_name: String, e: io::Error },
}

use std::fmt;
impl fmt::Debug for Error {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Error::*;
        match *self {
            CannotOpenFile { ref file_name, ref e } => {
                write!(f, "Cannot open input file `{}`: {}", file_name, e)
            }
            CannotReadFile { ref file_name, ref e } => {
                write!(f, "Cannot read input file `{}`: {}", file_name, e)
            }
        }
    }
}

impl_display_by_debug!(Error);