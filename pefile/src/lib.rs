///! fff-lang
///!
///! pefile, from native program with its attributes and instructions to binary

extern crate util;

mod errors;
mod dos_header;
mod file_headers;

use errors::Error;
use dos_header::get_default_dos_header;
use dos_header::check_dos_header;

pub use file_headers::FileHeader;
pub use file_headers::FileAttributes;
pub use file_headers::FileHeaderDirectoryIndex;
pub use file_headers::DataDirecotry;

pub struct PEFile {
    // nt_headers: ImageNTHeader,
}
impl PEFile {
    
    pub fn new() {
    }

    pub fn from_bytes(bytes: &[u8]) -> Result<PEFile, Error> {
        let _file_header_offset = check_dos_header(bytes)?;
        Ok(PEFile{})
    }
    pub fn into_bytes(self) {
        let _dos_header_bytes = get_default_dos_header();
    }
}

#[cfg(test)] #[test]
fn gen_hello_world_test() {
    use std::fs::File;
    use std::io::Write;
    use dos_header::get_default_dos_header;

    let dos_header_bytes = get_default_dos_header();

    let file_header = FileHeader::new();

    let file_header_bytes = file_header.into_bytes();

    let mut file = File::create(r"..\tests\pefile\gened-hello.exe").expect("Create file failed");
    let _ = file.write(&dos_header_bytes[..]).expect("write dos header failed");
    let _ = file.write(&file_header_bytes[..]).expect("write nt header failed");
    let _ = file.flush().expect("flush file failed");
}

#[cfg(test)] #[test]
fn it_works() {
    // first expect

    // let statics = ["Helloworld"];
    // let imports = [
    //     ("kernel32", "GetConsoleHandle"),
    //     ("kernel32", "WriteConsole"),
    //     ("kernel32", "ExitProcess"),
    // ];
    // let code = [
    //     MOV(RBP, RSP),
    //     PUSH(RBP),
    //     MOV(RAX, -11),
    //     CALL(imports[0]),
    //     MOV(RAX, [RBP - 4]),
    //     PUSH(statics[0]),
    //     PUSH(11),
    //     PUSH([RBP - 4]),
    //     CALL(imports[1]),
    //     XOR(RAX, RAX),
    //     CALL(imports[2]),
    // ]
    // let pefile = PEFile::new(statics, imports, code);
    // let file = File::create("hello.exe").expect("create failed");
    // pefile.write(file);
}

// TODO: 
// try to finish file header part of helloworld exe generation
// continue working on from bytes but do not check too much, only check is PE and is x64
//     no one will give you a file with exe extension and MZ header but with very very special this and that byte not correct