#![allow(dead_code)]
#![allow(non_upper_case_globals)]

#[macro_use]
extern crate bitflags;

pub mod dos_header;
pub mod util;

#[derive(Copy, Clone)]
pub struct DataDirecotry{
    pub virtual_address: u32,
    pub size: u32,
}

bitflags! {
    flags PEFileAttributes: u16 {
        // deprecated
        const LineNumberStripped = 0x0004,          // deprecated
        const LocalSymbolStripped = 0x0008,         // deprecated
        const AggressiveTrimWorkingSet = 0x0010,    // deprecated
        const ByteOrderLittleEndian = 0x0080,       // deprecated
        const ByteOrderBigEndian = 0x8000,          // deprecated

        // reserved
        const _Reserved1 = 0x0040,                  // reserved

        // don't when to use
        const RelocationStripped = 0x0001,
        const DebugInfoStripped = 0x0200,
        const UniprocessorSystemOnly = 0x4000,
        const RemovableRunFromSwap = 0x0400,        // if the image is on removable disk, fully load it and copy it to the swap file
        const NetRunFromSwap = 0x0800,              // if the image is on network media, fully load it and copy it to the swap file
        const Machine32bit = 0x0100,                // everyone tell me machine support 32bit word, no more info
        const FileSystemFile = 0x1000,

        // everyone use it but it should be useless for my PE64
        const LargeAddressAware = 0x0020,           // 3G for 32bit app on x86 Windows, 4G for 32bit app on x64 Windows, no one cares it for 64bit app on 64bit Windows because it already can use 8TB

        const Executable = 0x0002,
        const DynamicLoadLibrary = 0x2000,
    }
}
bitflags! {
    flags PEFileDLLAttributes : u16 {
        // reserved
        const Reserved1 = 0x0001,                   // reserved
        const Reserved2 = 0x0002,                   // reserved
        const Reserved3 = 0x0004,                   // reserved
        const Reserved4 = 0x0008,                   // reserved

        // won't use
        const ForceCodeIntegrityCheck = 0x0080,     // improves security, should not used on my app
        const MustRunInAppContainer = 0x1000,       // finally find a flag that prevent the executable from executing
        const WDMDriver = 0x2000,

        // don't kown when to use
        const NoIsolation = 0x0200,
        const NoSEH = 0x0400,
        const NoBind = 0x0800,

        const CanASLR = 0x0020,                     // everyone use it
        const CanRelocation = 0x0040,               // everyone use it
        const CanDEP = 0x0100,                      // everyone use it
        const TerminalServerAware = 0x8000,         // everyone use it
        const CanControlFlowGuard = 0x4000,         // improves security, I guess on some play like changing return address in stack
    }
}

bitflags! {
    flags PEFileHeaderDirectoryIndex: u32 {
        const Export = 0,
        const Import = 1,
        const Resource = 2,
        const Exception = 3,
        const Cerficate = 4,
        const Relocation = 5,
        const Debug = 6,
        const Architecture = 7,
        const GlobalPointer = 8,
        const ThreadStorage = 9,
        const LoadConfig = 10,
        const BoundImport = 11,
        const ImportAddress = 12,
        const DelayImport = 13,
        const DotNet = 14,
        const Reserved5 = 15,
    }
}

#[repr(C, packed)]
pub struct PEFileHeader {
    signature: u32,             // must be 0x00004550, "PE00"
    machine: u16,               // must be 0x8664, here only support AMD64
    section_count: u16,         // variable, section after the header  
    time_date_stamp: u32,       // variable, create time stamp, 0 or all 1 indicates not used
    symbol_table_offset: u32,   // must be 0, deprecated
    symbol_count: u32,          // must be 0, deprecated
    optional_header_size: u16,  // must be 240, for image optional header 64

    /// bitflag actaully const
    /// useful
    /// executable = 0x0002
    /// dynamic library = 0x2000,
    /// support u32 = 0x0100,
    /// large address space = 0x0020
    attributes: u16,

    magic_number: u16,          // must be 0x020B, here only support PE32+
    linker_version_1: u8,       // const, ffc-version.major = 0
    linker_version_2: u8,       // const, ffc-version.minor = 2
    code_size: u32,             // variable, sizeof text section or sizeof text sections, e.g. no more then 1 section, use 0x200
    data_size: u32,             // variable, sizeof data section or sizeof data sections
    bss_size: u32,              // variable, sizeof bss section or sizeof bss sections
    entry_point: u32,           // variable, entry point RVA
    code_base: u32,             // variable, code section RVA

    /// variable actually const, must be multiple of 64k
    /// preffered image base VA
    /// everyone use 0x140000000 for exe(64) and 0x10000000 for dll(32)
    image_base: u64,
    /// variable actually const, must >= file align
    /// every one use 0x1000
    section_align: u32,
    /// variable actually const, must be power of 2 between 512 and 64k, inclusive
    /// everyone use 0x200
    file_align: u32,

    os_version_1: u16,          // const, vc++ always use 6, I think it is 10
    os_version_2: u16,          // const, vc++ always use 0, I think it is 14393
    image_version_1: u16,       // const, vc++ always use 0, I think so
    image_version_2: u16,       // const, vc++ always use 0, I think so
    subsystem_version_1: u16,   // const, vc++ always use 6, I think it is 10
    subsystem_version_2: u16,   // const, vc++ always use 0, I think it is 14393
    win32_version: u32,         // must be 0, reserved, not deprecated

    image_size: u32,            // variable, sizeof image, must be multiple of section align
    /// variable (TODO: actuall const?), sizeof headers, from start of dos header to end of section headers
    /// must be multiple of file alignment
    headers_size: u32,

    checksum: u32,             // const, everyone use 0

    /// enum actually const, 2 for GUI, 3 for CUI
    /// only difference is GUI hide cmd window
    /// here only use 3
    subsystem: u16,

    /// bitflag actually const
    /// use ASLR = 0x0020
    /// can relocate = 0x0040
    /// use DEP = 0x0100
    /// Terminal server aware = 0x8000
    /// not used in exe
    dll_attributes: u16,
    stack_reserve: u64,         // const, vc++ always use 0x100000
    stack_commit: u64,          // const, vc++ always use 0x1000
    heap_reserve: u64,          // const, vc++ always use 0x100000
    heap_commit: u64,           // const, vc++ always use 0x1000
    loader_flag: u32,           // must be 0, reserved
    data_directory_count: u32,  // must be 16
    /// array
    /// 0: export, 1: import
    /// 2: resource, 3: exception
    /// 4: certificate, 5: base relocation
    /// 6: debug
    /// 7: arch, reserved, must be 0
    /// 8: global pointer, must be 0 (I guess obsolete)
    /// 9: thread storage
    /// 10: load config, 11: bound import
    /// 12: import address, 13: delay import
    /// 14: .Net, 
    /// 15: reserved, must be 0
    data_directories: [DataDirecotry; 16]    // only index 2, import dir is interested
}

// impl ImageNTHeader {

//     pub fn new() -> ImageNTHeader {
//         ImageNTHeader {
//             signature: 0x00004550,
//             machine: 0x8664,
//             section_count: 3,
//             time_date_stamp: 0,
//             symbol_table_offset: 0,
//             symbol_count: 0,
//             optional_header_size: 240,
//             attributes: 0x0122,  // exe, large address space, support DWORD,
//             magic_number: 0x020B, // 64
//             linker_version_1: 0,
//             linker_version_2: 1,
//             code_size: 0,
//             data_size: 0,
//             bss_size: 0,
//             entry_point: 0x1000,
//             code_base: 0,
//             image_base: 0x1_4000_0000_u64,
//             section_align: 0x1000,
//             file_align: 0x0200,
//             os_version_1: 6,
//             os_version_2: 0,
//             image_version_1: 0,
//             image_version_2: 0,
//             subsystem_version_1: 6,
//             subsystem_version_2: 0,
//             win32_version: 0,
//             image_size: 0x4000,
//             headers_size: 0x0400,
//             checksum: 0,
//             subsystem: 3,
//             dll_attributes: 0x8160, // ASLR, DEP, relocation, terminal server aware
//             stack_reserve: 0,
//             stack_commit: 0,
//             heap_reserve: 0,
//             heap_commit: 0,
//             loader_flag: 0,
//             data_directory_count: 16,
//             data_directories: [ DataDirecotry{ virtual_address: 0, size: 0 }; 16],
//         }
//     }

//     pub fn into_bytes(self) -> [u8; 264] {
//         use std::mem;
//         unsafe { mem::transmute::<ImageNTHeader, [u8; 264]>(self) }
//     } 
// }

pub struct PEFile {
    // nt_headers: ImageNTHeader,
}
impl PEFile {
    
    pub fn new() {
    }
}

#[cfg(test)]
#[test]
fn pefile_header_new() {

}
#[cfg(test)]
#[test]
fn pefile_header_load() {
    
}

#[cfg(test)]
#[test]
fn gen_hello_world_test() {
    use std::fs::File;
    use std::io::Write;
    use dos_header::get_default_dos_header;

    let dos_header_bytes = get_default_dos_header();

    // let nt_header = ImageNTHeader::new();
    // let nt_header_bytes = nt_header.into_bytes();

    let mut file = File::create("test\\gened-hello.exe").expect("Create file failed");
    let _ = file.write(&dos_header_bytes[..]).expect("write dos header failed");
    // let _ = file.write(&nt_header_bytes[..]).expect("write nt header failed");
    let _ = file.flush().expect("flush file failed");
}

#[cfg(test)]
#[test]
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
