///! fff-lang
///!
///! pefile/file_headers, for ImageNTHeaders { Signature, ImageNTHeader, ImageOptionalHeader }

use std::fmt;

use util::DateTime;

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
pub mod FileAttributes {
    // FileHeader::attribute
    // flags FileAttributes: u16 {
    //     // deprecated
    //     const LineNumberStripped = 0x0004,          // deprecated
    //     const LocalSymbolStripped = 0x0008,         // deprecated
    //     const AggressiveTrimWorkingSet = 0x0010,    // deprecated
    //     const ByteOrderLittleEndian = 0x0080,       // deprecated
    //     const ByteOrderBigEndian = 0x8000,          // deprecated

    //     // reserved
    //     const _Reserved1 = 0x0040,                  // reserved

    //     // don't when to use
    //     const RelocationStripped = 0x0001,
    //     const DebugInfoStripped = 0x0200,
    //     const UniprocessorSystemOnly = 0x4000,
    //     const RemovableRunFromSwap = 0x0400,        // if the image is on removable disk, fully load it and copy it to the swap file
    //     const NetRunFromSwap = 0x0800,              // if the image is on network media, fully load it and copy it to the swap file
    //     const FileSystemFile = 0x1000,

    //     // 3G for 32bit app on x86 Windows, 4G for 32bit app on x64 Windows, no one cares it for 64bit app on 64bit Windows because it already can use 8TB
    //     // everyone use it but it should be useless for my PE64
    //     const LargeAddressAware = 0x0020,           
    //     // everyone tell me machine support 32bit word, no more info
    //     const Machine32bit = 0x0100,                

    //     const Executable = 0x0002,
    //     const DynamicLoadLibrary = 0x2000,
    // }
    
    // flags PEFileDLLAttributes : u16 {
    //     // reserved
    //     const Reserved1 = 0x0001,                   // reserved
    //     const Reserved2 = 0x0002,                   // reserved
    //     const Reserved3 = 0x0004,                   // reserved
    //     const Reserved4 = 0x0008,                   // reserved

    //     // won't use
    //     const ForceCodeIntegrityCheck = 0x0080,     // improves security, should not used on my app
    //     const MustRunInAppContainer = 0x1000,       // finally find a flag that prevent the executable from executing
    //     const WDMDriver = 0x2000,

    //     // don't kown when to use
    //     const NoIsolation = 0x0200,
    //     const NoSEH = 0x0400,
    //     const NoBind = 0x0800,

    //     const CanASLR = 0x0020,                     // everyone use it
    //     const CanRelocation = 0x0040,               // everyone use it
    //     const CanDEP = 0x0100,                      // everyone use it
    //     const TerminalServerAware = 0x8000,         // everyone use it
    //     const CanControlFlowGuard = 0x4000,         // improves security, I guess on some play like changing return address in stack
    // }

    pub const Executable: u32 = 0x0001;              // map to attribute = 0x0122 and dll_attributes = 0,
    pub const DynamicLibrary: u32 = 0x0002;          // map to attribute = 0x2120 and dll_attributes = 0xC160,
    pub const SubsystemGUI: u32 = 0x0004;            // map to subsystem = 2
    pub const SubsystemCUI: u32 = 0x0008;            // map to subsystem = 3
}

#[allow(non_upper_case_globals)]
#[allow(non_snake_case)]
pub mod FileHeaderDirectoryIndex {
    pub const Export: u32 = 0;
    pub const Import: u32 = 1;
    pub const Resource: u32 = 2;
    pub const Exception: u32 = 3;
    pub const Certificate: u32 = 4;
    pub const Relocation: u32 = 5;
    pub const Debug: u32 = 6;
    pub const Architecture: u32 = 7;
    pub const GlobalPointer: u32 = 8;
    pub const ThreadStorage: u32 = 9;
    pub const LoadConfig: u32 = 10;
    pub const BoundImport: u32 = 11;
    pub const ImportAddress: u32 = 12;
    pub const DelayImport: u32 = 13;
    pub const Dotnet: u32 = 14;
}

pub struct DataDirecotry{
    pub virtual_address: u32,
    pub size: u32,
}

#[repr(C, packed)]
pub struct FileHeader {
    signature: u32,             // must be 0x00004550, "PE00"

    // NTHeader
    machine: u16,               // must be 0x8664, here only support AMD64
    section_count: u16,         // variable, section after the header  
    time_date_stamp: u32,       // variable, create time stamp, 0 or all 1 indicates not used
    symbol_table_offset: u32,   // must be 0, deprecated
    symbol_count: u32,          // must be 0, deprecated
    optional_header_size: u16,  // must be 240, for image optional header 64
    attributes: u16,            // nearly const, see FileAttributes::flags FileAttributes

    magic_number: u16,          // must be 0x020B, here only support PE32+
    linker_version_1: u8,       // const, ffc-version.major = 0
    linker_version_2: u8,       // const, ffc-version.minor = 3 (0.1.2 => 1 + 2 = 3)
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
    os_version_2: u16,          // const, vc++ always use 0, I think it is 15063
    image_version_1: u16,       // const, vc++ always use 0, I think so
    image_version_2: u16,       // const, vc++ always use 0, I think so
    subsystem_version_1: u16,   // const, vc++ always use 6, I think it is 10
    subsystem_version_2: u16,   // const, vc++ always use 0, I think it is 15063
    win32_version: u32,         // must be 0, reserved, not deprecated

    image_size: u32,            // variable, sizeof image, must be multiple of section align
    /// variable (TODO: actuall const?), sizeof headers, from start of dos header to end of section headers
    /// must be multiple of file alignment
    headers_size: u32,

    checksum: u32,              // const, everyone use 0

    subsystem: u16,             // nearly const, see FileAttributes
    dll_attributes: u16,        // nearly const, see FileAttributes
    stack_reserve: u64,         // const, vc++ always use 0x100000
    stack_commit: u64,          // const, vc++ always use 0x1000
    heap_reserve: u64,          // const, vc++ always use 0x100000
    heap_commit: u64,           // const, vc++ always use 0x1000
    loader_flag: u32,           // must be 0, reserved
    data_directory_count: u32,  // must be 16
    data_directories: [DataDirecotry; 16], // variable
}
impl FileHeader { // New

    fn new_basic() -> FileHeader {
        FileHeader {
            signature: 0x00004550,
            machine: 0x8664,
            section_count: 0,
            time_date_stamp: 0,
            symbol_table_offset: 0,
            symbol_count: 0,
            optional_header_size: 240,
            attributes: 0,
            magic_number: 0x020B,
            linker_version_1: 0,
            linker_version_2: 3,
            code_size: 0,
            data_size: 0,
            bss_size: 0,
            entry_point: 0,
            code_base: 0,
            image_base: 0,
            section_align: 0x1000,
            file_align: 0x0200,
            os_version_1: 10,
            os_version_2: 15063,
            image_version_1: 0,
            image_version_2: 0,
            subsystem_version_1: 10,
            subsystem_version_2: 15063,
            win32_version: 0,
            image_size: 0,
            headers_size: 0,
            checksum: 0,
            subsystem: 3,
            dll_attributes: 0,
            stack_reserve: 0,
            stack_commit: 0,
            heap_reserve: 0,
            heap_commit: 0,
            loader_flag: 0,
            data_directory_count: 16,
            data_directories: [ 
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
                DataDirecotry{ virtual_address: 0, size: 0 },
            ],
        }
    }
    pub fn new() -> FileHeader {
        let mut retval = FileHeader::new_basic();
        retval.attributes = 0x0122;
        return retval;
    }
    pub fn new_dll() -> FileHeader {
        let mut retval = FileHeader::new_basic();
        retval.attributes = 0x2120;
        retval.dll_attributes = 0xC160;
        return retval;
    }
}
impl FileHeader { // Set

}
impl fmt::Debug for FileHeader { // Format
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PEFileHeader{{\n")?;
        write!(f, "   TimeStamp: {}\n", DateTime::with_timestamp(self.time_date_stamp as u64).format("iso8601"))?;
        write!(f, "   ImageSize: {:x}\n", self.image_size)?;
        write!(f, "   HeadersSize: {:x}\n", self.headers_size)?;
        write!(f, "   Attributes:\n")?; {
            if self.attributes & 0x2 != 0 { write!(f, "      Executable\n")?; }
            if self.attributes & 0x20 != 0 { write!(f, "      LargeAddressAware\n")?; }
            if self.attributes & 0x100 != 0 { write!(f, "      32bitWordMachine\n")?; }
            if self.attributes & 0x2000 != 0 { write!(f, "      DynamicLibrary\n")?; }
        }
        write!(f, "   DLLAttributes:\n")?; {
            if self.dll_attributes & 0x20 != 0 { write!(f, "      ASLRAllowed\n")?; }
            if self.dll_attributes & 0x40 != 0 { write!(f, "      RelocationAllowed\n")?; }
            if self.dll_attributes & 0x100 != 0 { write!(f, "      DEPAllowed\n")?; }
            if self.dll_attributes & 0x4000 != 0 { write!(f, "      ControlFlowGuardAllowed\n")?; }
            if self.dll_attributes & 0x8000 != 0 { write!(f, "      TerminalServerAware\n")?; }
        }
        if self.dll_attributes == 0 { write!(f, "      (empty)\n")?; }
        write!(f, "   ImageBase: {:x}\n", self.image_base)?;
        write!(f, "   CodeSectionSize: {:x}\n", self.code_size)?;
        write!(f, "   DataSectionSize: {:x}\n", self.data_size)?;
        write!(f, "   BSSSectionSize: {:x}\n", self.bss_size)?;
        write!(f, "   CodeSectionStart: {:x}\n", self.code_base)?;
        write!(f, "   EntryPoint: {:x}\n", self.entry_point)?;
        
        let mut has_sections = false;
        write!(f, "   SectionCount: {:x}\n", self.section_count)?;
        write!(f, "   Sections: \n")?; for i in 0..16 {
            const DATA_DIRECTORY_NAMES: [&str; 15] = [
                "Export", "Import", "Resource", "Exception", 
                "Certificate", "Relocatio", "Debug", "Architecture",
                "GlobalPointer", "ThreadStorage", "LoadConfig", "BoundImport", 
                "ImportAddress", "DelayImport", "Dotnet"
            ];
            let current_dir = &self.data_directories[i];
            if current_dir.virtual_address != 0 && current_dir.size != 0 {
                has_sections = true;
                write!(f, "      {} from {:x} size {:x}\n", DATA_DIRECTORY_NAMES[i], current_dir.virtual_address, current_dir.size)?;
            }
        }
        if !has_sections { write!(f, "      (empty)\n")?; }
        write!(f, "}}")
    }     
}
impl FileHeader { // Serialize and de-
    pub fn into_bytes(self) -> [u8; 264] {
        use std::mem;
        unsafe { mem::transmute::<FileHeader, [u8; 264]>(self) }
    } 
    pub fn from_bytes(bytes: &[u8]) -> FileHeader {
        use std::mem;

        let mut copied_bytes = [0u8; 264];
        for i in 0..264 {
            copied_bytes[i] = bytes[i]; // No one cares performance here, one for each PE file
        }
        unsafe { mem::transmute::<[u8; 264], FileHeader>(copied_bytes) }
    }
}

#[cfg(test)] #[test]
fn pefile_header_new() {

    let file_header = FileHeader::new();
    println!("raw: {:?}", file_header);
}
#[cfg(test)] #[test]
fn pefile_header_load() {
    // ignored test because
    // - test by print, not auto test
    // - forget to deignore .exe file and test file is lost

    // use std::fs::File;
    // use std::io::Read;
    // use std::io::Seek;
    // use std::io::SeekFrom;
    // use super::dos_header::check_dos_header;
    
    // let mut test_file = File::open(r"..\tests\pefile\hello.exe").expect("cannot open file");
    // let file_len = test_file.seek(SeekFrom::End(0)).expect("cannot seek end");
    // test_file.seek(SeekFrom::Start(0)).expect("seek back failed");
    // let file_bytes = {
    //     let mut file_bytes = Vec::with_capacity(file_len as usize + 10);
    //     test_file.read_to_end(&mut file_bytes).expect("read file failed");
    //     file_bytes
    // }.into_boxed_slice();
    // println!("file bytes: {:?}", &file_bytes[0..10]);
    // let file_header_offset = check_dos_header(file_bytes.as_ref()).expect("cannot get dosheader");
    // let file_header = FileHeader::from_bytes(&file_bytes[(file_header_offset as usize)..]);
    // panic!("{:?}", file_header);
}