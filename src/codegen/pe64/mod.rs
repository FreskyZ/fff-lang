
// PE64 file emitter
use std::mem;
use fsz_common::get_time_stamp;

#[repr(C)]
pub struct ImageDOSHeader {
    magic_number: u16,
    last_page_byte_length: u16,
    page_count: u16,
    relocation_count: u16,
    paragraph_header_size: u16,
    minimum_extra_paragraph_count: u16,
    maximum_extra_paragraph_count: u16,
    init_stack_section: u16,
    init_stack_pointer: u16,
    check_sum: u16,
    init_instruction_pointer: u16,
    init_code_section: u16,
    relocation_table_file_address: u16,
    overlay_number: u16,
    reserved: [u16; 4],
    oem_id: u16,
    oem_info: u16,
    reserved2: [u16; 10],
    pub nt_header_file_address: u32,
} // sizeof = 64

// "MZ", Mark Zbikowski,
const IMAGE_DOS_SIGNATURE: u16 = 0x4D5A;

impl ImageDOSHeader {

    pub fn new() -> ImageDOSHeader {
        ImageDOSHeader{
            magic_number: IMAGE_DOS_SIGNATURE,
            last_page_byte_length: 0x0090,
            page_count: 0x0002,
            relocation_count: 0,
            paragraph_header_size: 0x0004,
            minimum_extra_paragraph_count: 0,
            maximum_extra_paragraph_count: 0xFFFF,
            init_stack_section: 0,
            init_stack_pointer: 0x00B8,
            check_sum: 0,
            init_instruction_pointer: 0,
            init_code_section: 0,
            relocation_table_file_address: 0x0040,
            overlay_number: 0,
            reserved: [0; 4],
            oem_id: 0,
            oem_info: 0,
            reserved2: [0; 10],
            nt_header_file_address: 64
        }
    }

    pub fn new_with_nt_header_address(offset: u32) -> ImageDOSHeader {
        let mut ret_val = ImageDOSHeader::new();
        ret_val.nt_header_file_address = offset;
        ret_val
    }

    pub fn as_bytes(&self) -> [u8; 64] {
        unsafe { mem::transmute_copy::<ImageDOSHeader, [u8; 64]>(self) }
    }
}

#[derive(Copy, Clone)]
pub struct DataDirecotry{
    pub virtual_address: u32,
    pub size: u32,
}

#[repr(C)]
pub struct ImageNTHeader {
    signature: u32, // 0x00004550, "PE00"

    // File Header
    /// I386 => 0x014C,
    /// IA64 => 0x0200,
    /// AMD64 => 0x8664,
    machine: u16,               // only amd64 supported
    pub section_count: u16,     
    time_date_stamp: u32,       // standarad time stamp
    symbol_table_offset: u32,   // 0
    symbol_count: u32,          // 0
    optional_header_size: u16,  // 32 or 64, only 64 supported here

    /// usefull flags:
    /// executable = 0x0002
    /// dynamic library = 0x2000,
    /// support u32 = 0x0100,
    /// *large address space* = 0x0020
    attributes: u16,

    // optional header 64
    magic_number: u16,          // 0x010B for 32, 0x020B for 64, only 64 supported
    linker_version_1: u8,       // here is smc::codegen module version, 0
    linker_version_2: u8,       // here is smc::codegen module version: 1
    code_size: u32,             // 0
    data_size: u32,             // 0
    bss_size: u32,              // 0
    pub entry_point: u32,       // !!! entry point !!!
    code_base: u32,             // 0
    pub image_base: u64,        // !!! image base !!!
    section_align: u32,         // 0x1000
    file_align: u32,            // 0x0200
    os_version_1: u16,          // 0
    os_version_2: u16,          // 0
    image_version_1: u16,       // 0
    image_version_2: u16,       // 0
    subsystem_version_1: u16,   // 6
    subsystem_version_2: u16,   // 0
    win32_version: u32,         // 0
    pub image_size: u32,        // !!! image size !!!

    /// may be different here, previous is 0x400
    /// from file pos 0 to section headers end, 
    pub headers_size: u32,      
    check_sum: u32,             // 0
    subsystem: u16,             // 3 for CUI, 2 for GUI, only 3 supported

    /// relocation allowed = 0x0040
    /// aslr allowed = 0x0020
    /// dep allowed = 0x0100
    /// terminal server aware = 0x8000
    /// not used in exe file
    dll_attributes: u16,
    stack_reserve: u64,         // 0 for auto
    stack_commit: u64,          // 0 for auto
    heap_reserve: u64,          // 0 for auto
    heap_commit: u64,           // 0 for auto
    loader_flag: u32,           // obsolete
    data_directory_count: u32,  // 16
    data_directories: [DataDirecotry; 16]    // only index 2, import dir is interested
}

impl ImageNTHeader {

    pub fn new() -> ImageNTHeader {
        ImageNTHeader {
            signature: 0x00004550,
            machine: 0x8664,
            section_count: 3,
            time_date_stamp: get_time_stamp() as u32,
            symbol_table_offset: 0,
            symbol_count: 0,
            optional_header_size: 240,
            attributes: 0x0122,  // exe, large address space, support DWORD,
            magic_number: 0x020B, // 64
            linker_version_1: 0,
            linker_version_2: 1,
            code_size: 0,
            data_size: 0,
            bss_size: 0,
            entry_point: 0x1000,
            code_base: 0,
            image_base: 0x1_4000_0000_u64,
            section_align: 0x1000,
            file_align: 0x0200,
            os_version_1: 6,
            os_version_2: 0,
            image_version_1: 0,
            image_version_2: 0,
            subsystem_version_1: 6,
            subsystem_version_2: 0,
            win32_version: 0,
            image_size: 0x4000,
            headers_size: 0x0400,
            check_sum: 0,
            subsystem: 3,
            dll_attributes: 0x8160, // ASLR, DEP, relocation, terminal server aware
            stack_reserve: 0,
            stack_commit: 0,
            heap_reserve: 0,
            heap_commit: 0,
            loader_flag: 0,
            data_directory_count: 16,
            data_directories: [ DataDirecotry{ virtual_address: 0, size: 0 }; 16],
        }
    }

    pub fn as_bytes(&self) -> [u8; 264] {
        unsafe { mem::transmute_copy::<ImageNTHeader, [u8; 264]>(self) }
    } 
}

#[cfg(test)]
mod tests {

    #[test]
    #[ignore]
    fn pe64_write_struct() {
        use std::fs::File;
        use std::io::Write;
        use std::mem;
        use super::ImageDOSHeader;
        use super::ImageNTHeader;

        let mut file = File::create("hello.exe").expect("Create file failed");
        let header = ImageDOSHeader::new_with_nt_header_address(64);
        match file.write(&header.as_bytes()[..]) {
            Ok(write_len) => assert_eq!(write_len, mem::size_of::<ImageDOSHeader>()),
            Err(e) => panic!("Write file failed: {}", e),
        }
        let nt_header = ImageNTHeader::new();
        match file.write(&nt_header.as_bytes()[..]) {
            Ok(write_len) => assert_eq!(write_len, mem::size_of::<ImageNTHeader>()),
            Err(e) => panic!("Write file failed: {}", e),
        }
    }
}