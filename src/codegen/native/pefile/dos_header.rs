///! fff-lang
///!
///! dos header include dos stub, no custom stub provided

use super::Error;

// ref: http://www.fysnet.net/exehdr.htm
// ref: http://www.tavi.co.uk/phobos/exeformat.html
// ref: https://en.wikibooks.org/wiki/X86_Disassembly/Windows_Executable_Files
// ref: fenv-0.1.1's target
// ref: a helloworld in cpp
static DEFAULT_DOS_HEADER : [u8; 208] = [
    // <dos-header>
    /* 0x00 */ 0x4D, 0x5A,      // u16 ImageDOSHeader::magic_number = 0x5A4D, "MZ", Mark Zbikowski
    /* 0x02 */ 0xD0,    0,      // u16 ImageDOSHeader::last_page_byte_count = 0x00D0, every one use 0x0090 here but I think D0 is proper
    /* 0x04 */    1,    0,      // u16 ImageDOSHeader::page_count = 1, file size = (page_count - 1) * 512 + last page byte count, every one use 0x0003 here but I think 1 is proper
    /* 0x06 */    0,    0,      // u16 ImageDOSHeader::relocation_count = 0, no relocation table entry, struct relocation entry is { u16 offset, u16 segment }
    /* 0x08 */    4,    0,      // u16 ImageDOSHeader::header_size_in_paragraph = 4, size of paragraph = 16, size of header = 64, so this is 4
    /* 0x0A */    9,    0,      // u16 ImageDOSHeader::min_additional_paragraph = 9, loader should allocate at least these paras to execute the program, set to 0xD0 - 0x40 / 0x10
    /* 0x0C */ 0xFF, 0xFF,      // u16 ImageDOSHeader::max_additional_paragraph = 0xFFFF, set to max value
    /* 0x0E */    0,    0,      // u16 ImageDOSHeader::init_stack_section_reg = 0, init ss
    /* 0x10 */ 0xC0,    0,      // u16 ImageDOSHeader::init_stack_pointer = 0x00C0, init sp, my default dos stub end at 0xD0, leave 0x10 bytes as init stack and init sp is 0xC0
    /* 0x12 */    0,    0,      // u16 ImageDOSHeader::check_sum = 0, set to 0 to disable file checksum, checksum algorithm is one's complement
    /* 0x14 */    0,    0,      // u16 ImageDOSHeader::init_instruction_pointer = 0, init ip
    /* 0x16 */    0,    0,      // u16 ImageDOSHeader::init_code_segment_reg = 0, init cs
    /* 0x18 */ 0x40,    0,      // u16 ImageDOSHeader::relocation_table_file_offset = 0x0040, as relocation table not used, point it to immediately after this header
    /* 0x1A */    0,    0,      // u16 ImageDOSHeader::overlay_number = 0, usually 0, not know what's not usual
    /* 0x1C */ 0, 0, 0, 0, 0, 0, 0, 0,       // reserved 1
    /* 0x24 */ 0, 0, 0, 0,       // u16 ImageDOSHeader::oem_id, oem_info = 0, not know how to use
    /* 0x28 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // reserved 2 
    /* 0x32 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // reserved 3
    /* 0x3C */ 0xD0, 0, 0, 0,   // u32 ImageDOSHeader::nt_header_offset = 0x000000D0, my default dos stub is 0xD0 

    // <dos-program>
    /* 0x40 */ 0x0E,                // PUSH CS
    /* 0x41 */ 0x1F,                // POP DS 
    /* 0x42 */ 0xBA, 0x0E, 0x00,    // MOV DX, 0x0E   ; MOV DX, "This Program..."
    /* 0x45 */ 0xB4, 0x09,          // MOV AX, 09     ; SYSTEM CALL TO DISPLAY STRING
    /* 0x47 */ 0xCD, 0x21,          // INT 21
    /* 0x49 */ 0xB8, 0x01, 0x4C,    // MOV AX, 0x4C01 ; AH = 0x4C: terminate with return code, AL = 0x01: return code 01
    /* 0x4C */ 0xCD, 0x21,          // INT 21
    /* 0x4E */ 0x54, 0x68, 0x69, 0x73, 0x20,                    // "This "
    /* 0x54 */ 0x70, 0x72, 0x6F, 0x57, 0x72, 0x61, 0x6D, 0x20,  // "program "
    /* 0x5A */ 0x63, 0x61, 0x6E, 0x6E, 0x6F, 0x74, 0x20,        // "cannot "
               0x62, 0x65, 0x20,                                // "be "
               0x72, 0x75, 0x6E, 0x20,                          // "run "
               0x69, 0x6E, 0x20,                                // "in "
               0x44, 0x4F, 0x53, 0x20,                          // "DOS "
               0x6D, 0x6F, 0x64, 0x65, 0x2E,                    // "mode."
               0x0D, 0x0D, 0x0A, 0x24,                          // "\n\n\r$"
    /* 0x79 */ 0, 0, 0, 0, 0, 0, 0,     // padding zero 1

    // <unknown>
    /* 0x80 */ 0x52, 0x45, 0x39, 0x54, 0x49, 0x48, 0x42, 0x79,  // "RE9TIHBy"
    /* 0x88 */ 0x62, 0x32, 0x64, 0x79, 0x59, 0x57, 0x30, 0x67,  // "b2dyYW0g"
    /* 0x90 */ 0x64, 0x57, 0x35, 0x31, 0x63, 0x32, 0x56, 0x6B,  // "dW51c2Vk"
    /* 0x98 */ 0x49, 0x48, 0x4E, 0x30, 0x59, 0x57, 0x4E, 0x72,  // "IHN0YWNr"
    /* 0xA0 */ 0x49, 0x47, 0x68, 0x6C, 0x63, 0x6D, 0x55, 0x67,  // "IGhlcmUg"
    /* 0xA8 */ 0x59, 0x57, 0x35, 0x6B, 0x49, 0x46, 0x70, 0x5A,  // "YW5kIFpZ"
    /* 0xB0 */ 0x53, 0x43, 0x42, 0x4A, 0x49, 0x45, 0x78, 0x50,  // "SCBJIExP"
    /* 0xB8 */ 0x56, 0x6B, 0x55, 0x67, 0x57, 0x55, 0x39, 0x56,  // "VkUgWU9V"
    /* 0xC0 */ 0, 0, 0, 0, 0, 0, 0, 0,  // padding zero 2
    /* 0xC8 */ 0, 0, 0, 0, 0, 0, 0, 0,  // padding zero 3
];

pub fn get_default_dos_header() -> &'static [u8] { &DEFAULT_DOS_HEADER[..] }

pub fn check_dos_header(bytes: &[u8]) -> Result<usize, Error> {

    if bytes.len() < 64 {
        Err(Error::ByteArrayTooShort)
    } else if bytes[0] != 0x4D || bytes[1] != 0x5A {
        Err(Error::InvalidDOSHeaderMagic)
    } else {
        // Small attention: 
        // cannot use &bytes[0x3C] as *const u8 as *const usize here because
        // it will regard that pointer as u64 and everything boom
        Ok(unsafe { *(&bytes[0x3C] as *const u8 as *const u32) as usize })
    }
}

#[cfg(test)] #[test]
fn get_dos_header_test() {

    let default_header = get_default_dos_header();
    assert!(default_header.len() > 64);
    assert_eq!(default_header[0], 0x4D);
    assert_eq!(default_header[1], 0x5A);
    assert_eq!(unsafe { *(&default_header[0x3C] as *const u8 as *const u32) }, 0xD0);
}

#[cfg(test)] #[test]
fn dos_header_check_test() {

    let bytes1 = [0u8; 0];
    assert_eq!(check_dos_header(&bytes1[..]), Err(Error::ByteArrayTooShort));

    let bytes2 = [0x4Du8, 0x5Au8, 1, 2, 3];
    assert_eq!(check_dos_header(&bytes2[..]), Err(Error::ByteArrayTooShort));

    let mut bytes3 = [0u8; 64];
    bytes3[0] = 0x4D;
    bytes3[1] = 0x5A;
    bytes3[60] = 233;
    assert_eq!(check_dos_header(&bytes3[..]), Ok(233));

    let mut bytes4 = [0u8; 233];
    bytes4[0] = 0x4D;
    bytes4[1] = 0x5A;
    bytes4[60] = 42;
    assert_eq!(check_dos_header(&bytes4[..]), Ok(42));
    
    let mut bytes5 = [0u8; 233];
    bytes5[0] = 0x4D;
    bytes5[60] = 42;
    assert_eq!(check_dos_header(&bytes5[..]), Err(Error::InvalidDOSHeaderMagic));
}