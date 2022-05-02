///! source::iter: source code content iterator, with interner

use std::collections::{HashMap, hash_map::DefaultHasher};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::NonZeroU32;
use std::path::PathBuf;
use std::ops::{Add, AddAssign};
use super::{SourceContext, SourceFile, FileId};

pub const EOF: char = 0u8 as char;

/// Character location
///
/// - it is byte index accross all source files, e.g. second file's position starts from first file's byte length (+1)
///   to reduce memory usage because location info is used extremely widely
/// - it is u32 not usize because it is not reasonable to
///   have a file size over 4GB or all source file total size over 4GB for this toy language (possibly for all languages)
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct Position(u32);

impl Position {
    pub const fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
        self.0
    }

    pub fn offset(self, offset: i32) -> Self {
        Self(if offset >= 0 { self.0 + offset as u32 } else { self.0 - (-offset) as u32 })
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<u32> for Position {
    fn from(v: u32) -> Self {
        Self(v)
    }
}

// use `position1 + position2` to merge into span
impl Add for Position {
    type Output = Span;
    fn add(self, rhs: Position) -> Span {
        debug_assert!(rhs.0 >= self.0, "invalid position + position");
        Span::new(self, rhs)
    }
}

/// Character range location
///
/// construct from 2 Positions,
/// while type name is Span, recommend variable name is `loc` or `location`
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}
impl Span {
    // e.g. Span::new(position1, position2) or Span::new(42, 43)
    pub fn new(start: impl Into<Position>, end: impl Into<Position>) -> Span {
        Span{ start: start.into(), end: end.into() }
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Span({}, {})", self.start.0, self.end.0)
    }
}

// use `span1 + span2` to merge span
// ATTENTION: only allow left + right, while gap/overlap both acceptable
impl Add for Span {
    type Output = Span;
    fn add(self, rhs: Span) -> Span {
        debug_assert!(rhs.start.0 >= self.start.0 && rhs.end.0 >= self.end.0, "invalid span + span");
        Span{ start: self.start, end: rhs.end }
    }
}

// or `span + position`
impl Add<Position> for Span {
    type Output = Span;
    fn add(self, rhs: Position) -> Span {
        debug_assert!(rhs.0 >= self.end.0, "invalid span + position");
        Span{ start: self.start, end: rhs }
    }
}

// use `span += position` to update span
impl AddAssign<Position> for Span {
    fn add_assign(&mut self, rhs: Position) {
        debug_assert!(rhs.0 >= self.end.0, "invalid span += position");
        self.end = rhs;
    }
}

// or use `span1 += span2`
// ATTENTION: only allow left += right, while gap/overlap both acceptable
impl AddAssign<Span> for Span {
    fn add_assign(&mut self, rhs: Span) {
        debug_assert!(rhs.start.0 >= self.start.0 && rhs.end.0 >= self.end.0, "invalid span += span");
        self.end = rhs.end;
    }
}

// use `position.into()` to turn position directly into span
impl From<Position> for Span {
    fn from(position: Position) -> Span {
        Span::new(position, position)
    }
}

/// a handle to an interned string
///
/// - IsId means InternedStringID, it is short because it is widely used
/// - it was named SymbolID or SymId or Sym but I found that 
///   symbol (in symbol table) in compiler principle means a "name", a name to a variable, function, etc.
///   although I will call that a "Name", or a "TypeId", "VarId" etc. in my semantic analysis, but this name
///   may confuse reader or myself after, for example, 10 years (although I'm not confused after this 5 years)
/// - SymbolID, StringID or InternedStringID is too long, 
///   Str or String makes reader think it is kind of string (a ptr, cal, len structure)
/// - it is u32 not usize because it is not reasonable to 
///   have more than u32::MAX strings in a program, and it is widely used
/// - recommend variable name `id` or `string_id`
#[derive(Eq, PartialEq, Clone, Copy, Hash)]
pub struct IsId(NonZeroU32);

impl IsId {
    pub(super) const POSITION_MASK: u32 = 1 << 31;

    pub const fn new(v: u32) -> Self {
        debug_assert!(v != 0, "isid cannot be 0");
        // SAFETY: debug_assert above
        Self(unsafe { NonZeroU32::new_unchecked(v) })
    }
    pub fn unwrap(self) -> u32 {
        self.0.get()
    }
}
impl From<u32> for IsId {
    fn from(v: u32) -> Self {
        Self::new(v)
    }
}

impl fmt::Debug for IsId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("IsId").field(&self.0.get()).finish()
    }
}

// this is currently an u128, but I suspect that it can fit in u64
// for current small test until even first bootstrap version, the string id and span should be easily fit in u64
// for "dont-know-whether-exist very large program", 
// considering these 2 ids increase accordingly, squash `u32::MAX - id` and span together may still be ok 
#[derive(PartialEq, Clone, Copy)]
pub struct IdSpan {
    pub id: IsId,
    pub span: Span,
}

impl IdSpan {
    pub fn new(id: impl Into<IsId>, span: Span) -> Self {
        Self{ id: id.into(), span }
    }
}

impl fmt::Debug for IdSpan {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("IdSpan")
            .field(&self.id)
            .field(&self.span)
            .finish()
    }
}

fn get_hash(content: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    Hash::hash(content, &mut hasher);
    hasher.finish()
}

// get LF byte indexes
fn get_endlines(content: &str) -> Vec<usize> {
    content.char_indices().filter(|(_, c)| c == &'\n').map(|(i, _)| i).collect()
}

// this iterator is the exact first layer of processing above source code content, 
// logically all location information comes from position created by the next function
//
// this iterator also includes the string intern interface, to make leixcal parser simpler
//
// from source context's perspective, this is also a source file builder which returns by 
// entry and import function and when running, append to string table and when finished, append to files
#[derive(Debug)]
pub struct SourceChars<'a> {
    content: String,
    current_index: usize,  // current iterating byte index, content bytes[current_index] should be the next returned char
    start_index: usize,    // starting byte index for this file, or previous files's total byte length, will copy to SourceFile.start_index when finished
    // copy to SourceFile
    path: PathBuf,
    namespace: Vec<IsId>,
    request: Option<Span>,
    // borrow other part of SourceContext except fs to prevent <F> propagation 
    files: &'a mut Vec<SourceFile>,
    string_hash_to_id: &'a mut HashMap<u64, IsId>,
    string_id_to_span: &'a mut Vec<Span>,
    string_additional: &'a mut String,
}

impl<'a> SourceChars<'a> {

    pub(super) fn new<F>(
        mut content: String, 
        start_index: usize, 
        path: PathBuf, 
        namespace: Vec<IsId>, 
        request: Option<Span>,
        context: &'a mut SourceContext<F>,
    ) -> Self {
        // append 3 '\0' char to end of content for the branchless (less branch actually) iterator
        content.push_str("\0\0\0");
        Self{
            content, 
            start_index, 
            current_index: 0, 
            path, 
            namespace, 
            request,
            files: &mut context.files,
            string_hash_to_id: &mut context.string_hash_to_id,
            string_id_to_span: &mut context.string_id_to_span,
            string_additional: &mut context.string_additional,
        }
    }

    /// iterate return char and byte index
    /// 
    /// ignore all bare or not bare CR, return EOF after EOF
    pub fn next(&mut self) -> (char, Position) {
        loop {
            if self.current_index == self.content.len() - 3 {
                return (EOF, Position::new((self.start_index + self.current_index) as u32));
            }
            let bytes = self.content.as_bytes();
            match bytes[self.current_index] {
                b'\r' => { // ignore \r
                    self.current_index += 1;
                    continue;
                },
                b @ 0..=128 => { // ascii fast path
                    self.current_index += 1;
                    return (b as char, Position::new((self.current_index - 1 + self.start_index) as u32));
                },
                _ => {
                    let width = get_char_width(&self.content, self.current_index);
                    if self.current_index + width > self.content.len() - 3 {
                        // TODO: this should be an error not panic, although unrecoverable
                        panic!("invalid utf-8 sequence");
                    }

                    const MASKS: [u8; 5] = [0, 0, 0x1F, 0x0F, 0x07]; // byte 0 masks
                    const SHIFTS: [u8; 5] = [0, 0, 12, 6, 0]; // shift back for width 2 and width 3
                    let bytes = &bytes[self.current_index..];
                    let r#char = ((((bytes[0] & MASKS[width]) as u32) << 18) | (((bytes[1] & 0x3F) as u32) << 12) | (((bytes[2] & 0x3F) as u32) << 6) | ((bytes[3] & 0x3F) as u32)) >> SHIFTS[width];

                    // TODO: check more invalid utf8 sequence include following bytes not start with 0b10 and larger than 10FFFF and between D800 and E000
                    self.current_index += width;
                    // SAFETY: invalid char should not cause severe issue in lexical parse and syntax parse
                    return (unsafe { char::from_u32_unchecked(r#char) }, Position::new((self.current_index - width + self.start_index) as u32));
                },
            }
        }
    }

    pub fn intern(&mut self, value: &str) -> IsId {

        // empty string is span 0,0, this span must exist because this function exists in this type
        if value.is_empty() {
            return IsId::new(1);
        }

        let hash = get_hash(value);
        if let Some(id) = self.string_hash_to_id.get(&hash) {
            *id
        } else {
            let new_id = IsId::new(self.string_id_to_span.len() as u32);
            self.string_hash_to_id.insert(hash, new_id);

            let start_position = if let Some(index) = self.string_additional.find(value) {
                index
            } else {
                self.string_additional.push_str(value);
                self.string_additional.len() - value.len()
            } as u32;
            // ATTENTION: this type of span's end is last byte index + 1 (exactly the one you use in str[begin..end], not last char
            let span = Span::new(start_position | IsId::POSITION_MASK, start_position + value.len() as u32);

            self.string_id_to_span.push(span);
            new_id
        }
    }

    // intern string at location
    pub fn intern_span(&mut self, location: Span) -> IsId {
        let (start, end) = (location.start.0 as usize, location.end.0 as usize);

        debug_assert!(start <= end, "invalid span");
        debug_assert!(self.start_index <= start, "not this file span");
        // does not check position for EOF because it is not expected to intern something include EOF
        debug_assert!(end - self.start_index < self.content.len() - 3 && start - self.start_index < self.content.len() - 3, "position overflow");

        let end_width = get_char_width(&self.content, end - self.start_index);
        let hash = get_hash(&self.content[start - self.start_index..end - self.start_index + end_width]);
        if let Some(id) = self.string_hash_to_id.get(&hash) {
            *id
        } else {
            let new_id = IsId::new(self.string_id_to_span.len() as u32);
            self.string_hash_to_id.insert(hash, new_id);
            self.string_id_to_span.push(location);
            new_id
        }
    }

    pub fn get_file_id(&self) -> FileId {
        FileId::new(self.files.len() as u32 + 1)
    }

    pub fn finish(mut self) {
        let content_length = self.content.len();
        self.content.truncate(content_length - 3);
        self.files.push(SourceFile{
            path: self.path,
            endlines: get_endlines(&self.content),
            content: self.content,
            namespace: self.namespace,
            start_index: self.start_index,
            request: self.request,
        });
    }
}

// impl<'a> Drop for SourceChars<'a> {
//     fn drop(&mut self) {
//         // this cannot be some SourceContext::finish_build because SourceChars 
//         // does not have a referece to SourceContext to prevent propagating <F: FileSystem> generic parameter
//         let content_length = self.content.len();
//         self.content.truncate(content_length - 3);
//         self.files.push(SourceFile{
//             path: std::mem::take(&mut self.path),
//             endlines: get_endlines(&self.content),
//             content: std::mem::take(&mut self.content),
//             namespace: std::mem::take(&mut self.namespace),
//             start_index: self.start_index,
//             request: self.request,
//         });
//     }
// }

// width byte[0]  byte[1]  byte[2]  byte[3]
// 1     0xxxxxxx
// 2     110xxxxx 10xxxxxx
// 3     1110xxxx 10xxxxxx 10xxxxxx
// 4     11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
const WIDTH: [usize; 256] = [
//  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, A, B, C, D, E, F
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 0
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 1
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 3
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 4
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 5
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 6
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 7
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 8
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 9
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // A
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // B
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // C
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, // D
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, // E
    4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, // F
];

pub fn get_char_width(content: &str, byte_index: usize) -> usize {
    WIDTH[content.as_bytes()[byte_index] as usize]
}
