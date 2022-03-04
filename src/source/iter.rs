///! source::iter: source code content iterator, with interner

use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;
use std::ops::{Add, AddAssign};
use super::{FileSystem, DefaultFileSystem, SourceContext, SourceFile, FileId};

pub const EOF: char = 0u8 as char;

/// Character location
///
/// - it is byte index accross all source files, e.g. second file's position starts from first file's byte length (+1)
///   to reduce memory usage because location info is used extremely widely
/// - it is u32 not usize because it is not reasonable to
///   have a file size over 4GB or all source file total size over 4GB for this toy language (possibly for all languages)
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct Position(pub(super) u32);

impl Position {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
        self.0
    }
    pub fn offset(self, offset: i32) -> Self {
        Self(if offset >= 0 { self.0 + offset as u32 } else { self.0 - (-offset) as u32 })
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
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
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
/// its name is short because it is widely used,
/// it is u32 not usize because it is widely used
/// and not reasonable to have more than u32::MAX symbols in all source files
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct Sym(u32);

impl Sym {
    pub(super) const MASK: u32 = 1 << 31; // string symbol mask

    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
        self.0
    }
}
impl From<u32> for Sym {
    fn from(v: u32) -> Self {
        Self(v)
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
// this iterator also includes the symbol intern interface, to make leixcal parser simpler
//
// from source context's perspective, this is also a source file builder which returns by 
// entry and import function and when running, append to symbols and when finished, append to files
pub struct SourceChars<'a, F = DefaultFileSystem> {
    pub(super) content: String,
    current_index: usize,  // current iterating byte index, content bytes[current_index] should be the next returned char
    pub(super) start_index: usize,    // starting byte index for this file, or previous files's total byte length, will become SourceFile.start_index when finished
    pub(super) path: PathBuf,         // copy to SourceFile
    pub(super) namespace: Vec<Sym>,   // copy to SourceFile
    pub(super) request: Option<Span>, // copy to SourceFile
    context: &'a mut SourceContext<F>,
}

impl<'a, F> SourceChars<'a, F> where F: FileSystem {

    pub(super) fn new(mut content: String, start_index: usize, path: PathBuf, namespace: Vec<Sym>, request: Option<Span>, context: &'a mut SourceContext<F>) -> Self {
        // append 3 '\0' char to end of content for the branchless (less branch actually) iterator
        content.push_str("\0\0\0");
        Self{ content, start_index, current_index: 0, path, namespace, request, context }
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
                    return (b as char, Position::new((self.current_index - 1) as u32));
                },
                _ => {
                    let width = get_char_width(&self.content, self.current_index);
                    if self.current_index + width > self.content.len() - 3 {
                        // TODO: this should be an error not panic, although unrecoverable
                        panic!("invalid utf-8 sequence");
                    }

                    const MASKS: [u8; 5] = [0, 0, 0x1F, 0x0F, 0x07]; // byte 0 masks
                    let bytes = &bytes[self.current_index..];
                    let r#char = (((bytes[0] & MASKS[width]) as u32) << 18) | (((bytes[1] & 0x3F) as u32) << 12) | (((bytes[2] & 0x3F) as u32) << 6) | ((bytes[3] & 0x3F) as u32);

                    // TODO: check more invalid utf8 sequence include following bytes not start with 0b10 and larger than 10FFFF and between D800 and E000
                    self.current_index += width;
                    // SAFETY: invalid char should not cause severe issue in lexical parse and syntax parse
                    return (unsafe { char::from_u32_unchecked(r#char) }, Position::new((self.current_index - width) as u32));
                },
            }
        }
    }

    // intern symbol at location
    pub fn intern_span(&mut self, location: Span) -> Sym {
        let (start, end) = (location.start.0 as usize, location.end.0 as usize);

        debug_assert!(start <= end, "invalid span");
        debug_assert!(self.start_index <= start, "not this file span");
        // does not check position for EOF because it is not expected to intern something include EOF
        debug_assert!(end < self.content.len() - 3 && start< self.content.len() - 3, "position overflow");

        let hash = get_hash(&self.content[start - self.start_index..end - self.start_index]);
        if let Some(symbol) = self.context.symbols.get(&hash) {
            *symbol
        } else {
            let symbol = Sym::new(self.context.rev_symbols.0.len() as u32);
            self.context.symbols.insert(hash, symbol);
            self.context.rev_symbols.0.push(location);
            symbol
        }
    }

    // the advantage compare to intern_string is str is only copied when creating new symbol
    // it is actually used in syntax parser
    pub fn intern_str(&mut self, value: &str) -> Sym {
        let hash = get_hash(&value);
        if let Some(symbol) = self.context.symbols.get(&hash) {
            *symbol
        } else {
            let symbol = Sym::new(self.context.rev_symbols.1.len() as u32 | Sym::MASK);
            self.context.symbols.insert(hash, symbol);
            self.context.rev_symbols.1.push(value.to_owned());
            symbol
        }
    }

    // the advantage compare to intern_str is no need to copy when creating new symbol
    pub fn intern_string(&mut self, value: String) -> Sym {
        let hash = get_hash(&value);
        if let Some(symbol) = self.context.symbols.get(&hash) {
            *symbol
        } else {
            let symbol = Sym::new(self.context.rev_symbols.1.len() as u32 | Sym::MASK);
            self.context.symbols.insert(hash, symbol);
            self.context.rev_symbols.1.push(value);
            symbol
        }
    }

    pub fn finish(mut self) -> FileId {
        // // no, not this, cannot move self when self is borrowed (the self.context)
        // // self.context.finish_build(self)
        
        let file_id = FileId((self.context.files.len() + 1) as u32);
        let content_length = self.content.len();
        self.content.truncate(content_length - 3);
        self.context.files.push(SourceFile{
            path: self.path,
            endlines: get_endlines(&self.content),
            content: self.content,
            namespace: self.namespace,
            start_index: self.start_index,
            request: self.request,
        });
        file_id
    }
}

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
