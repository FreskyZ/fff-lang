///! source: read file and provide character iterator, manage locations and symbols

#[cfg(test)]
mod tests;
#[cfg(test)]
pub use tests::VirtualFileSystem;

use std::collections::HashMap;
use std::path::{PathBuf, Path};

/// Character location
///
/// - it is byte index accross all source files, e.g. second file's position starts from first file's byte length (+1)
///   to reduce memory usage because location info is used extremely widely
/// - it is u32 not usize because it is not reasonable to
///   have a file size over 4GB or all source file total size over 4GB for this toy language (possibly for all languages)
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct Position(u32);

impl Position {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn into(self) -> u32 {
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

/// a handle to an interned string
///
/// it is u32 not usize because it is widely used
/// and not reasonable to have more than u32::MAX symbols in all source files
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct SymId(u32);

impl SymId {
    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn into(self) -> u32 {
        self.0
    }
}
impl From<u32> for SymId {
    fn from(v: u32) -> Self {
        Self(v)
    }
}

/// a handle to a file
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct FileId(u32);
impl FileId {
    pub const ENTRY: FileId = FileId(1);

    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn into(self) -> u32 {
        self.0
    }
    pub fn is_entry(self) -> bool {
        self.0 == 1
    }
}
impl From<u32> for FileId {
    fn from(v: u32) -> Self {
        Self(v)
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

// use `span1 + span2` to merge them
impl std::ops::Add for Span {
    type Output = Span;
    fn add(self, rhs: Span) -> Span {
        Span{ start: self.start, end: rhs.end }
    }
}

// e.g. `position.into()` in where span is expected
impl From<Position> for Span {
    fn from(position: Position) -> Span {
        Span::new(position, position)
    }
}

#[derive(Debug)]
pub struct SourceFile {
    path: PathBuf,         // absolute path
    content: String,
    namespace: Vec<SymId>, // empty for entry module
    start_index: usize,    // starting byte index in span to this file, or total byte length before SourceContext.files this item
    endlines: Vec<usize>,  // LF byte indexes
    #[allow(dead_code)] // does not know where need it, but save it for now
    request: Option<Span>, // module item location, file id also can be found by span
}

pub struct Chars<'a> {
    index: usize,   // index of next character, initial value should be SourceFile.start_index
    slice: &'a str, // advancing string slice of SourceFile.content
}

pub const EOF: char = 0u8 as char;

impl<'a> Chars<'a> {
    /// iterate return char and byte index
    /// 
    /// ignore all bare or not bare CR, return EOF after EOF, fuse
    pub fn next(&mut self) -> (char, Position) {
        loop {
            if self.slice.len() == 0 {
                return (EOF, Position::new(self.index as u32));
            } else if self.slice.as_bytes()[0] == b'\r' {
                self.slice = &self.slice[1..];
                self.index += 1;
                continue;
            } else {
                let bytes = self.slice.as_bytes();
                let (char_length, r#char) = match get_char_length(self.slice, 0) {
                    1 => (1, bytes[0] as u32),
                    2 => (2, (((bytes[0] as u32) & 0b00011111u32) << 6) + ((bytes[1] as u32) & 0b00111111u32)),
                    3 => (3, (((bytes[0] as u32) & 0b00001111u32) << 12) + (((bytes[1] as u32) & 0b00111111u32) << 6) + (((bytes[2] as u32) & 0b00111111u32))),
                    4 => (4, (((bytes[0] as u32) & 0b00000111u32) << 18) + (((bytes[1] as u32) & 0b00111111u32) << 12) + (((bytes[2] as u32) & 0b00111111u32) << 6) + ((bytes[3] as u32) & 0b00111111u32)),
                    _ => panic!("invalid utf-8 sequence"),
                };
                self.slice = &self.slice[char_length..];
                self.index += char_length;
                // SAFETY: invalid char should not cause severe issue in lexical parse and syntax parse
                return (unsafe { char::from_u32_unchecked(r#char) }, Position::new((self.index - char_length) as u32));
            }
        }
    }
}

fn get_relative_path(base_path: &Path, module_path: &Path) -> PathBuf {
    use std::path::{Component, Prefix};

    let mut base_components = base_path.components();
    let mut module_components = module_path.components(); 
    let mut dotdot_count = 0;
    let mut normal_components = Vec::new();
    // for each component pair (c1, c2)
    //    if c1 == c2, continue,
    //    for first c1 != c2, count dotdot = 0, start recording c2's component in c2_mores
    //    then for next Some(c1)s, add dotdot_count 1, until c1 is none
    //    and for next Some(c2)s, push c2_mores, until c2 is none
    loop {
        match (base_components.next(), module_components.next()) {
            (None, None) => break,
            (c1, c2) if c1 == c2 => continue,
            // to make `\\?\C:\` same as `C:\`
            // ... every time handling `\\?\` makes me think about life
            (Some(Component::Prefix(prefix_component1)), Some(Component::Prefix(prefix_component2))) => {
                match (prefix_component1.kind(), prefix_component2.kind()) {
                    (Prefix::VerbatimDisk(volumn_name1), Prefix::Disk(volumn_name2)) 
                    | (Prefix::Disk(volumn_name1), Prefix::VerbatimDisk(volumn_name2)) if volumn_name1 == volumn_name2 => continue,
                    _ => (),
                }
                return module_path.to_owned(); // even prefix is not same, then quick return
            }
            (Some(_), Some(c2)) => { dotdot_count += 1; normal_components.push(c2); }
            (Some(_), None) => { dotdot_count += 1; }
            (None, Some(c2)) => { normal_components.push(c2); }
        }
    }
    let mut result = vec![Component::ParentDir; dotdot_count]; // ... very rare `vec!` usage
    result.extend(normal_components);
    result.into_iter().collect()
}

impl SourceFile {
    pub fn chars(&self) -> Chars {
        Chars{ index: self.start_index, slice: &self.content }
    }

    /// get relative path to current working directory
    #[allow(dead_code)] // should be used in diagnostics formatter
    pub fn get_relative_path(&self) -> PathBuf {
        let cwd = std::env::current_dir().expect("cannot get current dir");
        get_relative_path(&cwd, &self.path)
    }
}

// allow virtual file system for test
pub trait FileSystem {
    fn canonicalize(&self, path: impl AsRef<Path>) -> std::io::Result<PathBuf>;
    fn read_to_string(&self, path: impl AsRef<Path>) -> std::io::Result<String>;
}

#[derive(Debug)]
pub struct DefaultFileSystem;

impl Default for DefaultFileSystem {
    fn default() -> Self { 
        Self
    }
}
impl FileSystem for DefaultFileSystem {
    fn canonicalize(&self, path: impl AsRef<Path>) -> std::io::Result<PathBuf> {
        std::fs::canonicalize(path)
    }
    fn read_to_string(&self, path: impl AsRef<Path>) -> std::io::Result<String> {
        std::fs::read_to_string(path)
    }
}

/// source context contains all things (data and operations) related with source code
///
/// because all other types (Position, Span, SymId, FileId) contains contextless number value
/// and only with SourceContext can you explain what they are and validate their usages
#[derive(Debug)]
pub struct SourceContext<F = DefaultFileSystem> {
    fs: F,
    // file id starts from 1, 1 is entry module, entry module is 1
    files: Vec<SourceFile>,
    // map string content to symbol id, but cannot reference self.files[...].content, so key is already hashed
    // symbol id starts from 1 for span and from 0x1000_0000 for value
    symbols: HashMap<u64, SymId>,
    // reverse map symbol id to span to symbol content, item 0 is dummy
    span_symbols: Vec<Span>,
    // reverse map symbol id to symbol content, clearing first bit of symbol id value
    // // this vec string is the only thing to make it look like string intern facility compare to previous u64 and span
    value_symbols: Vec<String>,
}

impl<F> SourceContext<F> where F: Default {
    pub fn new() -> Self {
        Self { fs: Default::default(), files: Vec::new(), symbols: HashMap::new(), span_symbols: vec![Span::new(0, 0)], value_symbols: Vec::new() }
    }
}
impl<F> SourceContext<F> {
    pub fn new_file_system(fs: F) -> Self {
        Self{ fs, files: Vec::new(), symbols: HashMap::new(), span_symbols: vec![Span::new(0, 0)], value_symbols: Vec::new() }
    }
}

// get LF byte indexes
fn get_endlines(content: &str) -> Vec<usize> {
    content.char_indices().filter(|(_, c)| c == &'\n').map(|(i, _)| i).collect()
}

// get utf8 length of character at byte index
fn get_char_length(content: &str, byte_index: usize) -> usize {
    content[byte_index..].chars().next().unwrap().len_utf8()
}

const FILE_EXT: &str = ".f3";
const INDEX_FILE: &str = "index.f3";

impl<F> SourceContext<F> where F: FileSystem {

    pub fn entry(&mut self, path: PathBuf) {
        // simply panic for cannot read entry file, since it is really early critical error
        let path = self.fs.canonicalize(&path).expect("cannot read entry");
        let content = self.fs.read_to_string(&path).expect("cannot read entry");

        self.files.push(SourceFile{ path, endlines: get_endlines(&content), content, start_index: 0, namespace: Vec::new(), request: None });
    }

    // return option not result: let syntax parse module declare raise error
    pub fn import(&mut self, request: Span, module_name_symbol_id: SymId) -> Option<FileId> {
        let (request_file_id, request_file, _) = self.map_position_to_file_and_byte_index(request.start);
        
        let module_name = self.resolve_symbol(module_name_symbol_id);
        let hyphened_module_name = module_name.replace('_', "-");
        let module_name_with_ext = format!("{}{}", module_name, FILE_EXT);
        let hyphened_module_name_with_ext = format!("{}{}", hyphened_module_name, FILE_EXT);
        
        let parent_path = request_file.path.parent().expect("request file is root");
        macro_rules! make_paths { ($([$($c:expr),+],)+) => { vec![$( parent_path.join([$($c,)+].into_iter().collect::<PathBuf>()), )+] } }

        let resolve_options = if request_file_id.is_entry() || request_file.path.ends_with("index.f3") { // path.ends_with checks last *component* not string content
            make_paths!{   
                [&hyphened_module_name_with_ext],
                [&hyphened_module_name, INDEX_FILE],
                [module_name_with_ext],
                [module_name, INDEX_FILE],
            }
        } else { 
            let this_module_name_symbol_id = request_file.namespace.last().expect("namespace component missing");
            let this_module_name = self.resolve_symbol(*this_module_name_symbol_id);
            let hyphened_this_module_name = this_module_name.replace('_', "-");
            make_paths![
                [&hyphened_this_module_name, &hyphened_module_name_with_ext],
                [&hyphened_this_module_name, &hyphened_module_name, INDEX_FILE],
                [&hyphened_this_module_name, &module_name_with_ext],
                [&hyphened_this_module_name, module_name, INDEX_FILE],
                [this_module_name, &hyphened_module_name_with_ext],
                [this_module_name, &hyphened_module_name, INDEX_FILE],
                [this_module_name, &module_name_with_ext],
                [this_module_name, module_name, INDEX_FILE],
            ]
        };

        // borrowck says if put this sentence in closure then it borrows self.files so cannot borrow self.files mutably (to insert)
        let mut namespace = request_file.namespace.clone();
        // RFINRE: read failure is not module resolution error, simply regard read/open error as not exist
        resolve_options.into_iter().filter_map(|p| self.fs.read_to_string(&p).map(|c| (p, c)).ok()).next().map(|(path, content)| {
            let file_id = FileId::new(self.files.len() as u32 + 1);
            namespace.push(module_name_symbol_id);
            let last_file = self.files.last().expect("unexpected empty files");
            let start_index = last_file.start_index + last_file.content.len() + 1; // +1 for position for EOF
            self.files.push(SourceFile{ path, endlines: get_endlines(&content), content, start_index, namespace, request: Some(request) });
            file_id
        })
    }

    pub fn get_file(&self, file_id: FileId) -> &SourceFile {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.len(), "invalid file id");
        &self.files[file_id - 1]
    }
}

impl<F> SourceContext<F> {

    /// return (file id, file, byte index in current file)
    fn map_position_to_file_and_byte_index(&self, position: Position) -> (FileId, &SourceFile, usize) {
        let position = position.0 as usize;
        for (index, file) in self.files.iter().enumerate() {
            if file.start_index + file.content.len() + /* EOF position */ 1 > position {
                return (FileId::new(index as u32 + 1), file, position - file.start_index);
            }
        }
        unreachable!("position overflow");
    }

    #[allow(dead_code)]
    pub fn map_position_to_file(&self, position: Position) -> FileId {
        self.map_position_to_file_and_byte_index(position).0
    }

    /// line starts from 1, column starts from 1
    pub fn map_position_to_line_column(&self, position: Position) -> (FileId, usize, usize) {
        let (file_id, file, byte_index) = self.map_position_to_file_and_byte_index(position);

        let (line, line_start_index) = if file.endlines.len() == 0 {
            (1, 0)
        } else if byte_index <= file.endlines[0] {
            (1, 0)
        } else {
            // rev iterate through endlines to find input byte index's range
            // `+2` for index = endlines[*0*] + 1 is line *2*
            // `+1` for LF must be 1 byte width, endlines[xxx] + 1 is exactly first char of next line
            // logically must find, so directly unwrap
            file.endlines.iter().enumerate().rev()
                .find(|&(_, &endl_index)| byte_index > endl_index).map(|(item_index, endl_index)| (item_index + 2, endl_index + 1)).unwrap()
        };

        let mut column = 1;
        let mut current_byte_index = line_start_index;
        loop {
            if current_byte_index == byte_index { // exact match
                return (file_id, line, column);
            } else if current_byte_index == file.content.len() {
                return (file_id, line, column);   // allow position for EOF char 
            } else if current_byte_index > file.content.len() {
                panic!("position overflow");
            } else if file.content.as_bytes()[current_byte_index] != b'\r' { // ignore \r in column counting
                column += 1;
            }
            current_byte_index += get_char_length(&file.content, current_byte_index);
        }
    }

    /// (file, start line, start column, end line, end column), line starts from 1, column starts from 1
    pub fn map_span_to_line_column(&self, location: Span) -> (FileId, usize, usize, usize, usize) {
        let (start_file, start_line, start_column) = self.map_position_to_line_column(location.start);
        let (end_file, end_line, end_column) = self.map_position_to_line_column(location.end);

        debug_assert_eq!(start_file, end_file, "span cross file");
        (start_file, start_line, start_column, end_line, end_column)
    }

    pub fn map_span_to_content(&self, location: Span) -> &str {
        debug_assert!(location.start.0 <= location.end.0, "invalid span");

        let (start_file_id, file, start_byte_index) = self.map_position_to_file_and_byte_index(location.start);
        let (end_file_id, _, end_byte_index) = self.map_position_to_file_and_byte_index(location.end);

        debug_assert_eq!(start_file_id, end_file_id, "span cross file");
        // these 2 will not happen because map_position_to_file_and_byte_index already rejected them
        // debug_assert!(start_byte_index <= file.content.len(), "span overflow");
        // debug_assert!(end_byte_index <= file.content.len(), "span overflow");
        
        if end_byte_index == file.content.len() {
            // allow EOF position
            if start_byte_index == file.content.len() {
                ""
            } else {
                &file.content[start_byte_index..]
            }
        } else {
            &file.content[start_byte_index..end_byte_index + get_char_length(&file.content, end_byte_index)]
        }
    }

    #[allow(dead_code)] // diagnostics formatting not implemented currently
    pub fn map_line_to_content(&self, file_id: FileId, line: usize) -> &str {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.len(), "invalid file id");

        let file = &self.files[file_id - 1];
        debug_assert!(line > 0 && line <= file.endlines.len() + 1, "line number overflow");

        if file.content.len() == 0 { // empty file
            ""
        } else if file.endlines.is_empty() { // only one line
            &file.content
        } else if line == 1 && file.endlines[0] == 0 { // first line is empty
            ""
        } else {
            let start_byte_index = if line == 1 { 0 } else { file.endlines[line - 2] + /* next char of LF */ 1 };
            let end_byte_index = if line == file.endlines.len() + 1 { file.content.len() - 1 } else { file.endlines[line - 1] - 1 };
            &file.content[start_byte_index..end_byte_index + get_char_length(&file.content, end_byte_index)]
        }
    }
}

const SYMID_MASK: u32 = 0x1000_0000u32;

fn get_hash(content: &str) -> u64 {
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    std::hash::Hash::hash(content, &mut hasher);
    std::hash::Hasher::finish(&hasher)
}

impl<F> SourceContext<F> {

    // intern symbol at location
    pub fn intern_span(&mut self, location: Span) -> SymId {
        debug_assert!(location.start.0 <= location.end.0, "invalid span");

        let hash = get_hash(self.map_span_to_content(location));
        if let Some(symbol_id) = self.symbols.get(&hash) {
            *symbol_id
        } else {
            let symbol_id = SymId::new(self.span_symbols.len() as u32);
            self.symbols.insert(hash, symbol_id);
            self.span_symbols.push(location);
            symbol_id
        }
    }

    #[allow(dead_code)] // TODO intern string literal value
    pub fn intern_value(&mut self, value: String) -> SymId {
        let hash = get_hash(&value);
        if let Some(symbol_id) = self.symbols.get(&hash) {
            *symbol_id
        } else {
            let symbol_id = SymId::new(self.value_symbols.len() as u32 | SYMID_MASK);
            self.symbols.insert(hash, symbol_id);
            self.value_symbols.push(value);
            symbol_id
        }
    }

    pub fn resolve_symbol(&self, symbol_id: SymId) -> &str {
        let symbol_id = symbol_id.0;
        debug_assert!(symbol_id > 0, "invalid symbol id");

        if (symbol_id & SYMID_MASK) == SYMID_MASK {
            let index = (symbol_id & !SYMID_MASK) as usize;
            debug_assert!(index < self.value_symbols.len(), "invalid symbol id");
            &self.value_symbols[index]
        } else {
            debug_assert!((symbol_id as usize) < self.span_symbols.len(), "invalid symbol id");
            self.map_span_to_content(self.span_symbols[symbol_id as usize])
        }
    }
}
