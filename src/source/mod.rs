///! source: read file and provide character iterator, manage locations and symbols

#[cfg(test)]
mod tests;
#[cfg(test)]
pub use fs::VirtualFileSystem;

use std::path::{PathBuf, Path};

mod fs;
pub use fs::{FileSystem, DefaultFileSystem};
mod iter;
use iter::get_char_width;
pub use iter::{Position, Span, Chars, EOF};
mod symbol;
use symbol::{Symbols, ResolveResult};
pub use symbol::SymId;

/// a handle to a file
#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub struct FileId(u32);
impl FileId {
    pub const ENTRY: FileId = FileId(1);

    pub fn new(v: u32) -> Self {
        Self(v)
    }
    pub fn unwrap(self) -> u32 {
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

// get LF byte indexes
fn get_endlines(content: &str) -> Vec<usize> {
    content.char_indices().filter(|(_, c)| c == &'\n').map(|(i, _)| i).collect()
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

// see SourceContext::get_chars why this type exists
#[derive(Debug)]
struct SourceFiles {
    // file id is item index + 1, so it starts from 1, 1 is the entry module, the entry module is 1
    items: Vec<SourceFile>,
}

/// source context contains all things (data and operations) related with source code
///
/// because all other types (Position, Span, SymId, FileId) contains contextless number value
/// and only with SourceContext can you explain what they are and validate their usages
#[derive(Debug)]
pub struct SourceContext<F = DefaultFileSystem> {
    fs: F,
    files: SourceFiles,
    symbols: Symbols,
}

impl<F> SourceContext<F> where F: Default {
    pub fn new() -> Self {
        Self { fs: Default::default(), files: SourceFiles::new(), symbols: Symbols::new() }
    }
}
impl<F> SourceContext<F> {
    pub fn new_file_system(fs: F) -> Self {
        Self{ fs, files: SourceFiles::new(), symbols: Symbols::new() }
    }
}

const FILE_EXT: &str = ".f3";
const INDEX_FILE: &str = "index.f3";

// methods about source file itself
impl<F> SourceContext<F> where F: FileSystem {

    pub fn entry(&mut self, path: PathBuf) {
        // simply panic for cannot read entry file, since it is really early critical error
        let path = self.fs.canonicalize(&path).expect("cannot read entry");
        let content = self.fs.read_to_string(&path).expect("cannot read entry");

        self.files.items.push(SourceFile{ path, endlines: get_endlines(&content), content, start_index: 0, namespace: Vec::new(), request: None });
    }

    // return option not result: let syntax parse module declare raise error
    pub fn import(&mut self, request: Span, module_name_symbol_id: SymId) -> Option<FileId> {
        let (request_file_id, request_file, _) = self.files.map_position_to_file_and_byte_index(request.start);
        
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
            let file_id = FileId::new(self.files.items.len() as u32 + 1);
            namespace.push(module_name_symbol_id);
            let last_file = self.files.items.last().expect("unexpected empty files");
            let start_index = last_file.start_index + last_file.content.len() + 1; // +1 for position for EOF
            self.files.items.push(SourceFile{ path, endlines: get_endlines(&content), content, start_index, namespace, request: Some(request) });
            file_id
        })
    }
    
    #[allow(dead_code)] // should be used in diagnostics formatter
    pub fn get_file(&self, file_id: FileId) -> &SourceFile {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.items.len(), "invalid file id");
        &self.files.items[file_id - 1]
    }

    // it is not in SourceFile because it will use vfs
    /// get relative path to current working directory
    #[allow(dead_code)] // should be used in diagnostics formatter
    pub fn get_relative_path(&self, file_id: FileId) -> PathBuf {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.items.len(), "invalid file id");
        let file = &self.files.items[file_id - 1];
        // TODO: vfs
        let cwd = std::env::current_dir().expect("cannot get current dir");
        get_relative_path(&cwd, &file.path)
    }

    // it is not in SourceFile because it borrows a lot of self
    pub fn get_chars(&mut self, file_id: FileId) -> Chars {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.items.len(), "invalid file id");
        let file = &self.files.items[file_id - 1];
        // only split self into separate SourceFiles and Symbols type can borrowck understand that
        Chars{ slice: &file.content, index: file.start_index, files: &self.files, symbols: &mut self.symbols }
    }
}

// these are also methods for giving numeric ids meaning while it is actually splitted from SourceContext so put them here
impl SourceFiles {
    fn new() -> Self {
        Self{ items: Vec::new() }
    }

    /// return (file id, file, byte index in current file)
    fn map_position_to_file_and_byte_index(&self, position: Position) -> (FileId, &SourceFile, usize) {
        let position = position.0 as usize;
        for (index, file) in self.items.iter().enumerate() {
            if file.start_index + file.content.len() + /* EOF position */ 1 > position {
                return (FileId::new(index as u32 + 1), file, position - file.start_index);
            }
        }
        unreachable!("position overflow");
    }

    fn map_span_to_content(&self, location: Span) -> &str {
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
            &file.content[start_byte_index..end_byte_index + get_char_width(&file.content, end_byte_index)]
        }
    }
}

// methods for giving numeric ids meaning
impl<F> SourceContext<F> {

    #[allow(dead_code)]
    pub fn map_position_to_file(&self, position: Position) -> FileId {
        self.files.map_position_to_file_and_byte_index(position).0
    }

    /// line starts from 1, column starts from 1
    pub fn map_position_to_line_column(&self, position: Position) -> (FileId, usize, usize) {
        let (file_id, file, byte_index) = self.files.map_position_to_file_and_byte_index(position);

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
            current_byte_index += get_char_width(&file.content, current_byte_index);
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
        self.files.map_span_to_content(location)
    }

    #[allow(dead_code)] // diagnostics formatting not implemented currently
    pub fn map_line_to_content(&self, file_id: FileId, line: usize) -> &str {
        let file_id = file_id.0 as usize;
        debug_assert!(file_id > 0 && file_id <= self.files.items.len(), "invalid file id");

        let file = &self.files.items[file_id - 1];
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
            &file.content[start_byte_index..end_byte_index + get_char_width(&file.content, end_byte_index)]
        }
    }

    pub fn resolve_symbol(&self, symbol_id: SymId) -> &str {
        match self.symbols.resolve(symbol_id) {
            ResolveResult::Span(span) => self.files.map_span_to_content(span),
            ResolveResult::Str(content) => content,
        }
    }
}
