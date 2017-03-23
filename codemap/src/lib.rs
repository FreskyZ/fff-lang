#![allow(dead_code)]
///! fff-lang
///!
///! codemap, from source files to source file string's  
///! iterator<(char, position)>,  
///! which is <0.1.0>'s filemap and lexical's layer 0

#[macro_use] extern crate codepos;
extern crate messages;

use codepos::Position;
use messages::Message;

mod code_char;
mod code_file;

pub use code_char::EOFCHAR;
pub use code_char::EOFSCHAR;
pub use code_char::CodeChar;
use code_char::new_code_char;
use code_file::CodeFile;
use code_file::CodeFileIter;

// Iterator to get chars, this is my multi source file core logic processor
pub struct CodeChars<'a> {
    files: Vec<CodeFileIter<'a>>,
    current_index: Option<usize>,   // none for EOFs
    last_eofpos: Position,          // for furthur returns
}
impl<'a> CodeChars<'a> {

    fn new(codemap: &'a mut CodeMap) -> CodeChars {
        
        let current_index = if codemap.files.len() > 0 { Some(0) } else { None };
        CodeChars{ 
            files: codemap.files.iter_mut().map(|mut file| file.iter()).collect(),
            current_index: current_index,
            last_eofpos: Position::new(),
        }
    }

    pub fn next(&mut self) -> CodeChar {

        match self.current_index {
            Some(index) => {
                let ret_val = self.files[index].next();
                if ret_val.is_eof() {
                    self.last_eofpos = ret_val.pos();
                    if index == self.files.len() - 1 {
                        self.current_index = None;
                    } else {
                        self.current_index = Some(index + 1);
                    }
                }
                return ret_val;
            }
            None => return new_code_char(EOFSCHAR, self.last_eofpos.clone()),
        }
    }
}

// CodeMap, source code file and content manager
// # Examples
// ```
/// let mut codemap = CodeMap::with_str("123\ndef");
/// codemap.input_str("456");
/// let mut iter = codemap.iter();
/// assert_eq!(iter.next().as_tuple(), ('1', make_pos!(0, 1, 1)));
/// assert_eq!(iter.next().as_tuple(), ('2', make_pos!(0, 1, 2)));
/// assert_eq!(iter.next().as_tuple(), ('3', make_pos!(0, 1, 3)));
/// assert_eq!(iter.next().as_tuple(), ('\n', make_pos!(0, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), ('d', make_pos!(0, 2, 1)));
/// assert_eq!(iter.next().as_tuple(), ('e', make_pos!(0, 2, 2)));
/// assert_eq!(iter.next().as_tuple(), ('f', make_pos!(0, 2, 3)));;
/// assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(0, 2, 4)));
/// assert_eq!(iter.next().as_tuple(), ('4', make_pos!(1, 1, 1)));
/// assert_eq!(iter.next().as_tuple(), ('5', make_pos!(1, 1, 2)));
/// assert_eq!(iter.next().as_tuple(), ('6', make_pos!(1, 1, 3)));
/// assert_eq!(iter.next().as_tuple(), (EOFCHAR, make_pos!(1, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(1, 1, 4)));
/// assert_eq!(iter.next().as_tuple(), (EOFSCHAR, make_pos!(1, 1, 4)));
// ```
pub struct CodeMap {
    files: Vec<CodeFile>,
}
impl CodeMap {
    
    pub fn new() -> CodeMap {
        CodeMap{ files: Vec::new() }
    }
    
    // helper function to make it simple
    pub fn with_files(file_names: Vec<String>) -> Result<CodeMap, Message> {
        let mut ret_val = CodeMap::new();
        ret_val.input_files(file_names).map(|_| ret_val)
    }
    pub fn with_str(content: &str) -> CodeMap {
        let mut ret_val = CodeMap::new();
        ret_val.input_str(content);
        ret_val
    }
}
impl CodeMap {

    // any error abort
    pub fn input_files(&mut self, file_names: Vec<String>) -> Result<(), Message> {

        for (index, file_name) in file_names.into_iter().enumerate() {
  
            self.files.push(try!(CodeFile::from_file(index as u32, file_name)));
        }

        Ok(())
    }
    pub fn input_str(&mut self, content: &str) {

        let len = self.files.len() as u32;
        self.files.push(CodeFile::from_str(len, content));
    }

    pub fn iter<'a>(&'a mut self) -> CodeChars<'a> {
        CodeChars::new(self)
    }

    // pub fn get_line(file_id: u32, line: u32) -> &str {

    // }
    // pub fn format_message(message: Message) -> String {

    // }
}

#[cfg(test)] #[test]
fn codemap_input() {
    use self::code_char::EOFCHAR;

    let file1 = "../tests/codemap/file1.ff".to_owned();
    let file2 = "../tests/codemap/file2.ff".to_owned();
    let mut codemap = CodeMap::new();

    let _ = codemap.input_files(vec![file1, file2]).expect("error input files");
    let _ = codemap.input_str("some\nstr\n");

    // current feature is, simple merge and yield char
    let mut iter = codemap.iter();
    assert_eq!(iter.next(), new_code_char('a', make_pos!(0, 1, 1)));
    assert_eq!(iter.next(), new_code_char('b', make_pos!(0, 1, 2)));
    assert_eq!(iter.next(), new_code_char('c', make_pos!(0, 1, 3)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(0, 1, 4)));
    assert_eq!(iter.next(), new_code_char('d', make_pos!(0, 2, 1)));
    assert_eq!(iter.next(), new_code_char('e', make_pos!(0, 2, 2)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(0, 2, 3)));
    assert_eq!(iter.next(), new_code_char('f', make_pos!(0, 3, 1)));
    assert_eq!(iter.next(), new_code_char('g', make_pos!(0, 3, 2)));
    assert_eq!(iter.next(), new_code_char('h', make_pos!(0, 3, 3)));
    assert_eq!(iter.next(), new_code_char(EOFCHAR, make_pos!(0, 3, 4))); 
    assert_eq!(iter.next(), new_code_char('i', make_pos!(1, 1, 1)));
    assert_eq!(iter.next(), new_code_char('j', make_pos!(1, 1, 2)));
    assert_eq!(iter.next(), new_code_char('k', make_pos!(1, 1, 3)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(1, 1, 4)));
    assert_eq!(iter.next(), new_code_char('l', make_pos!(1, 2, 1)));
    assert_eq!(iter.next(), new_code_char('m', make_pos!(1, 2, 2)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(1, 2, 3)));
    assert_eq!(iter.next(), new_code_char(EOFCHAR, make_pos!(1, 3, 1)));
    assert_eq!(iter.next(), new_code_char('s', make_pos!(2, 1, 1)));
    assert_eq!(iter.next(), new_code_char('o', make_pos!(2, 1, 2)));
    assert_eq!(iter.next(), new_code_char('m', make_pos!(2, 1, 3)));
    assert_eq!(iter.next(), new_code_char('e', make_pos!(2, 1, 4)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(2, 1, 5)));
    assert_eq!(iter.next(), new_code_char('s', make_pos!(2, 2, 1)));
    assert_eq!(iter.next(), new_code_char('t', make_pos!(2, 2, 2)));
    assert_eq!(iter.next(), new_code_char('r', make_pos!(2, 2, 3)));
    assert_eq!(iter.next(), new_code_char('\n', make_pos!(2, 2, 4)));
    assert_eq!(iter.next(), new_code_char(EOFCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next(), new_code_char(EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next(), new_code_char(EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next(), new_code_char(EOFSCHAR, make_pos!(2, 3, 1)));
    assert_eq!(iter.next(), new_code_char(EOFSCHAR, make_pos!(2, 3, 1)));
}

#[cfg(test)]
mod cool_driver_feas {

    mod detail {

        #[derive(Debug, Eq, PartialEq)]
        pub struct MessageCollectionMapper<'a, T>{
            ret_val: T,
            messages: &'a mut MessageCollection,
        }
        impl<'a, TCurrent> MessageCollectionMapper<'a, TCurrent> {
            fn new(ret_val: TCurrent, messages: &'a mut MessageCollection) -> MessageCollectionMapper<'a, TCurrent> {
                MessageCollectionMapper{ ret_val: ret_val, messages: messages }
            }

            pub fn map<
                TNext: IMessageCollectionMapResult, 
                TFn: FnOnce(TCurrent) -> (TNext, MessageCollection)>
                (self, f: TFn) -> MessageCollectionMapper<'a, TNext> {
                
                if self.messages.continuable {
                    let (result, messages) = f(self.ret_val);
                    self.messages.merge(messages);
                    MessageCollectionMapper::new(result, self.messages)
                } else {
                    MessageCollectionMapper::new(TNext::empty(), self.messages)
                }
            }

            pub fn map_err<TFn: FnOnce(&MessageCollection)>(self, f: TFn) {
                f(self.messages);
            }
        }

        pub trait IMessageCollectionMapResult {
            fn empty() -> Self;
        }
        impl IMessageCollectionMapResult for () {
            fn empty() -> () { () }
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct MessageCollection {
            continuable: bool,
            errors: Vec<String>,
        }
        impl MessageCollection {
            pub fn new() -> MessageCollection {
                MessageCollection{ 
                    continuable: true,
                    errors: Vec::new(),
                }
            }
            pub fn push(&mut self, error: &str) {
                self.errors.push(error.to_owned());
            }

            pub fn is_continuable(&self) -> bool { self.continuable }
            fn merge(&mut self, mut other: MessageCollection) {
                self.errors.append(&mut other.errors);
                self.continuable = self.continuable && other.continuable;
            }
            pub fn set_uncontinuable(&mut self) {
                self.continuable = false;
            }

            pub fn map<
                TResult: IMessageCollectionMapResult, 
                TFn: FnOnce(i32) -> (TResult, MessageCollection)>
                (&mut self, f: TFn) -> MessageCollectionMapper<TResult> {
                
                if self.continuable {
                    let (result, messages) = f(0);
                    self.merge(messages);
                    MessageCollectionMapper::new(result, self)
                } else {
                    MessageCollectionMapper::new(TResult::empty(), self)
                }
            }
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct CodeMapIter<'a>{
            map: &'a mut CodeMap,
            j: i32,
        }
        impl<'a> CodeMapIter<'a> {
            pub fn next(&mut self) -> i32 {
                self.map.i += 1;
                self.j = self.map.i;
                self.j
            }
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct CodeMap {
            i: i32,
        }
        impl IMessageCollectionMapResult for CodeMap {
            fn empty() -> CodeMap {
                CodeMap{ i: -1 }
            }
        }
        impl CodeMap {
            pub fn new(file_names: Vec<String>, set_uncontinuable: bool) -> (CodeMap, MessageCollection) {

                println!("CodeMap::new(), file_names: {:?}, set_uncontinuable: {}", file_names, set_uncontinuable);
                let mut messages = MessageCollection::new();
                messages.push("codemap continuable error");
                if set_uncontinuable { 
                    messages.push("codemap uncontinuable error");
                    messages.set_uncontinuable(); 
                }

                (CodeMap{ i: 0 }, messages)
            }
            pub fn iter<'a>(&'a mut self) -> CodeMapIter<'a> {
                CodeMapIter{ map: self, j: 0 }
            }
            pub fn format(&self, messages: &MessageCollection) -> String {
                format!("{:?}", messages)
            }
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct TokenStream {
            tokens: Vec<i32>,
        }
        impl IMessageCollectionMapResult for TokenStream {
            fn empty() -> TokenStream {
                TokenStream{ tokens: Vec::new() }
            }
        }
        pub fn lexical_parse<'a>(mut chars: CodeMapIter<'a>, set_uncontinuable: bool) -> (TokenStream, MessageCollection) {

            println!("lexical_parse, chars: {:?}, set_uncontinuable: {}", chars, set_uncontinuable);
            let mut messages = MessageCollection::new();
            messages.push("lexical_parse continuable error");
            if set_uncontinuable { 
                messages.push("lexical_parse uncontinuable error");
                messages.set_uncontinuable(); 
            }

            let mut result = Vec::new();
            result.push(chars.next());
            result.push(chars.next());
            (TokenStream{ tokens: result }, messages)
        }

        #[derive(Debug, Eq, PartialEq)]
        pub struct SyntaxTree {
            items: Vec<String>,
        }
        impl IMessageCollectionMapResult for SyntaxTree {
            fn empty() -> SyntaxTree {
                SyntaxTree{ items: Vec::new() }
            }
        }
        pub fn syntax_parse(token_stream: TokenStream, set_uncontinuable: bool) -> (SyntaxTree, MessageCollection) {

            println!("syntax_parase, token_stream: {:?}, set_uncontinuable: {}", token_stream, set_uncontinuable);
            let mut messages = MessageCollection::new();
            messages.push("syntax_parase continuable error");
            if set_uncontinuable { 
                messages.push("syntax_parase uncontinuable error");
                messages.set_uncontinuable(); 
            }

            let mut result = Vec::new();
            for token in token_stream.tokens {
                result.push(format!("{}", token));
            }
            (SyntaxTree{ items: result }, messages)
        }

        pub fn vm_run(syntax_tree: SyntaxTree, set_uncontinuable: bool) -> ((), MessageCollection) {

            println!("vm_run, syntax_tree: {:?}, set_uncontinuable: {}", syntax_tree, set_uncontinuable);
            let mut messages = MessageCollection::new();
            messages.push("vm_run continuable error");
            if set_uncontinuable { 
                messages.push("vm_run uncontinuable error");
                messages.set_uncontinuable(); 
            }

            let mut output = String::new();
            for item in syntax_tree.items {
                output += &format!("{}", item);
            }
            println!("vm_run, output: {}", output);
            ((), messages)
        }
    }

    #[test]
    fn cool_driver_test() {
        use self::detail::MessageCollection;
        use self::detail::CodeMap;
        use self::detail::lexical_parse;
        use self::detail::syntax_parse;
        use self::detail::vm_run;

        println!(); 

        let file_name = "file1.ff".to_owned();
        
        // TODO: try `struct MessageCollectionAndReturnValue<T>(T, MessageCollection)
        // Attention:
        //     can I impl `messages::IMessageCollectionResult` for `messages::MessageCollectionAndRetVal<syntax::SyntaxTree>`
        //     and call it's `map` in `driver`?
        MessageCollection::new()
            .map(|_| CodeMap::new(vec![file_name], false))
            .map(|mut codemap| lexical_parse(codemap.iter(), false))
            .map(|tokens| syntax_parse(tokens, false))
            .map(|ast| vm_run(ast, false))
            .map_err(|errs| { println!("\nerrors: {:?}", errs); });
        
        assert!(false);
    }
}