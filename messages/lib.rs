//! fff-lang
//!
//! Every kind of messages

#[cfg_attr(test, macro_use)] 
extern crate codemap;

use std::fmt;
use codemap::Span;
use codemap::CodeMap;

#[derive(Eq, PartialEq)]
struct LocationAndDesc {
    loc: Span, 
    desc: String,
}
impl From<(Span, String)> for LocationAndDesc {
    fn from(loc_and_desc: (Span, String)) -> LocationAndDesc {
        LocationAndDesc{ loc: loc_and_desc.0, desc: loc_and_desc.1 }
    }
}
#[derive(Eq, PartialEq)]
pub struct Message {
    main_desc: String, 
    details: Vec<LocationAndDesc>, // first is regarded as main
    helps: Vec<String>,
}
impl Message {    
    pub fn new(main_desc: String, pos_and_descs: Vec<(Span, String)>) -> Message {
        Message{ 
            main_desc: main_desc, 
            details: pos_and_descs.into_iter().map(|pos_and_desc| pos_and_desc.into()).collect(), 
            helps: Vec::new() 
        }
    }
    pub fn new_by_str(main_desc: &str, pos_and_descs: Vec<(Span, &str)>) -> Message {
        Message{ 
            main_desc: main_desc.to_owned(), 
            details: pos_and_descs.into_iter().map(|pos_and_desc| match pos_and_desc { (pos, desc) => (pos, desc.to_owned()).into() }).collect(),
            helps: Vec::new(),
        }
    }
    pub fn with_help(main_desc: String, pos_and_descs: Vec<(Span, String)>, helps: Vec<String>) -> Message {
        Message{ 
            main_desc: main_desc, 
            details: pos_and_descs.into_iter().map(|pos_and_desc| pos_and_desc.into()).collect(), 
            helps: helps 
        }
    }
    pub fn with_help_by_str(main_desc: &str, pos_and_descs: Vec<(Span, &str)>, helps: Vec<&str>) -> Message {
        Message{ 
            main_desc: main_desc.to_owned(), 
            details: pos_and_descs.into_iter().map(|pos_and_desc| match pos_and_desc { (pos, desc) => (pos, desc.to_owned()).into() }).collect(),
            helps: helps.into_iter().map(|help| help.to_owned()).collect(),
        }
    }
    pub fn new_simple(main_desc: &str) -> Message {
        Message{ main_desc: main_desc.to_owned(), details: Vec::new(), helps: Vec::new() }
    }

    pub fn format(&self, _codemap: &CodeMap) -> String {
        // TODO: think it is very complex and do it later, according to docs/internal/message-format.md
        String::new()
    }
}
impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        
        write!(f, "{}:", self.main_desc)?;
        for &LocationAndDesc{ ref loc, ref desc } in &self.details {
            write!(f, "\n   | At {:?}: {}", loc, desc)?;
        }
        for help in &self.helps {
            write!(f, "\n   = help: {}", help)?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq)]
pub struct MessageCollection {
    items: Vec<Message>,
    m_uncontinuable: bool,
}
impl fmt::Debug for MessageCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for message in &self.items {
            writeln!(f, "{:?}\n", message)?;
        }
        Ok(())
    }
}
impl MessageCollection {

    pub fn new() -> MessageCollection { MessageCollection{ items: Vec::new(), m_uncontinuable: false } }
    pub fn is_empty(&self) -> bool { self.items.is_empty() }

    pub fn push(&mut self, message: Message) {
        
        // Not repeat report error
        // Used in lexical/v2lexer/char::pass_non_ascii_char
        if self.items.len() == 0 || (self.items.len() > 0 && message != self.items[self.items.len() - 1]) {
            self.items.push(message);
        }
    }

    pub fn pop(&mut self) { let _ = self.items.pop(); }
    pub fn set_uncontinuable(&mut self) { self.m_uncontinuable = true; }
    pub fn is_uncontinuable(&self) -> bool { self.m_uncontinuable }
}

#[macro_export]
macro_rules! check_messages_continuable {
    ($msgs: expr) => (if $msgs.is_uncontinuable() { panic!("messages is uncontinuable: {:?}", $msgs) })
}
#[macro_export]
macro_rules! make_messages {
    ($($x:expr),*) => ({
        let mut retval = MessageCollection::new();
        {
            let _retval = &mut retval; // `&mut` for statisfy 'unused mut', `_` for statisfy unused var
            $(
                _retval.push($x);
            )*
        }
        retval
    });
    ($($x:expr,)*) => (make_messages![$($x),*])
}

#[cfg(test)] #[test]
fn messages_format() {

}

#[cfg(test)] #[test]
fn message_complex_new() {

    assert_eq!(
        Message::new_by_str("123", vec![
            (Span::default(), "456"),
            (Span::default(), "789"),
        ]), 
        Message::new("123".to_owned(), vec![
            (Span::default(), "456".to_owned()),
            (Span::default(), "789".to_owned()),
        ])
    );
}

#[cfg(test)] #[test]
fn message_by_macro() {

    let mut messages = MessageCollection::new();
    assert_eq!(messages, make_messages![]);

    messages.push(Message::new_by_str("a", vec![(make_span!(1, 1), "b")]));
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(make_span!(1, 1), "b")])]);
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), ]);

    messages.push(Message::new_by_str("c", vec![(make_span!(2, 3), "d")]));
    messages.push(Message::new_by_str("e", vec![(make_span!(2, 8), "f")]));
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), 
        Message::new_by_str("c", vec![(make_span!(2, 3), "d")]),
        Message::new_by_str("e", vec![(make_span!(2, 8), "f")])
    ]);
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(make_span!(1, 1), "b")]), 
        Message::new_by_str("c", vec![(make_span!(2, 3), "d")]),
        Message::new_by_str("e", vec![(make_span!(2, 8), "f")]),
    ]);
}