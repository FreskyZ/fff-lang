#![macro_use]

use std::fmt;
use crate::source::{Span, SourceContext};

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

    pub fn format(&self, scx: Option<&SourceContext>) -> String {
        let mut retval = format!("{}:", self.main_desc);
        for LocationAndDesc{ loc, desc } in &self.details {
            retval += &format!("\n   | At {}: {}", scx.map(|scx| format!("{:?}", scx.map_span_to_line_column(*loc))).unwrap_or(String::new()), desc);
        }
        for help in &self.helps {
            retval += &format!("\n   = help: {}", help);
        }
        return retval;
    }
}
impl fmt::Debug for Message {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(None)) }
}

#[derive(Eq, PartialEq)]
pub struct MessageCollection {
    items: Vec<Message>,
    m_uncontinuable: bool,
}
impl Default for MessageCollection {
    fn default() -> MessageCollection { MessageCollection{ items: Vec::new(), m_uncontinuable: false } }
}
impl MessageCollection {
    pub fn format(&self, source: Option<&SourceContext>) -> String {
        let mut retval = String::new();
        for message in &self.items {
            retval += &format!("{}\n", message.format(source));
        }
        retval
    }
}
impl fmt::Debug for MessageCollection {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(None)) }
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
            (Span::new(0, 0), "456"),
            (Span::new(0, 0), "789"),
        ]), 
        Message::new("123".to_owned(), vec![
            (Span::new(0, 0), "456".to_owned()),
            (Span::new(0, 0), "789".to_owned()),
        ])
    );
}

#[cfg(test)] #[test]
fn message_by_macro() {

    let mut messages = MessageCollection::new();
    assert_eq!(messages, make_messages![]);

    messages.push(Message::new_by_str("a", vec![(Span::new(1, 1), "b")]));
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(Span::new(1, 1), "b")])]);
    assert_eq!(messages, make_messages![Message::new_by_str("a", vec![(Span::new(1, 1), "b")]), ]);

    messages.push(Message::new_by_str("c", vec![(Span::new(2, 3), "d")]));
    messages.push(Message::new_by_str("e", vec![(Span::new(2, 8), "f")]));
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(Span::new(1, 1), "b")]), 
        Message::new_by_str("c", vec![(Span::new(2, 3), "d")]),
        Message::new_by_str("e", vec![(Span::new(2, 8), "f")])
    ]);
    assert_eq!(messages, make_messages![
        Message::new_by_str("a", vec![(Span::new(1, 1), "b")]), 
        Message::new_by_str("c", vec![(Span::new(2, 3), "d")]),
        Message::new_by_str("e", vec![(Span::new(2, 8), "f")]),
    ]);
}