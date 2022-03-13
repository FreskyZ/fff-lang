///! diagnostics: diagnostics collecting and formatting

use std::fmt;
use crate::source::{SourceContext, FileSystem, Span};

pub mod strings;

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Diagnostic {
    name: String,
    details: Vec<(Span, String)>,
    helps: Vec<String>,
}

impl Diagnostic {

    pub fn span(&mut self, span: impl Into<Span>) -> &mut Self {
        self.details.push((span.into(), String::new()));
        self
    }
    pub fn detail(&mut self, span: impl Into<Span>, description: impl Into<String>) -> &mut Self {
        self.details.push((span.into(), description.into()));
        self
    }
    pub fn help(&mut self, help: impl Into<String>) -> &mut Self {
        self.helps.push(help.into());
        self
    }
}

#[derive(Debug)]
#[cfg_attr(test, derive(PartialEq))]
pub struct Diagnostics {
    items: Vec<Diagnostic>,
}

impl Diagnostics {

    pub fn new() -> Self {
        Self{ items: Vec::new() }
    }

    pub fn emit(&mut self, name: impl Into<String>) -> &mut Diagnostic {
        self.items.push(Diagnostic{ name: name.into(), details: Vec::new(), helps: Vec::new() });
        self.items.last_mut().unwrap()
    }
}

pub struct DiagnosticsDisplay<'a, 'b, F>(&'a Diagnostics, &'b SourceContext<F>);

impl Diagnostics {

    pub fn display<'a, 'b, F>(&'a self, ctx: &'b SourceContext<F>) -> DiagnosticsDisplay<'a, 'b, F> {
        DiagnosticsDisplay(self, ctx)
    }
}

static SPACES: [&str; 10] = ["", " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ", "         "];

impl<'a, 'b, F> fmt::Display for DiagnosticsDisplay<'a, 'b, F> where F: FileSystem {

    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use std::fmt::Write;

        for item in &self.0.items {
            write!(f, "error: {}\n", item.name)?;

            let mut last_start_line_length = 1;
            for (span, detail) in &item.details {
                let (file, start_line, start_column, _end_line, end_column) = self.1.map_span_to_line_column(*span);
                let path = self.1.get_relative_path(file);
                let start_line_length = format!("{}", start_line).len(); // length of the start line numeric value, to indent
                let start_line_content = self.1.map_line_to_content(file, start_line);
                last_start_line_length = start_line_length;

                f.write_str(SPACES[start_line_length])?;
                f.write_str("--> ")?;
                write!(f, "{}:{}:{}\n", path.display(), start_line, start_column)?;
                f.write_str(SPACES[start_line_length + 1])?;
                write!(f, "|\n{} | {}\n", start_line, start_line_content)?;
                f.write_str(SPACES[start_line_length + 1])?;
                f.write_char('|')?;
                for _ in 0..start_column {
                    f.write_char(' ')?;
                }
                for _ in start_column..end_column + 1 {
                    f.write_char('^')?;
                }
                write!(f, " {}\n", detail)?;
            }
            for help in &item.helps {
                f.write_str(SPACES[last_start_line_length + 1])?;
                write!(f, "= help: {}\n", help)?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

#[cfg(test)]
macro_rules! make_errors {
    () => { 
        crate::diagnostics::Diagnostics::new() 
    };
    ($e:ident: $($init:expr),+$(,)?) => {{
        let mut $e = crate::diagnostics::Diagnostics::new();
        $(
            $init;
        )*
        $e
    }}
}
#[cfg(test)]
pub(crate) use make_errors;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_usage() {

        let mut diagnostics = Diagnostics::new();
        diagnostics.emit("error name 1");
        diagnostics.emit("Error name 2").detail(Span::new(1, 2), "description 1").detail(Span::new(2, 3), "description 2").help("help 1");

        assert_eq!{ diagnostics, Diagnostics{
            items: vec![
                Diagnostic{ name: "error name 1".to_owned(), details: Vec::new(), helps: Vec::new() },
                Diagnostic{ name: "Error name 2".to_owned(), details: vec![(Span::new(1, 2), "description 1".to_owned()), (Span::new(2, 3), "description 2".to_owned())], helps: vec!["help 1".to_owned()] }
            ]
        }}

        assert_eq!{ make_errors!{
            e: e.emit("error name 1"),
            e.emit("Error name 2").detail(Span::new(1, 2), "description 1").detail(Span::new(2, 3), "description 2").help("help 1"),
        }, Diagnostics{
            items: vec![
                Diagnostic{ name: "error name 1".to_owned(), details: Vec::new(), helps: Vec::new() },
                Diagnostic{ name: "Error name 2".to_owned(), details: vec![(Span::new(1, 2), "description 1".to_owned()), (Span::new(2, 3), "description 2".to_owned())], helps: vec!["help 1".to_owned()] }
            ]
        }}
    }

    #[test]
    fn display() {
        //                                         0123456789 01234567890
        let mut scx = crate::source::make_source!("var a = '\\u12345678';" as "relative/path/name");
        let mut ecx = Diagnostics::new();
        scx.entry("relative/path/name").finish();
        ecx.emit("invalid unicode escape")
            .detail(Span::new(8, 8), "char literal start here")
            .detail(Span::new(9, 18), "unicode escape here")
            .help("0x12345678 is not valid unicode code point");
        assert_eq!{ ecx.display(&scx).to_string(),
        "error: invalid unicode escape
 --> ../../../../../relative/path/name:1:9
  |
1 | var a = '\\u12345678';
  |         ^ char literal start here
 --> ../../../../../relative/path/name:1:10
  |
1 | var a = '\\u12345678';
  |          ^^^^^^^^^^ unicode escape here
  = help: 0x12345678 is not valid unicode code point

"
        }
    }
}
