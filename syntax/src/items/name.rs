///! fff-lang
///!
///! syntax/name, currently is
///! Name = fIdent [ fNamespaceSep fIdent ]*
// future may support something like `to_string::<i32>(a)`

use std::fmt;
use codepos::StringPosition;

use lexical::Token;
use lexical::SeperatorKind;

use super::super::ISyntaxItemFormat;
use super::super::ISyntaxItemGrammar;
use super::super::ISyntaxItemParse;
use super::super::ParseSession;
use super::super::ParseResult;

#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct NameSegment{
    pub ident: String,
    pub ident_strpos: StringPosition,
}
impl NameSegment {
    fn new(ident: String, ident_strpos: StringPosition) -> NameSegment { NameSegment{ ident, ident_strpos } }
}
#[cfg_attr(test, derive(Eq, PartialEq))]
pub struct Name {
    pub segments: Vec<NameSegment>,
    pub all_strpos: StringPosition,   
}
impl ISyntaxItemFormat for Name {
    fn format(&self, indent: u32) -> String {
        format!("{}Name <{:?}>{}", 
            Name::indent_str(indent), self.all_strpos,
            self.segments.iter().fold(String::new(), |mut buf, segment| { 
                buf.push_str(&format!("\n{}Ident '{}' <{:?}>", Name::indent_str(indent + 1), segment.ident, segment.ident_strpos));
                buf 
            }),
        )
    }
}
impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.format(0)) }
}
impl Name {
    fn new(all_strpos: StringPosition, segments: Vec<NameSegment>) -> Name { Name{ all_strpos, segments } }
}
impl ISyntaxItemGrammar for Name {
    fn is_first_final(sess: &ParseSession) -> bool { if let &Token::Ident(_) = sess.tk { true } else { false } }
}
impl ISyntaxItemParse for Name {

    fn parse(sess: &mut ParseSession) -> ParseResult<Name> {
        
        let mut segments = Vec::new();
        let starting_strpos = sess.pos;

        let (first_ident, first_ident_strpos) = sess.expect_ident()?;
        segments.push(NameSegment::new(first_ident, first_ident_strpos));
        let mut ending_strpos = first_ident_strpos;

        loop {
            if let &Token::Sep(SeperatorKind::NamespaceSeperator) = sess.tk {
                sess.move_next();
                let (ident, ident_strpos) = sess.expect_ident()?;
                segments.push(NameSegment::new(ident, ident_strpos));
                ending_strpos = ident_strpos;
            } else {
                break;
            }
        }
        return Ok(Name::new(StringPosition::merge(starting_strpos, ending_strpos), segments));
    }
}

#[cfg(test)] #[test]
fn name_parse() {
    use super::super::ISyntaxItemWithStr;

    assert_eq!{ Name::with_test_str("hello"), 
        Name::new(make_strpos!(1, 1, 1, 5), vec![
            NameSegment::new("hello".to_owned(), make_strpos!(1, 1, 1, 5))
        ]) 
    }
    //                               0        1         2         3         4
    //                               12345678901234567890123456789012345678901
    assert_eq!{ Name::with_test_str("std::network::wlan::native::GetWLANHandle"),
        Name::new(make_strpos!(1, 1, 1, 41), vec![
            NameSegment::new("std".to_owned(), make_strpos!(1, 1, 1, 3)), 
            NameSegment::new("network".to_owned(), make_strpos!(1, 6, 1, 12)),
            NameSegment::new("wlan".to_owned(), make_strpos!(1, 15, 1, 18)),
            NameSegment::new("native".to_owned(), make_strpos!(1, 21, 1, 26)),
            NameSegment::new("GetWLANHandle".to_owned(), make_strpos!(1, 29, 1, 41)),
        ])
    }
}