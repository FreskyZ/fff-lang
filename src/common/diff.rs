use std::fmt;
use std::str::from_utf8;

pub struct DiffDisplay<'a, 'b>(&'a str, &'b str);

impl<'a, 'b> fmt::Display for DiffDisplay<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut actual_lines = self.0.lines().collect::<Vec<_>>();
        let mut expect_lines = self.1.lines().collect::<Vec<_>>();

        // if both side is single line with len > 100, then it is comparing debug fmt,
        // hard split (regardless of word boundary) them to fixed length lines
        // (in bytes, because ast debug print does not contain non ascii char, because identifiers and string literals are isid)
        const HL: usize = 72;
        if actual_lines.len() == 1 && actual_lines[0].len() > HL && expect_lines.len() == 1 && expect_lines[0].len() > HL {
            actual_lines = self.0.as_bytes().chunks(HL).map(|c| from_utf8(c).unwrap()).collect::<Vec<_>>();
            expect_lines = self.1.as_bytes().chunks(HL).map(|c| from_utf8(c).unwrap()).collect::<Vec<_>>();
        }

        let common_line_count = std::cmp::min(actual_lines.len(), expect_lines.len());
        for line in 0..common_line_count {
            if actual_lines[line] != expect_lines[line] {
                writeln!(f, "{: >3} |A {}", line + 1, actual_lines[line])?;
                writeln!(f, "    |E {}", expect_lines[line])?;
            } else {
                writeln!(f, "{: >3} |  {}", line + 1, actual_lines[line])?;
            }
        }
        if actual_lines.len() > common_line_count {
            for line in common_line_count..actual_lines.len() {
                writeln!(f, "{: >3} |A {}", line + 1, actual_lines[line])?;
            }
        }
        if expect_lines.len() > common_line_count {
            for line in common_line_count..expect_lines.len() {
                writeln!(f, "{: >3} |E {}", line + 1, expect_lines[line])?;
            }
        }
        Ok(())
    }
}

pub fn diff<'a, 'b>(expect: &'a str, actual: &'b str) -> DiffDisplay<'a, 'b> {
    DiffDisplay(expect, actual)
}
