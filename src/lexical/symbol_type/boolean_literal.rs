
// Boolean literal
#[cfg(test)]
use std::fmt;
use common::StringPosition;

test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub struct BooleanLiteral {
        pub value: bool,
        pub pos: StringPosition,
    }
}

#[cfg(test)]
impl fmt::Debug for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Boolean literal {:?} at {:?}", self.value, self.pos)
    }
}