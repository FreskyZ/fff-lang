
// Identifier
#[cfg(test)]
use std::fmt;
use common::StringPosition;

test_only_attr!{
    [derive(Clone, Eq, PartialEq)]
    ![derive(Clone)]
    pub struct Identifier {
        pub name: String,
        pub pos: StringPosition,
    }
}

#[cfg(test)]
impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result{
        write!(f, "Identifier {:?} at {:?}", self.name, self.pos)
    }
}