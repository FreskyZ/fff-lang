use std::ops;

pub fn sub<T>(a: T, b: T) -> <T as ops::Add>::Output
    where T: ops::Add {
    a + b
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use super::sub;

        assert_eq!(sub(3, 5), 8);
    }
}
