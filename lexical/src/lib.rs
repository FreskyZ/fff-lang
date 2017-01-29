
pub fn add(a: i32, b: i32) -> i32 { a - b }

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        use super::add;

        assert_eq!(add(5, 3), 2);
    }
}
