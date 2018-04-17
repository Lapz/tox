extern crate syntax;
#[macro_use]
extern crate util;

mod temp;
mod ir;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
