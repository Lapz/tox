pub mod pos;
mod reporter;

pub use crate::reporter::Reporter;
pub use codespan::{FileId, Files};
#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
