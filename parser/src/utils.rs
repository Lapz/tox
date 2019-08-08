use crate::AstNode;
use std::fs::File;
use std::io::Read;
use std::path::Path;
pub fn dump_debug<T: AstNode>(item: &T) -> String {
    format!("{:#?}", item.syntax())
}

pub fn test_data(path: &str) -> String {
    let path = Path::new(path);
    let mut buf = String::new();
    let contents = File::open(path).unwrap().read_to_string(&mut buf);

    buf
}
