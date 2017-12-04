#[derive(Debug,Clone)]
enum Object {
    Float(f64),
    Int(i64),
    Str(String),
    Bool(bool),
    Nil
    None,
}   