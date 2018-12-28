use crate::object::{RawObject, StringObject};
use crate::value::Value;
use rand::{thread_rng, Rng};
use std::time::{SystemTime, UNIX_EPOCH};
/// Calculate the number of seconds since the UNIX_EPOCH
pub fn clock(_: *const Value) -> Value {
    let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();

    Value::float(time.as_secs() as f64 + f64::from(time.subsec_nanos()) * 1e-9)
}
/// Return a random number between the min and max range
/// Panics if min is larger than the max
pub fn random(args: *const Value) -> Value {
    let min = unsafe { (*args.add(0)).as_int() };

    let max = unsafe { (*args.add(1)).as_int() };
    let mut rng = thread_rng();
    Value::int(rng.gen_range(min, max))
}

/// Reads input from stdin until the user presses enter
pub fn read(_: *const Value) -> Value {
    let mut input = String::new();
    use std::io;
    io::stdin()
        .read_line(&mut input)
        .expect("Unable to read input from stdin");

    Value::object(StringObject::from_owned(
        input,
        ::std::ptr::null::<RawObject>() as RawObject,
    ))
}

/// Open a file and returns the contents
pub fn fopen(args: *const Value) -> Value {
    let path = unsafe { (*args.add(0)).as_string().value().trim_end_matches('\0') };

    let mut input = String::new();

    use std::io::Read;

    println!("{:?}", path);

    ::std::fs::File::open(path)
        .unwrap()
        .read_to_string(&mut input)
        .unwrap();

    Value::object(StringObject::from_owned(
        input,
        ::std::ptr::null::<RawObject>() as RawObject,
    ))
}
