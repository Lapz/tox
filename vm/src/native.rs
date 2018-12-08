use crate::value::Value;
use rand::{thread_rng, Rng};
use std::time::{SystemTime, UNIX_EPOCH};
use object::{StringObject,RawObject};
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