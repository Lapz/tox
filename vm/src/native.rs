use std::time::{SystemTime, UNIX_EPOCH};
use crate::value::Value;

pub fn clock(_: u8, _: *const Value) -> Value {
    
        let time = SystemTime::now().duration_since(UNIX_EPOCH).unwrap();

        Value::float(time.as_secs() as f64 + f64::from(time.subsec_nanos()) * 1e-9)
    
}
