use value::Value;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn clock(arg_count:u8,args:*mut [Value]) -> Value {
   unsafe {
       let time =SystemTime::now().duration_since(UNIX_EPOCH).unwrap();
       
       Value::float(time.as_secs() as f64 + f64::from(time.subsec_nanos()) * 1e-9)
   }
   
}