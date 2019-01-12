mod vm;

pub(crate) type CodegenResult<T> = Result<T, ()>;
pub use crate::vm::compile as compile_vm;
