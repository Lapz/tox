use ast as t;
use ctx::CompileCtx;

use infer::types::{Type, TypeVar};
use infer::{Infer, InferResult};
use syntax::ast::Function;
use util::pos::{Span, Spanned};
