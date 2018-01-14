extern crate llvm;
extern crate syntax;

use llvm::{Module,Context,Builder};


fn compile(ast:&[WithPos<Statement>]) {

    let ctx = Context::new();
    let module = Module::new("main",&ctx);

    

}