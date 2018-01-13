use llvm::prelude::*;
use llvm::core::*;
use llvm::{LLVMBuilder, LLVMLinkage, LLVMRealPredicate, LLVMValue};
use llvm::bit_writer::LLVMWriteBitcodeToFile;
use llvm;
use ast::expr::*;
use ast::statement::Statement;
use pos::WithPos;
use libc;
use std::ptr;

const LLVM_FALSE: LLVMBool = 0;
const LLVM_TRUE: LLVMBool = 1;

pub unsafe fn compile(statements: &[WithPos<Statement>]) {
    let context = LLVMContextCreate();
    let module = LLVMModuleCreateWithName(b"tox\0".as_ptr() as *const _);
    let builder = LLVMCreateBuilderInContext(context);

    let void = llvm::core::LLVMVoidTypeInContext(context);
    let function_type = llvm::core::LLVMFunctionType(void, ptr::null_mut(), 0, 0);
    let function =
        llvm::core::LLVMAddFunction(module, b"main\0".as_ptr() as *const _, function_type);
    let bb = llvm::core::LLVMAppendBasicBlockInContext(
        context,
        function,
        b"entry\0".as_ptr() as *const _,
    );

    for statement in statements {
        compile_statment(statement, module, builder);
    }

    llvm::core::LLVMPositionBuilderAtEnd(builder, bb);

    LLVMBuildRet(builder, int64(1));

    LLVMDumpModule(module);

    LLVMPrintModuleToFile(
        module,
        "temp.txt".as_ptr() as *const i8,
        "Couldn't write to file".as_ptr() as *mut *mut i8,
    );

    LLVMDisposeBuilder(builder);

    LLVMWriteBitcodeToFile(module, "tox.txt".as_ptr() as *const i8);
    LLVMContextDispose(context);
}

fn int() -> LLVMTypeRef {
    unsafe { LLVMInt64Type() }
}

fn float() -> LLVMTypeRef {
    unsafe { LLVMDoubleType() }
}

fn int1() -> LLVMTypeRef {
    unsafe { LLVMInt1Type() }
}

fn int8() -> LLVMTypeRef {
    unsafe { LLVMInt8Type() }
}

fn void() -> LLVMTypeRef {
    unsafe { LLVMVoidType() }
}

fn int64(val: libc::c_ulonglong) -> LLVMValueRef {
    unsafe { LLVMConstInt(LLVMInt64Type(), val, LLVM_FALSE) }
}

pub unsafe fn compile_statment(
    expr: &WithPos<Statement>,
    module: LLVMModuleRef,
    builder: *mut LLVMBuilder,
) -> LLVMValueRef {
    match expr.node {
        Statement::ExpressionStmt(ref expr) => compile_expr(expr, module, builder),
        Statement::Function { ref name, ref body } => {
            match body.node {
                Expression::Func {
                    ref parameters,
                    ref body,
                    ..
                } => {
                    let dobule_ty = LLVMDoubleType();
                }

                _ => unimplemented!(),
            }

            unimplemented!()
        }
        _ => unimplemented!(),
    }
}

unsafe fn compile_expr(
    expr: &WithPos<Expression>,
    module: LLVMModuleRef,
    builder: *mut LLVMBuilder,
) -> LLVMValueRef {
    match expr.node {
        Expression::Binary {
            ref left_expr,
            ref operator,
            ref right_expr,
        } => match *operator {
            Operator::Plus => LLVMBuildFAdd(
                builder,
                compile_expr(left_expr, module, builder),
                compile_expr(right_expr, module, builder),
                "addtmp".as_ptr() as *const i8,
            ),
            Operator::Minus => LLVMBuildFSub(
                builder,
                compile_expr(left_expr, module, builder),
                compile_expr(right_expr, module, builder),
                "subtmp".as_ptr() as *const i8,
            ),
            Operator::Star => LLVMBuildFMul(
                builder,
                compile_expr(left_expr, module, builder),
                compile_expr(right_expr, module, builder),
                "multmp".as_ptr() as *const i8,
            ),
            Operator::LessThan => LLVMBuildFCmp(
                builder,
                LLVMRealPredicate::LLVMRealOLT,
                compile_expr(left_expr, module, builder),
                compile_expr(right_expr, module, builder),
                "cmptmp".as_ptr() as *const i8,
            ),

            _ => unimplemented!(),
        },

        Expression::Call {
            ref callee,
            ref arguments,
        } => {
            let calle_func = compile_expr(callee, module, builder);

            let mut args = vec![];

            arguments
                .iter()
                .for_each(|a| args.push(compile_expr(a, module, builder)));

            LLVMBuildCall(
                builder,
                calle_func,
                args.as_ptr() as *mut *mut LLVMValue,
                args.len() as u32,
                "calltmp".as_ptr() as *const i8,
            )
        }
        Expression::Literal(ref value) => match *value {
            Literal::Int(i) => LLVMConstInt(int(), i as libc::c_ulonglong, 1),
            Literal::Float(f) => LLVMConstReal(float(), f as libc::c_double),

            Literal::True(b) | Literal::False(b) => LLVMConstInt(int1(), b as libc::c_ulonglong, 1),

            Literal::Str(ref s) => {
                let value = LLVMAddGlobal(
                    module,
                    LLVMArrayType(int8(), s.len() as libc::c_uint),
                    "string".as_ptr() as *const i8,
                );

                LLVMSetLinkage(value, LLVMLinkage::LLVMInternalLinkage);
                LLVMSetGlobalConstant(value, LLVM_TRUE);

                LLVMSetInitializer(
                    value,
                    LLVMConstString(
                        s.as_ptr() as *const libc::c_char,
                        s.len() as libc::c_uint,
                        LLVM_TRUE,
                    ),
                );

                value
            }

            Literal::Nil => LLVMConstNull(void()),
        },

        Expression::Var(ref sym, _) => LLVMGetNamedGlobal(module, sym.0 as *const libc::c_char),

        _ => unimplemented!(),
    }
}

// fn new_string_ptr(s:&str) -> *const i8 {

// }
