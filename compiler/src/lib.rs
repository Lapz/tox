#![feature(use_nested_groups)]
#[macro_use]
extern crate llvm;
extern crate llvm_sys;
extern crate syntax;
extern crate util;

use std::rc::Rc;
use std::collections::HashMap;
use llvm::{Arg, Builder, CBox, CSemiBox, Compile, Context, DoubleType, FunctionType, IntegerType,
           Module, Type, Value, VoidType};
use syntax::ast::statement::Statement;
use syntax::ast::expr::{Expression, Literal, Operator};
use util::{env::{Entry, TypeEnv}, pos::WithPos, types::{BaseType, Type as Ty}};
use llvm::{Function, Predicate, types::ArrayType};
use llvm_sys::core::LLVMConstVector;
use util::symbol::{Symbol, SymbolFactory, Table};
use llvm_sys::prelude::*;

pub fn compile<'a>(ast: &[WithPos<Statement>], strings: &Rc<SymbolFactory>, env: &TypeEnv) -> Result<(), CompilerError> {
    let ctx = Context::new();
    let module = Module::new("tox", &ctx);
    let func = module.add_function("main", Type::get::<fn(f64, f64) -> f64>(&ctx));
    // let entry = func.append("entry");
    let builder = Builder::new(&ctx);
    // builder.position_at_end(entry);
    let values = HashMap::new();
    // let a = &func[0];
    // let b = &func[1];
    let mut value = 1.compile(&ctx);

    for statement in ast {
        value = compile_statement(statement, &builder, &module, &ctx, &values, env)?;
    }

    builder.build_ret(value);

    module.write_bitcode("out.ll");

    module.verify().unwrap();

    println!("{:?}", module);

    Ok(())
}

pub enum CompilerError {}

fn compile_expr<'a, 'b>(
    expr: &WithPos<Expression>,
    builder: &'a CSemiBox<'a, Builder>,
    module: &'a CSemiBox<'a, Module>,
    context: &'a CBox<Context>,
    values: &'a HashMap<&Symbol, &'a Arg>,
    env: &TypeEnv,
) -> Result<&'a Value, CompilerError> {
    match expr.node {
        Expression::Literal(ref lit) => match *lit {
            Literal::True(b) | Literal::False(b) => Ok(b.compile(&context)),
            Literal::Str(ref s) => Ok(s.compile(&context)),
            Literal::Float(ref n) => Ok(n.compile(&context)),
            Literal::Int(ref n) => Ok(n.compile(&context)),
            Literal::Nil => Ok(().compile(&context)),
        },
        Expression::Binary {
            ref left_expr,
            ref operator,
            ref right_expr,
        } => {
            let left = compile_expr(left_expr, builder, module, context, values, env)?;
            let right = compile_expr(right_expr, builder, module, context, values, env)?;
            match *operator {
                Operator::Plus => Ok(builder.build_add(left, right)),
                Operator::Minus => Ok(builder.build_sub(left, right)),
                Operator::Star => Ok(builder.build_mul(left, right)),

                Operator::LessThan => {
                    let comp = builder.build_cmp(&left, &right, Predicate::LessThan);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }

                Operator::LessThanEqual => {
                    let comp = builder.build_cmp(&left, &right, Predicate::LessThanOrEqual);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }

                Operator::GreaterThan => {
                    let comp = builder.build_cmp(&left, &right, Predicate::GreaterThan);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }
                Operator::GreaterThanEqual => {
                    let comp = builder.build_cmp(&left, &right, Predicate::GreaterThanOrEqual);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }

                Operator::BangEqual => {
                    let comp = builder.build_cmp(&left, &right, Predicate::NotEqual);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }

                Operator::EqualEqual => {
                    let comp = builder.build_cmp(&left, &right, Predicate::Equal);
                    let res = builder.build_bit_cast(&comp, &Type::get::<f64>(&context));
                    Ok(res)
                }

                Operator::Modulo => Ok(builder.build_rem(left, right)),

                _ => unimplemented!(),
            }
        }

        Expression::Array { ref items } => {
            let mut v = Vec::with_capacity(items.len());

            for item in items {
                v.push(compile_expr(item, builder, module, context, values, env)?)
            }

            let ty = v[0].get_type();
            let array_ty = ArrayType::new(ty, v.len());

            Ok(builder.build_array_alloca(array_ty, v.len().compile(&context)))
        }

        Expression::Call {
            ref callee,
            ref arguments,
        } => match callee.node {
            Expression::Var(ref sym, _) => {
                let func = module.get_function(&env.name(*sym)).unwrap();

                let mut v = vec![];

                for arg in arguments {
                    v.push(compile_expr(arg, builder, module, context, values, env)?)
                }

                Ok(builder.build_call(func, &v))
            }
            _ => unimplemented!(),
        },

        Expression::Func{ref body,..} => compile_statement(body, builder, module, context, values, env),

        Expression::Var(ref sym, _) => Ok(values.get(sym).unwrap()),

        // Expression::
        ref e => unimplemented!("{:#?}", e),
    }
}

fn compile_statement<'a, 'b>(
    statement: &WithPos<Statement>,
    builder: &'a CSemiBox<'a, Builder>,
    module: &'a CSemiBox<'a, Module>,
    context: &'a CBox<Context>,
    values: &'a HashMap<&Symbol, &'a Arg>,
    env: &TypeEnv,
) -> Result<&'a Value, CompilerError> {
    match statement.node {
        Statement::ExpressionStmt(ref expr) => {
            compile_expr(expr, builder, module, context, values, env)
        }
        Statement::Block(ref statements) => {


            let mut ret = Ok(().compile(&context));

            for statement in statements {
                ret = compile_statement(statement, builder, module, context, values, env);
            }

            // block.get_terminator();
            ret
        }
        Statement::Return(ref ret) => {
            let val = if let Some(ref r) = *ret {
                compile_expr(r, builder, module, context, values, env)?
            } else {
                ().compile(&context)
            };

            builder.build_ret(val);

            Ok(val)
        }
        Statement::Function { ref name, ref body } => {
            match *env.look_var(*name).unwrap() {
                Entry::FunEntry {
                    ref params,
                    ref returns,
                } => {
                    let mut arg_tys: Vec<_> = params
                        .iter()
                        .map(|param| get_type(param, context))
                        .collect();

                    let sig = FunctionType::new(get_type(returns, context), &arg_tys);
                    let func = module.add_function(&env.name(*name), sig);

                    match body.node {
                        Expression::Func { ref parameters, .. } => {
                            for (i, param) in parameters.iter().enumerate() {
                                &func[i].set_name(&env.name(param.0));
                            }
                        }
                        _ => unreachable!(),
                    }

                    // builder.build_ret_void();
                }

                _ => unreachable!(),
            }

            let func = module.get_function(&env.name(*name)).unwrap();
            let block = func.append("body");
            builder.position_at_end(block);

            let mut values = HashMap::new();

            match body.node {
                Expression::Func { ref parameters, .. } => {
                    for (i, param) in parameters.iter().enumerate() {
                        values.insert(&param.0, &func[i]);
                    }
                },

                _ => unreachable!(),
            }

            let ret = compile_expr(body, builder, module, context, &values, env)?;
            builder.build_ret(ret);
            // block.get_terminator();

            module.verify().unwrap();
            Ok(func)
            // unimplemented!()
        }
        _ => unimplemented!(),
    }
}
fn get_type<'a>(ty: &Ty, context: &'a CBox<Context>) -> &'a Type {
    match *ty {
        Ty::Simple(ref sim) => match *sim {
            BaseType::Int => IntegerType::new(context, 64),
            BaseType::Float => DoubleType::new(context),
            BaseType::Str => <[u8; 0]>::get_type(context),
            BaseType::Nil => VoidType::new(context),
            BaseType::Bool => bool::get_type(context),
        },
        _ => unimplemented!(),
    }
}

fn get_sym(expr: &WithPos<Expression>, env: &TypeEnv) -> String {
    match expr.node {
        Expression::Var(sym, _) => env.name(sym),
        _ => unreachable!(),
    }
}
