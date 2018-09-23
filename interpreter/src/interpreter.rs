use super::object::Object;
use fnv::FnvHashMap;
use interpreter::env::Environment;
use std::cell::RefCell;
use std::mem;
use std::rc::Rc;
use syntax::ast::expr::*;
use syntax::ast::statement::Statement;
use util::pos::Spanned;
use util::symbol::Symbol;
use util::symbol::Symbols;

#[derive(Debug)]
pub enum RuntimeError {
    Break,
    Continue,
    IndexOutOfBound,
    InvalidIndexType,
    NotAnIn,
    UndefinedProperty,
    CantParseAsInt(Vec<u8>),
    UndefinedSymbol(Symbol),
    Return(Box<Object>),
}

impl RuntimeError {
    pub fn fmt(&self, symbols: &Symbols<()>) {
        match *self {
            RuntimeError::UndefinedSymbol(ref symbol) => {
                println!("Undefined variable '{}' ", symbols.name(*symbol));
            }
            _ => (),
        }
    }
}

pub(crate) fn evaluate_statement(
    statement: &Spanned<Statement>,
    locals: &FnvHashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match statement.value {
        Statement::Block(ref statements) => {
            let mut environment = Environment::new_with_outer(env);

            for statement in statements {
                evaluate_statement(statement, locals, &mut environment)?;
            }

            Ok(Object::None)
        }

        Statement::Break => Err(RuntimeError::Break),
        Statement::Continue => Err(RuntimeError::Continue),
        Statement::Class {
            ref name,
            ref body,
            ref superclass,
        } => {
            env.define(name.value, Object::Nil);

            let mut class_methods: FnvHashMap<Symbol, Object> = FnvHashMap::default();

            let mut sklass = None;

            let mut s = false;

            let old_env = env.clone();

            if let Some(ref sclass) = *superclass {
                let new_env = Environment::new_with_outer(env);
                mem::replace(env, new_env);

                s = true;

                let superclass = env.get(&sclass.value, 1)?;

                sklass = Some(Box::new(env.get(&sclass.value, 1).unwrap().clone()));
                env.define(Symbol(1), superclass);
            }

            for method in &body.value.0 {
                match method.value {
                    Statement::Function {
                        ref name,
                        ref body,
                        ref params,
                        ..
                    } => {
                        let mut func_params = Vec::with_capacity(params.value.len());

                        for param in &params.value {
                            func_params.push(param.value.name.value);
                        }

                        class_methods.insert(
                            name.value,
                            Object::Function(
                                name.value,
                                func_params,
                                Box::new(*body.clone()),
                                Box::new(env.clone()),
                            ),
                        );
                    }

                    _ => unreachable!(),
                }
            }

            if s {
                mem::replace(env, old_env);
            }

            env.define(name.value, Object::Class(name.value, sklass, class_methods));
            Ok(Object::None)
        }

        Statement::While { ref body, ref cond } => {
            while evaluate_expression(cond, locals, env)?.is_truthy() {
                match evaluate_statement(body, locals, env) {
                    Ok(_) => (),
                    Err(e) => match e {
                        RuntimeError::Break => break,
                        RuntimeError::Continue => continue,
                        _ => return Err(e),
                    },
                };
            }

            Ok(Object::None)
        }

        Statement::Expr(ref expr) => evaluate_expression(expr, locals, env),

        Statement::For {
            ref init,
            ref cond,
            ref incr,
            ref body,
        } => {
            if init.is_none() && cond.is_none() && incr.is_none() {
                loop {
                    match evaluate_statement(body, locals, env) {
                        Ok(value) => value,
                        Err(e) => match e {
                            RuntimeError::Break => break,
                            RuntimeError::Continue => continue,
                            _ => return Err(e),
                        },
                    };
                }
                return Ok(Object::None);
            }

            if let Some(ref init) = *init {
                evaluate_statement(init, locals, env)?;
            }

            if let Some(ref cond) = *cond {
                while evaluate_expression(cond, locals, env)?.is_truthy() {
                    match evaluate_statement(body, locals, env) {
                        Ok(value) => value,
                        Err(e) => match e {
                            RuntimeError::Break => break,
                            RuntimeError::Continue => continue,
                            _ => return Err(e),
                        },
                    };

                    if let Some(ref inc) = *incr {
                        evaluate_expression(inc, locals, env)?;
                    }
                }
            }

            Ok(Object::None)
        }

        Statement::Function {
            ref name,
            ref body,
            ref params,
            ..
        } => {
            let mut func_params = Vec::with_capacity(params.value.len());

            for param in &params.value {
                func_params.push(param.value.name.value);
            }

            env.define(
                name.value,
                Object::Function(
                    name.value,
                    func_params,
                    Box::new(*body.clone()),
                    Box::new(env.clone()),
                ),
            );

            Ok(Object::None)
        }

        Statement::Print(ref expr) => {
            use std::io;
            use std::io::prelude::*;

            let value = evaluate_expression(expr, locals, env)?;
            println!("{}", value.as_string());
            let _ = io::stdout().flush();

            Ok(Object::None)
        }

        Statement::Return(ref expr) => Err(RuntimeError::Return(Box::new(evaluate_expression(
            expr, locals, env,
        )?))),

        Statement::If {
            ref cond,
            ref otherwise,
            ref then,
        } => {
            if evaluate_expression(cond, locals, env)?.is_truthy() {
                evaluate_statement(then, locals, env)
            } else if let Some(ref else_statement) = *otherwise {
                evaluate_statement(else_statement, locals, env)
            } else {
                Ok(Object::None)
            }
        }

        Statement::TypeAlias { .. } => Ok(Object::None),

        Statement::Var {
            ref ident,
            ref expr,
            ..
        } => {
            if let Some(ref expr) = *expr {
                let value = evaluate_expression(expr, locals, env)?;
                env.define(ident.value, value);
                return Ok(Object::None);
            }

            env.define(ident.value, Object::Nil);

            Ok(Object::None)
        }
    }
}

fn evaluate_expression(
    expression: &Spanned<Expression>,
    locals: &FnvHashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match expression.value {
        Expression::Array { ref items, .. } => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate_expression(item, locals, env)?)
            }
            Ok(Object::Array(values))
        }

        Expression::Assign {
            ref name,
            ref kind,
            ref value,
            ref handle,
        } => {
            let mut value = evaluate_expression(value, locals, env)?;

            let distance = get_distance(locals, &name.value, handle)?;
            match kind.value {
                AssignOperator::Equal => (),

                AssignOperator::PlusEqual => {
                    let current = env.get(&name.value, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x + y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x + y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::MinusEqual => {
                    let current = env.get(&name.value, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x - y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x - y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::SlashEqual => {
                    let current = env.get(&name.value, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x / y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x / y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::StarEqual => {
                    let current = env.get(&name.value, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x * y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x * y),
                        _ => unreachable!(),
                    }
                }
            }

            env.assign(&name.value, value.clone(), distance)?;

            Ok(value)
        }

        Expression::Binary {
            ref lhs,
            ref op,
            ref rhs,
        } => {
            let left = evaluate_expression(lhs, locals, env)?;
            let right = evaluate_expression(rhs, locals, env)?;

            match op.value {
                Op::BangEqual => Ok(Object::Bool(left != right)),
                Op::EqualEqual => Ok(Object::Bool(left == right)),
                Op::LessThan => Ok(Object::Bool(left < right)),
                Op::LessThanEqual => Ok(Object::Bool(left <= right)),
                Op::GreaterThan => Ok(Object::Bool(left > right)),
                Op::GreaterThanEqual => Ok(Object::Bool(left >= right)),
                Op::Plus => add(left, right),
                Op::Minus => minus(left, right),
                Op::Star => times(left, right),
                Op::Slash => divide(left, right),
                Op::Modulo => modulo(left, right),
                Op::Exponential => expon(left, right),
                Op::And => {
                    if !left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right)
                    }
                }
                Op::Or => {
                    if left.is_truthy() {
                        Ok(left)
                    } else {
                        Ok(right)
                    }
                }
            }
        }

        Expression::Call {
            ref callee,
            ref args,
        } => {
            let callee = evaluate_expression(callee, locals, env)?;

            let mut obj_arguments = Vec::with_capacity(args.len());

            for arg in args {
                obj_arguments.push(evaluate_expression(arg, locals, env)?);
            }

            callee.call(&obj_arguments, locals)
        }
        Expression::ClassInstance {
            ref symbol,
            ref props,
        } => match env.get(&symbol.value, 0)? {
            Object::Class(_, ref superclass, ref methods) => {
                let mut instance_props: FnvHashMap<Symbol, Object> = FnvHashMap::default();
                let mut s_class_methods = None;

                if let Some(ref sklass) = *superclass {
                    match **sklass {
                        Object::Class(_, _, ref methods_) => {
                            s_class_methods = Some(methods_.clone());
                        }
                        _ => unimplemented!(),
                    }
                }

                for prop in props.iter() {
                    let value = evaluate_expression(&prop.value.expr, locals, env)?;
                    instance_props.insert(prop.value.symbol.value, value);
                }

                env.define(
                    symbol.value,
                    Object::Instance {
                        methods: methods.clone(),
                        fields: Rc::new(RefCell::new(instance_props.clone())),
                        sclassmethods: s_class_methods.clone(),
                    },
                );

                Ok(Object::Instance {
                    methods: methods.clone(),
                    fields: Rc::new(RefCell::new(instance_props)),
                    sclassmethods: s_class_methods,
                })
            }

            _ => unimplemented!(),
        },

        Expression::Literal(ref lit) => evaluate_literal(lit),
        Expression::Get {
            ref object,
            ref property,
            ..
        } => {
            let object = evaluate_expression(object, locals, env)?;

            match object {
                instance @ Object::Instance { .. } => instance.get_property(&property.value, env),
                class @ Object::Class(_, _, _) => class.get_property(&property.value, env),
                _ => Err(RuntimeError::NotAnIn),
            }
        }
        Expression::Grouping { ref expr } => evaluate_expression(expr, locals, env),
        Expression::Index {
            ref target,
            ref index,
        } => {
            let target = evaluate_expression(target, locals, env)?;
            let index = evaluate_expression(index, locals, env)?;

            match target {
                Object::Array(r) => {
                    let index = match index {
                        Object::Int(i) => i,
                        _ => unreachable!(),
                    };

                    if index > (r.len() as i64) || index < 0 {
                        return Err(RuntimeError::IndexOutOfBound);
                    }

                    Ok(r[index as usize].to_owned())
                }
                _ => unimplemented!(),
            }
        }

        Expression::Set {
            ref object,
            ref name,
            ref value,
            ..
        } => {
            let object = evaluate_expression(object, locals, env)?;
            let value = evaluate_expression(value, locals, env)?;

            match object {
                mut instance @ Object::Instance { .. } => {
                    instance.set(name.value, &value);
                }
                _ => return Err(RuntimeError::NotAnIn),
            }

            Ok(value)
        }

        Expression::Var(ref symbol, ref handle) => {
            let distance = get_distance(locals, &symbol.value, handle)?;

            env.get(&symbol.value, distance)
        }

        Expression::Ternary {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let cond = evaluate_expression(condition, locals, env)?;

            if cond.is_truthy() {
                evaluate_expression(then_branch, locals, env)
            } else {
                evaluate_expression(else_branch, locals, env)
            }
        }

        Expression::This(ref handle) => {
            let distance = get_distance(locals, &Symbol(0), handle)?;
            env.get(&Symbol(0), distance)
        }

        Expression::Unary { ref op, ref expr } => {
            let right = evaluate_expression(expr, locals, env)?;

            match op.value {
                UnaryOp::Minus => match right {
                    Object::Float(f) => Ok(Object::Float(-f)),
                    Object::Int(i) => Ok(Object::Int(-i)),
                    _ => unreachable!(),
                },
                UnaryOp::Bang => Ok(!right),
            }
        }
    }
}

fn add(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
        (Object::Str(mut l), Object::Str(r)) => {
            l.extend(r.iter());

            Ok(Object::Str(l))
        }
        _ => unreachable!(),
    }
}

fn times(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l * r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l * r)),
        _ => unreachable!(),
    }
}

#[inline]
fn modulo(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l % r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l % r)),
        _ => unreachable!(),
    }
}

#[inline]
fn expon(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l.powf(r))),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l.pow(r as u32))),
        _ => unreachable!(),
    }
}
#[inline]
fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => unreachable!(),
    }
}
#[inline]
fn divide(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l / r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l / r)),
        _ => unreachable!(),
    }
}

fn evaluate_literal(expression: &Literal) -> Result<Object, RuntimeError> {
    match *expression {
        Literal::Float(i) => Ok(Object::Float(i)),
        Literal::Int(i) => Ok(Object::Int(i)),
        Literal::Str(ref s) => Ok(Object::Str(s.clone())),
        Literal::Nil => Ok(Object::Nil),
        Literal::True(ref b) | Literal::False(ref b) => Ok(Object::Bool(*b)),
    }
}

fn get_distance(
    locals: &FnvHashMap<VariableUseHandle, usize>,
    variable: &Symbol,
    handle: &VariableUseHandle,
) -> Result<usize, RuntimeError> {
    match locals.get(handle) {
        Some(distance) => Ok(*distance),
        None => Err(RuntimeError::UndefinedSymbol(*variable)),
    }
}

pub mod env {

    use util::symbol::Symbol;

    use super::RuntimeError;
    use builtins::BuiltIn;
    use object::Object;

    use std::rc::Rc;

    use fnv::FnvHashMap;
    use std::cell::RefCell;
    use util::symbol::Symbols;

    #[derive(Debug, Clone, Default)]
    /// A Loxlocals,enviroment
    /// which contains a map of a Symbol and value and the scope in which a
    /// Symbol was declared
    pub struct Environment {
        actual: Rc<RefCell<EnvironmentImpl>>,
    }

    impl PartialEq for Environment {
        fn eq(&self, other: &Environment) -> bool {
            self.actual.borrow().values == other.actual.borrow().values
        }
    }

    impl Environment {
        pub fn new() -> Self {
            Self::default()
        }

        pub fn new_with_outer(outer: &Environment) -> Environment {
            Environment {
                actual: Rc::new(RefCell::new(EnvironmentImpl::with_values(outer))),
            }
        }

        pub fn fill_env(&mut self, symbols: &mut Symbols<()>) {
            let built_in = BuiltIn::new().get_built_ins(symbols);

            for (variable, run_time_value) in built_in {
                self.define(variable, run_time_value);
            }
        }

        /// Defines a Symbol by inserting the name and
        /// value into thelocals,environmentImpl
        pub fn define(&self, name: Symbol, value: Object) {
            self.actual.borrow_mut().values.insert(name, value);
        }

        pub fn remove(&self, name: &Symbol) {
            self.actual.borrow_mut().values.remove(name);
        }

        /// Takes a Symbol and a value.
        /// Checks if the value exsits and inserts into the actual values if not,
        /// searches the outer scope and then calls itselfs.
        /// Returns a runtime error if the not found
        pub fn assign(
            &self,
            name: &Symbol,
            value: Object,
            distance: usize,
        ) -> Result<(), RuntimeError> {
            let mut actual = self.actual.borrow_mut();

            if distance == 0 {
                if actual.values.contains_key(name) {
                    *actual.values.get_mut(name).unwrap() = value;
                    Ok(())
                } else {
                    Err(RuntimeError::UndefinedSymbol(*name))
                }
            } else {
                match actual.outer {
                    Some(ref outer) => outer.assign(name, value, distance - 1),
                    None => Err(RuntimeError::UndefinedSymbol(*name)),
                }
            }
        }

        /// Takes a Symbol and a value.
        /// Checks if the value exists and if not,
        /// searches the outer scope and then calls itselfs.
        /// Returns a runtime error if the not found
        pub fn get(&self, name: &Symbol, distance: usize) -> Result<Object, RuntimeError> {
            let actual = self.actual.borrow();

            if distance == 0 {
                if let Some(value) = actual.values.get(name) {
                    return Ok(value.clone());
                } else {
                    Err(RuntimeError::UndefinedSymbol(*name))
                }
            } else {
                match actual.outer {
                    Some(ref outer) => outer.get(name, distance - 1),
                    None => Err(RuntimeError::UndefinedSymbol(*name)),
                }
            }
        }

        pub fn dump(&self) {
            let actual = self.actual.borrow();
            println!("-----------ENVIRONMENT-------");
            for (name, value) in &actual.values {
                println!("name:{}, value:{}", name, value)
            }

            if let Some(ref outer) = actual.outer {
                outer.dump()
            }
        }
    }

    /// The actual struct that contains the values.
    /// It is wrapped within a Rc<RefCell<_>>
    #[derive(Debug, Default)]
    pub(crate) struct EnvironmentImpl {
        outer: Option<Environment>,
        values: FnvHashMap<Symbol, Object>,
    }

    impl EnvironmentImpl {
        fn with_values(outer: &Environment) -> Self {
            EnvironmentImpl {
                outer: Some(outer.clone()),
                values: FnvHashMap::default(),
            }
        }
    }

}
