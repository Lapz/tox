use super::object::Object;
use syntax::ast::expr::*;
use syntax::ast::statement::Statement;
use util::pos::WithPos;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use util::symbol::Symbol;
use interpreter::env::Environment;

#[derive(Debug)]
pub enum RuntimeError {
    Break,
    Continue,
    IndexOutOfBound,
    InvalidIndexType,
    NotAnIn,
    UndefinedProperty,
    CantParseAsInt,
    UndefinedSymbol(Symbol),
    Return(Box<Object>),
}

pub(crate) fn evaluate_statement(
    statement: &WithPos<Statement>,
    locals: &HashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match statement.node {
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
            ref methods,
            ref superclass,
            ..
        } => {
            env.define(*name, Object::Nil);

            let mut sym_methods: HashMap<Symbol, Object> = HashMap::new();

            let mut sklass = None;
            let mut s = false;

            if let Some(sclass) = *superclass {
                // env = &Environment::new_with_outer(env);

                // let sk =env.look_object(sclass).unwrap().clone();

                //locals,env.define(Symbol(1), sk);
                // sklass = Some(Box::new(env.look_object(sclass).unwrap().clone()));
                s = true;
            }

            for method in methods {
                match method.node {
                    Statement::Function { ref name, ref body } => match body.node {
                        Expression::Func {
                            ref parameters,
                            ref body,
                            ..
                        } => {
                            let mut params: Vec<Symbol> =
                                parameters.iter().map(|params| params.0).collect();
                            sym_methods.insert(
                                *name,
                                Object::Function(*name, params, *body.clone(), env.clone()),
                            );
                        }
                        _ => unreachable!(),
                    },

                    _ => unimplemented!(),
                }
            }

            if s {}

            env.assign(name, Object::Class(*name, sklass, sym_methods), 0)?;
            Ok(Object::None)
        }

        Statement::DoStmt {
            ref condition,
            ref body,
        }
        | Statement::WhileStmt {
            ref body,
            ref condition,
        } => {
            while evaluate_expression(condition, locals, env)?.is_truthy() {
                match evaluate_statement(body, locals, env) {
                    Ok(value) => value,
                    Err(e) => match e {
                        RuntimeError::Break => break,
                        RuntimeError::Continue => continue,
                        _ => return Err(e),
                    },
                };
            }

            Ok(Object::None)
        }

        Statement::ExpressionStmt(ref expr) => evaluate_expression(expr, locals, env),

        Statement::ForStmt {
            ref initializer,
            ref condition,
            ref increment,
            ref body,
        } => {
            if initializer.is_none() && condition.is_none() && increment.is_none() {
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

            if let Some(ref init) = *initializer {
                evaluate_statement(init, locals, env)?;
            }

            if let Some(ref cond) = *condition {
                while evaluate_expression(cond, locals, env)?.is_truthy() {
                    match evaluate_statement(body, locals, env) {
                        Ok(value) => value,
                        Err(e) => match e {
                            RuntimeError::Break => break,
                            RuntimeError::Continue => continue,
                            _ => return Err(e),
                        },
                    };

                    if let Some(ref inc) = *increment {
                        evaluate_expression(inc, locals, env)?;
                    }
                }
            }

            Ok(Object::None)
        }

        Statement::Function { ref name, ref body } => match body.node {
            Expression::Func {
                ref parameters,
                ref body,
                ..
            } => {
                let mut params: Vec<Symbol> = parameters.iter().map(|params| params.0).collect();
                env.define(
                    *name,
                    Object::Function(*name, params, *body.clone(), env.clone()),
                );

                
                Ok(Object::None)
            }
            _ => unreachable!(),
        },

        Statement::IfStmt {
            ref condition,
            ref else_branch,
            ref then_branch,
        } => {
            if evaluate_expression(condition, locals, env)?.is_truthy() {
                evaluate_statement(then_branch, locals, env)
            } else if let Some(ref else_statement) = *else_branch {
                evaluate_statement(else_statement, locals, env)
            } else {
                Ok(Object::None)
            }
        }

        Statement::Print(ref expr) => {
            use std::io;
            use std::io::prelude::*;

            let value = evaluate_expression(expr, locals, env)?;

            println!("{}", value.as_string());
            let _ = io::stdout().flush();

            Ok(Object::None)
        }

        Statement::Return(ref r) => {
            if let Some(ref expr) = *r {
                return Err(RuntimeError::Return(Box::new(evaluate_expression(
                    expr,
                    locals,
                    env,
                )?)));
            }
         Err(RuntimeError::Return(Box::new(Object::Nil)))
        }

        Statement::TypeAlias { .. } => Ok(Object::None),

        Statement::Var(ref symbol, ref expression, ..) => {
            if let Some(ref expr) = *expression {
                let value = evaluate_expression(expr, locals, env)?;
                env.define(*symbol, value);
                return Ok(Object::None)
            }

            env.define(*symbol,Object::Nil);

            Ok(Object::None)
        }

        _ => unimplemented!("Extern linking of functions is not available"),
    }
}

fn evaluate_expression(
    expression: &WithPos<Expression>,
    locals: &HashMap<VariableUseHandle, usize>,
    env: &mut Environment,
) -> Result<Object, RuntimeError> {
    match expression.node {
        Expression::Array { ref items, .. } => {
            let mut values = Vec::with_capacity(items.len());
            for item in items {
                values.push(evaluate_expression(item, locals, env)?)
            }
            Ok(Object::Array(values))
        }

        Expression::This(ref handle) => {
            let distance = get_distance(locals, &Symbol(0), handle)?;
            env.get(&Symbol(0), distance)
        }

        Expression::Assign {
            ref name,
            ref kind,
            ref value,
            ref handle,
        } => {
            let mut value = evaluate_expression(value, locals, env)?;

            let distance = get_distance(locals, name, handle)?;
            match *kind {
                AssignOperator::Equal => (),

                AssignOperator::PlusEqual => {
                    let current = env.get(name, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x + y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x + y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::MinusEqual => {
                    let current = env.get(name, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x - y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x - y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::SlashEqual => {
                    let current = env.get(name, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x / y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x / y),
                        _ => unreachable!(),
                    }
                }

                AssignOperator::StarEqual => {
                    let current = env.get(name, distance)?;

                    match (current, value) {
                        (Object::Int(x), Object::Int(y)) => value = Object::Int(x * y),
                        (Object::Float(x), Object::Float(y)) => value = Object::Float(x * y),
                        _ => unreachable!(),
                    }
                }
            }

            env.assign(name, value.clone(), distance)?;

            Ok(value)
        }

        Expression::Binary {
            ref left_expr,
            ref operator,
            ref right_expr,
        } => {
            let left = evaluate_expression(left_expr, locals, env)?;
            let right = evaluate_expression(right_expr, locals, env)?;

            match *operator {
                Operator::BangEqual => Ok(Object::Bool(left != right)),
                Operator::EqualEqual => Ok(Object::Bool(left == right)),
                Operator::LessThan => Ok(Object::Bool(left < right)),
                Operator::LessThanEqual => Ok(Object::Bool(left <= right)),
                Operator::GreaterThan => Ok(Object::Bool(left > right)),
                Operator::GreaterThanEqual => Ok(Object::Bool(left >= right)),
                Operator::Plus => add(left, right),
                Operator::Minus => minus(left, right),
                Operator::Star => times(left, right),
                Operator::Slash => divide(left, right),
                Operator::Modulo => modulo(left, right),
                Operator::Exponential => expon(left, right),
            }
        }

        Expression::Call {
            ref callee,
            ref arguments,
        } => {
            let callee = evaluate_expression(callee, locals, env)?;

            let mut obj_arguments = Vec::with_capacity(arguments.len());

            for expr in arguments {
                obj_arguments.push(evaluate_expression(expr, locals, env)?);
            }

            callee.call(&obj_arguments, locals,)
        }

        Expression::ClassInstance {
            ref name,
            ref properties,
        } => match env.get(name, 0)? {
            Object::Class(_, ref superclass, ref methods) => {
                let mut props: HashMap<Symbol, Object> = HashMap::new();
                let mut s_class_methods = None;
                
                if let Some(ref sklass) = *superclass {
                    match **sklass {
                        Object::Class(_, _, ref methods_) => {
                            s_class_methods = Some(methods_.clone());
                        }
                        _ => unimplemented!(),
                    }
                }

                for &(ref name, ref expr) in properties {
                    let value = evaluate_expression(expr, locals, env)?;
                    props.insert(*name, value);
                }

                env.define(
                    *name,
                    Object::Instance {
                        methods: methods.clone(),
                        fields: Rc::new(RefCell::new(props.clone())),
                        sclassmethods: s_class_methods.clone(),
                    },
                );

                Ok(Object::Instance {
                    methods: methods.clone(),
                    fields: Rc::new(RefCell::new(props)),
                    sclassmethods: s_class_methods,
                })
            }

            _ => unreachable!(),
        },

        Expression::Dict { ref items } => {
            let mut dict: HashMap<Object, Object> = HashMap::new();

            for &(ref key, ref value) in items {
                let eval_key = evaluate_expression(key, locals, env)?;
                let eval_value = evaluate_expression(value, locals, env)?;

                dict.insert(eval_key, eval_value);
            }

            Ok(Object::Dict(dict))
        }

        Expression::Grouping { ref expr } => evaluate_expression(expr, locals, env),

        Expression::IndexExpr {
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
                Object::Dict(r) => {
                    let index = match index {
                        Object::Int(i) => Object::Int(i),
                        Object::Str(r) => Object::Str(r),
                        Object::Bool(b) => Object::Bool(b),
                        _ => return Err(RuntimeError::InvalidIndexType),
                    };

                    let nil = Object::Nil;

                    Ok(r.get(&index).unwrap_or(&nil).clone())
                }
                _ => unimplemented!(),
            }
        }
        Expression::Literal(ref lit) => evaluate_literal(lit),

        Expression::Logical {
            ref left,
            ref operator,
            ref right,
        } => {
            let left = evaluate_expression(left, locals, env)?;

            match *operator {
                LogicOperator::Or => if left.is_truthy() {
                    return Ok(left);
                },
                LogicOperator::And => if !left.is_truthy() {
                    return Ok(left);
                },
            }

            let right = evaluate_expression(right, locals, env)?;

            Ok(right)
        }

        Expression::Func {
            ref parameters,
            ref body,
            ..
        } => {
            let mut params: Vec<Symbol> = parameters.iter().map(|params| params.0).collect();
            Ok(Object::Function(
                env.unique_id(),
                params,
                *body.clone(),
                env.clone(),
            ))
        }
        Expression::Var(ref symbol, ref handle) => {
            let distance = get_distance(locals, symbol, handle)?;

            env.get(symbol, distance)
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
                    instance.set(*name, &value);
                }
                _ => return Err(RuntimeError::NotAnIn),
            }

            Ok(value)
        }
        Expression::Super(ref handle) => {
            let distance = get_distance(locals, &Symbol(1), handle)?;
            env.get(&Symbol(1), distance)
        }

        Expression::Get {
            ref object,
            ref property,
            ..
        } => {
            let object = evaluate_expression(object, locals, env)?;
           
            match object {
                instance @ Object::Instance { .. } => instance.get_property(property, env),
                class @ Object::Class(_, _, _) => class.get_property(property, env),
                _ => Err(RuntimeError::NotAnIn),
            }
        }
        Expression::Ternary {
            ref condition,
            ref then_branch,
            ref else_branch,
        } => {
            let condition = evaluate_expression(condition, locals, env)?;

            if condition.is_truthy() {
                evaluate_expression(then_branch, locals, env)
            } else {
                evaluate_expression(else_branch, locals, env)
            }
        }

        Expression::Unary {
            ref operator,
            ref expr,
        } => {
            let right = evaluate_expression(expr, locals, env)?;

            match *operator {
                UnaryOperator::Minus => match right {
                    Object::Float(f) => Ok(Object::Float(-f)),
                    Object::Int(i) => Ok(Object::Int(-i)),
                    _ => unreachable!(),
                },
                UnaryOperator::Bang => Ok(!right),
            }
        }
    }
}

fn add(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l + r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l + r)),
        (Object::Str(ref mut l), Object::Str(ref r)) => {
            l.push_str(r);

            Ok(Object::Str(l.to_owned()))
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

fn minus(lhs: Object, rhs: Object) -> Result<Object, RuntimeError> {
    match (lhs, rhs) {
        (Object::Float(l), Object::Float(r)) => Ok(Object::Float(l - r)),
        (Object::Int(l), Object::Int(r)) => Ok(Object::Int(l - r)),
        _ => unreachable!(),
    }
}

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
        Literal::Str(ref s) => Ok(Object::Str(s.to_owned())),
        Literal::Nil => Ok(Object::Nil),
        Literal::True(ref b) | Literal::False(ref b) => Ok(Object::Bool(*b)),
    }
}

fn get_distance(
    locals: &HashMap<VariableUseHandle, usize>,
    variable: &Symbol,
    handle: &VariableUseHandle,
) -> Result<usize, RuntimeError> {
    match locals.get(handle) {
        Some(distance) => Ok(*distance),
        None => Err(RuntimeError::UndefinedSymbol(*variable)),
    }
}

pub mod env {

    use std::collections::HashMap;
    use util::env::TypeEnv;
    use util::symbol::Symbol;
    use util::Unique;

    use object::Object;
    use super::RuntimeError;
    use builtins::BuiltIn;

    use std::rc::Rc;
    use std::cell::RefCell;

    #[derive(Debug, Clone)]
    /// A Loxlocals,enviroment
    /// which contains a map of a Symbol and value and the scope in which a
    /// Symbol was declared
    pub struct Environment {
        actual: Rc<RefCell<EnvironmentImpl>>,
        unique: Unique,
    }

    impl PartialEq for Environment {
        fn eq(&self, other: &Environment) -> bool {
            &self.actual.borrow().values == &other.actual.borrow().values
        }
    }

    impl Environment {
        pub fn new() -> Self {
           Environment {
                actual: Rc::new(RefCell::new(EnvironmentImpl::new())),
                unique: Unique::new(),
           }

        }

        pub fn new_with_outer(outer: &Environment) -> Environment {
            Environment {
                actual: Rc::new(RefCell::new(EnvironmentImpl::with_values(outer))),
                unique: Unique::new(),
            }
        }

        pub fn unique_id(&mut self) -> Symbol {
            let next = Unique::new().0 + 3;
            Symbol(next)
        }

        pub fn fill_env(&mut self,env:&mut TypeEnv) {
        let built_in = BuiltIn::new().get_built_ins(env);

        for (variable, run_time_value) in built_in {
            self.define(variable, run_time_value);
        }
}


        /// Defines a Symbol by inserting the name and
        /// value into thelocals,environmentImpl
        pub fn define(&self, name: Symbol, value: Object) {
            self.actual.borrow_mut().values.insert(name, value);
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
                if actual.values.contains_key(&name) {
                    *actual.values.get_mut(name).unwrap() = value;
                    return Ok(());
                } else {
                    Err(RuntimeError::UndefinedSymbol(*name))
                }
            } else {
                match &actual.outer {
                    &Some(ref outer) => outer.assign(name, value, distance - 1),
                    &None => Err(RuntimeError::UndefinedSymbol(*name)),
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
                match &actual.outer {
                    &Some(ref outer) => outer.get(name, distance - 1),
                    &None => Err(RuntimeError::UndefinedSymbol(*name)),
                }
            }
        }

        pub fn dump(&self) {
            let actual = self.actual.borrow();
            println!("-----------ENVIRONMENT-------");
            for (name, value) in &actual.values {
                println!("name:{}, value:{}", name, value)
            }

            match actual.outer {
                Some(ref outer) => outer.dump(),
                None => (),
            }
        }
    }

    /// The actual struct that contains the values.
    /// It is wrapped within a Rc<RefCell<_>>
    #[derive(Debug)]
    pub(crate) struct EnvironmentImpl {
        outer: Option<Environment>,
        values: HashMap<Symbol, Object>,
    }

    impl EnvironmentImpl {
        fn new() -> Self {
            EnvironmentImpl {
                outer: None,
                values: HashMap::new(),
            }
        }

        fn with_values(outer: &Environment) -> Self {
            EnvironmentImpl {
                outer: Some(outer.clone()),
                values: HashMap::new(),
            }
        }
    }

}
