use infer::types::{Type, TypeCon};
use std::collections::HashMap;
use util::pos::Spanned;
use util::symbol::Symbol;

use std::hash::Hash;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Value {
    True,
    False,
    String(String),
    Var(String),
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
struct TypeName(pub String);

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Constructor {
    name: Symbol,
    arity: usize,
    span: usize,
}

/// A pattern can bind a value or it can match(destructre a value)
/// i.e 
/// match xs {
///     Cons(x,xs) -> ...,
///     xs -> ...
/// }
/// 
/// would be encoded as: 
/// [
///     Match("xs",[Bind("x"),Bind("xs")]),
///     Bind("xs")
/// ]
#[derive(Debug, Clone, PartialEq)]
enum Pattern {
    Bind(Type),                    // e.g foo /vars
    Match(Symbol,Vec<Pattern>),    //e.g. List(10,Nil)
}

// impl Constructor {
//     pub fn new(name:Symbol, arity: usize, span: usize) -> Self {
//         Self {
//             name,
//             arity,
//             span,
//         }
//     }
// }

// /// Attempts to match the expression against each pattern from a rule in rules.
// /// It succeds with the rhs if the rule matches; it fails otherwise
// fn fail(expr: Pattern, rules: &mut Vec<(Pattern, Pattern)>) -> Option<Pattern> {
//     if rules.is_empty() {
//         return None;
//     } else {
//         let (pat1, rhs1) = rules.pop().unwrap();

//         _match(pat1, expr, vec![], rhs1, rules)
//     }
// }

// fn succed(
//     mut work: Vec<(Pattern, Pattern)>,
//     rhs: Pattern,
//     rules: &mut Vec<(Pattern, Pattern)>,
// ) -> Option<Pattern> {
//     for rule in rules.iter() {
//         work.push(rule.clone());
//     }

//     if work.is_empty() {
//         Some(rhs)
//     } else {
//         let (pat1, obj1) = work.pop().unwrap();
//         _match(pat1, obj1, work, rhs, rules)
//     }
// }

// fn _match(
//     pat1: Pattern,
//     obj1: Pattern,
//     mut work: Vec<(Pattern, Pattern)>,
//     rhs: Pattern,
//     rules: &mut Vec<(Pattern, Pattern)>,
// ) -> Option<Pattern> {
//     match pat1 {
//         Pattern::Var(_) => succed(work, rhs, rules),
//         Pattern::Con(ref pcon, ref pargs) => match obj1 {
//             Pattern::Con(ref ocon, ref oargs) => {
//                 if ocon == pcon {
//                     let both = oargs
//                         .clone()
//                         .into_iter()
//                         .zip(pargs.clone().into_iter())
//                         .collect::<Vec<(Pattern, Pattern)>>();
//                     work.extend(both);

//                     succed(work, rhs, rules)
//                 } else {
//                     fail(obj1, rules)
//                 }
//             }
//             Pattern::Var(_) => succed(work, rhs, rules),
//         },
//     }
// }
