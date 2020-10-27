use std::collections::HashSet;
use std::fmt::{self, Display};

/// A pattern Constructor <br/>
/// i.e given enum RGB { <br/>
///     Red,<br/>
///     Green,<br/>
///     Blue<br/>
/// } <br/>
///<br/>
/// Red => Constructor {<br/>
///     arity:0,<br/>
///     span:3,<br/>
/// }
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Constructor {
    pub name: String,
    pub arity: usize,
    pub span: usize,
}

/// A pattern
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Con(Constructor, Vec<Pattern>), //e.g. List(10,Nil)
    WildCard,
    Or(Box<Pattern>, Box<Pattern>),
}

impl Pattern {
    pub fn is_con(&self) -> bool {
        match self {
            Pattern::Con(_, _) => true,
            _ => false,
        }
    }
}

pub fn nil() -> Pattern {
    Pattern::Con(
        Constructor {
            name: "Nil".into(),
            arity: 0,
            span: 1,
        },
        vec![],
    )
}

/// list split  pattern / x ::xy
pub fn split(lhs: Pattern, rhs: Pattern) -> Pattern {
    let con = Constructor {
        name: "Split".into(),
        arity: 2,
        span: 1,
    };

    Pattern::Con(con, vec![lhs, rhs])
}

/// list pattern | [(pattern)*]
pub fn list(args: Vec<Pattern>) -> Pattern {
    let con = Constructor {
        name: "List".into(),
        arity: args.len(),
        span: 1,
    };

    Pattern::Con(con, args)
}

/// wildcard pattern | _
pub fn wcard() -> Pattern {
    Pattern::WildCard
}

impl Pattern {
    pub fn con(&self) -> Constructor {
        match self {
            Pattern::Con(ref con, _) => con.clone(),
            _ => panic!("Trynna get the con of none constructor pattern"),
        }
    }
    pub fn cons(&self) -> HashSet<Constructor> {
        match self {
            Pattern::WildCard => HashSet::new(),
            Pattern::Con(ref con, _) => {
                let mut set = HashSet::new();
                set.insert(con.clone());
                set
            }
            Pattern::Or(ref lhs, ref rhs) => {
                let set = lhs.cons();
                set.union(&rhs.cons());
                set
            }
        }
    }
}

impl Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Pattern::Con(ref con, ref args) => {
                write!(f, "{}", con.name)?;

                if args.is_empty() {
                    return Ok(());
                }

                write!(f, "(")?;

                for (i, arg) in args.iter().enumerate() {
                    if i + 1 == args.len() {
                        write!(f, "{}", arg)?;
                    } else {
                        write!(f, "{},", arg)?;
                    }
                }

                write!(f, ")")
            }

            //            Pattern::Var(ref var) => write!(f, "{}", var),
            Pattern::WildCard => write!(f, "_"),
            Pattern::Or(ref lhs, ref rhs) => write!(f, "{} | {}", lhs, rhs),
        }
    }
}

impl Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
