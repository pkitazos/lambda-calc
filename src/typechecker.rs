use std::collections::HashMap;
use std::fmt;

use crate::{BinOp, Const, Term, Type};

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UndefinedVariable {
        var: String,
        term: Term,
    },
    TypeMismatch {
        expected: Type,
        found: Type,
        term: Term,
    },
    NotAFunction {
        found: Type,
        term: Term,
    },
    NotAPair {
        found: Type,
        term: Term,
    },
    IfBranchMismatch {
        then_type: Type,
        else_type: Type,
        term: Term,
    },
    IfScrutineeNotBool {
        found: Type,
        term: Term,
    },
    ArithmeticRequiresInt {
        found: Type,
        term: Term,
    },
    EqualityTypeMismatch {
        lhs: Type,
        rhs: Type,
        term: Term,
    },
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeError::UndefinedVariable { var, term } => {
                write!(f, "Undefined variable: {}\nIn term: {}", var, term)
            }
            TypeError::TypeMismatch {
                expected,
                found,
                term,
            } => {
                write!(
                    f,
                    "Type mismatch: expected {}, found {}\nIn term: {}",
                    expected, found, term
                )
            }
            TypeError::NotAFunction { found, term } => {
                write!(f, "Not a function: found type {}\nIn term: {}", found, term)
            }
            TypeError::NotAPair { found, term } => {
                write!(f, "Not a pair: found type {}\nIn term: {}", found, term)
            }
            TypeError::IfBranchMismatch {
                then_type,
                else_type,
                term,
            } => {
                write!(
                    f,
                    "If branch type mismatch: then has type {}, else has type {}\nIn term: {}",
                    then_type, else_type, term
                )
            }
            TypeError::IfScrutineeNotBool { found, term } => {
                write!(
                    f,
                    "If condition must be Bool, found {}\nIn term: {}",
                    found, term
                )
            }
            TypeError::ArithmeticRequiresInt { found, term } => {
                write!(
                    f,
                    "Arithmetic operation requires Int, found {}\nIn term: {}",
                    found, term
                )
            }
            TypeError::EqualityTypeMismatch { lhs, rhs, term } => {
                write!(
                    f,
                    "Equality requires matching types: left has type {}, right has type {}\nIn term: {}",
                    lhs, rhs, term
                )
            }
        }
    }
}

pub fn typecheck(env: &HashMap<String, Type>, term: &Term) -> Result<Type, TypeError> {
    match term {
        Term::Const(Const::Unit) => Ok(Type::Unit),
        Term::Const(Const::Int(_)) => Ok(Type::Int),
        Term::Const(Const::Bool(_)) => Ok(Type::Bool),

        Term::Var(var) => env
            .get(var)
            .map(|x| x.clone())
            .ok_or(TypeError::UndefinedVariable {
                var: var.clone(),
                term: term.clone(),
            }),

        Term::Pair(lhs, rhs) => typecheck_pair(env, lhs, rhs),

        Term::Lambda(var, t, term) => typecheck_in_extended_env(env, var.clone(), &t, term, |t2| {
            Ok(Type::Arrow(Box::new(t.clone()), Box::new(t2)))
        }),

        Term::Fst(m) => match typecheck(env, m)? {
            Type::Pair(lhs_type, _) => Ok(*lhs_type),

            other_type => Err(TypeError::NotAPair {
                found: other_type,
                term: term.clone(),
            }),
        },

        Term::Snd(m) => match typecheck(env, m)? {
            Type::Pair(_, rhs_type) => Ok(*rhs_type),

            other_type => Err(TypeError::NotAPair {
                found: other_type,
                term: term.clone(),
            }),
        },

        Term::App(m, n) => {
            let t_m = typecheck(env, m)?;
            let t_n = typecheck(env, n)?;

            match t_m {
                Type::Arrow(from, to) if *from == t_n => Ok(*to),

                Type::Arrow(from, _) => Err(TypeError::TypeMismatch {
                    expected: *from,
                    found: t_n,
                    term: term.clone(),
                }),

                _ => Err(TypeError::NotAFunction {
                    found: t_m,
                    term: term.clone(),
                }),
            }
        }

        Term::Let(var, m, n) => {
            let t = typecheck(env, m)?;
            typecheck_in_extended_env(env, var.clone(), &t, n, Ok)
        }

        Term::If(b, m, n) => {
            let t_b = typecheck(env, b)?;
            let t_m = typecheck(env, m)?;
            let t_n = typecheck(env, n)?;

            match (t_b, t_m == t_n) {
                (Type::Bool, true) => Ok(t_m),

                (Type::Bool, false) => Err(TypeError::IfBranchMismatch {
                    then_type: t_m,
                    else_type: t_n,
                    term: term.clone(),
                }),

                (other_type, _) => Err(TypeError::IfScrutineeNotBool {
                    found: other_type,
                    term: term.clone(),
                }),
            }
        }

        Term::BinOp(op, m, n) => {
            let t_m = typecheck(env, m)?;
            let t_n = typecheck(env, n)?;

            match (op, t_m, t_n) {
                (BinOp::Eq, t1, t2) if t1 == t2 => Ok(Type::Bool),

                (BinOp::Eq, t1, t2) => Err(TypeError::EqualityTypeMismatch {
                    lhs: t1,
                    rhs: t2,
                    term: term.clone(),
                }),

                (BinOp::Plus, Type::Int, Type::Int) => Ok(Type::Int),
                (BinOp::Minus, Type::Int, Type::Int) => Ok(Type::Int),

                (_, t1, t2) => Err(TypeError::ArithmeticRequiresInt {
                    found: if t1 != Type::Int { t1 } else { t2 },
                    term: term.clone(),
                }),
            }
        }
    }
}

fn typecheck_pair(env: &HashMap<String, Type>, lhs: &Term, rhs: &Term) -> Result<Type, TypeError> {
    let lhs_type = typecheck(env, lhs)?;
    let rhs_type = typecheck(env, rhs)?;

    Ok(Type::Pair(Box::new(lhs_type), Box::new(rhs_type)))
}

fn typecheck_in_extended_env<F>(
    env: &HashMap<String, Type>,
    var: String,
    t: &Type,
    term: &Term,
    modifier: F,
) -> Result<Type, TypeError>
where
    F: Fn(Type) -> Result<Type, TypeError>,
{
    let mut temp_env = env.clone();
    temp_env.insert(var, t.clone());
    typecheck(&temp_env, term).and_then(modifier)
}
