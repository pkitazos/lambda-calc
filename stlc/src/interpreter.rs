use crate::{BinOp, Const, Term};
use std::{fmt, rc::Rc};

#[derive(Clone, Debug, PartialEq)]
pub enum Env {
    Empty,
    Cons(String, Box<Value>, Rc<Env>),
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Env::Empty => write!(f, ""),
            Env::Cons(v, m, env) => write!(f, "{}: {}, {}", v, m, env),
        }
    }
}

pub fn empty_env() -> Rc<Env> {
    Rc::new(Env::Empty)
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Const(Const),
    Pair(Box<Value>, Box<Value>),
    Closure(String, Box<Term>, Rc<Env>), // variable, body, captured env
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Const(c) => write!(f, " {} ", c),
            Value::Pair(m, n) => write!(f, " ({}, {}) ", m, n),
            Value::Closure(v, m, env) => write!(f, " λ {} . {} Env{{{}}} ", v, m, env),
        }
    }
}

pub fn eval(term: Term, env: &Rc<Env>) -> Value {
    match term {
        Term::Const(c) => Value::Const(c),

        Term::Var(v) => find(&v, env),

        Term::Pair(m, n) => Value::Pair(Box::new(eval(*m, env)), Box::new(eval(*n, env))),

        Term::Lambda(v, _, m) => Value::Closure(v, m, Rc::clone(env)),

        Term::Fst(m) => match eval(*m, env) {
            Value::Pair(a, _b) => *a,
            other => panic!(
                "runtime error: expected a Pair in Fst, but found {:?}",
                other
            ),
        },
        Term::Snd(m) => match eval(*m, env) {
            Value::Pair(_a, b) => *b,
            other => panic!(
                "runtime error: expected a Pair in Snd, but found {:?}",
                other
            ),
        },

        Term::App(m, n) => match eval(*m, env) {
            Value::Closure(v, m_2, closure_env) => {
                let value = eval(*n, env);
                let new_env = Rc::new(Env::Cons(v, Box::new(value), Rc::clone(&closure_env)));

                eval(*m_2, &new_env)
            }
            other => panic!(
                "runtime error: expected a Lambda in App, but found {:?}",
                other
            ),
        },

        Term::Let(v, m, n) => {
            let value = eval(*m, env);
            let new_env = Rc::new(Env::Cons(v, Box::new(value), Rc::clone(&env)));

            eval(*n, &new_env)
        }

        Term::If(b, m, n) => match eval(*b, env) {
            Value::Const(Const::Bool(true)) => eval(*m, env),
            Value::Const(Const::Bool(false)) => eval(*n, env),
            other => panic!(
                "runtime error: expected a Bool in If condition, but found {:?}",
                other
            ),
        },

        Term::BinOp(BinOp::Eq, m, n) => {
            let m2 = eval(*m, env);
            let n2 = eval(*n, env);

            match (m2, n2) {
                (Value::Const(Const::Bool(b_m)), Value::Const(Const::Bool(b_n))) => {
                    Value::Const(Const::Bool(b_m == b_n))
                }

                (Value::Const(Const::Int(i_m)), Value::Const(Const::Int(i_n))) => {
                    Value::Const(Const::Bool(i_m == i_n))
                }

                (left, right) => panic!(
                    "runtime error: cannot compare {:?} and {:?} with Eq",
                    left, right
                ),
            }
        }

        Term::BinOp(op, m, n) => {
            let m2 = eval(*m, env);
            let n2 = eval(*n, env);

            match (op, m2, n2) {
                (BinOp::Plus, Value::Const(Const::Int(i_m)), Value::Const(Const::Int(i_n))) => {
                    Value::Const(Const::Int(i_m + i_n))
                }

                (BinOp::Minus, Value::Const(Const::Int(i_m)), Value::Const(Const::Int(i_n))) => {
                    Value::Const(Const::Int(i_m - i_n))
                }
                (op, left, right) => panic!(
                    "runtime error: unsupported operation {:?} with operands {:?} and {:?}",
                    op, left, right
                ),
            }
        }
    }
}

fn find(v: &str, env: &Rc<Env>) -> Value {
    match env.as_ref() {
        Env::Empty => panic!("runtime error: variable '{}' not found in environment", v),
        Env::Cons(name, value, rest) => {
            if name == v {
                *value.clone()
            } else {
                find(v, &rest)
            }
        }
    }
}
