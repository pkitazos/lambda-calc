use crate::Term;
use std::{fmt, rc::Rc};

#[derive(Clone, Debug)]
pub enum Env {
    Empty,
    Cons(String, Value, Rc<Env>),
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Env::Empty => write!(f, ""),
            Env::Cons(v, m, env) => write!(f, "{}: {}, {}", v, m, env),
        }
    }
}

// pub fn env_from_defs(defs: Vec<(String, Term)>) -> Rc<Env> {
//     // initialise an empty env
//     for (id, expr) in defs {
//         // try to evaluate the expression with the env so far
//         // if it successfully evaluates store it under the id in the Env
//         // otherwise try evaluating the next one and come back to this one with a more complete env
//     }
// }

pub fn empty_env() -> Rc<Env> {
    Rc::new(Env::Empty)
}

#[derive(Clone, Debug)]
pub enum Value {
    Closure(String, Box<Term>, Rc<Env>), // variable, body, captured env
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Closure(v, m, env) => write!(f, " λ {} . {} Env{{{}}}", v, m, env),
        }
    }
}

pub fn eval(term: Term, env: &Rc<Env>) -> Result<Value, String> {
    match term {
        Term::Var(v) => find(&v, env),

        Term::Lambda(v, m) => Ok(Value::Closure(v, m, Rc::clone(env))),

        Term::App(m, n) => match eval(*m, env)? {
            Value::Closure(v, m_2, closure_env) => {
                let value = eval(*n, env)?;
                let new_env = Rc::new(Env::Cons(v, value, Rc::clone(&closure_env)));

                eval(*m_2, &new_env)
            }
        },
    }
}

fn find(v: &str, env: &Rc<Env>) -> Result<Value, String> {
    match env.as_ref() {
        Env::Empty => Err(format!(
            "runtime error: variable '{}' not found in environment",
            v
        )),
        Env::Cons(name, val, rest) => {
            if name == v {
                Ok(val.clone())
            } else {
                find(v, &rest)
            }
        }
    }
}
