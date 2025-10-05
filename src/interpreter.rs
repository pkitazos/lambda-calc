use crate::{BinOp, Const, Term};
use std::rc::Rc;

pub enum Env {
    Empty,
    Cons(String, Term, Rc<Env>),
}

pub fn empty_env() -> Rc<Env> {
    Rc::new(Env::Empty)
}

pub fn eval(term: Term, env: &Rc<Env>) -> Term {
    match term {
        Term::Const(_c) => term,

        Term::Var(v) => find(&v, env),

        Term::Pair(m, n) => Term::Pair(Box::new(eval(*m, env)), Box::new(eval(*n, env))),

        Term::Lambda(v, t, m) => Term::Lambda(v, t, m),

        Term::Fst(m) => match eval(*m, env) {
            Term::Pair(a, _b) => *a,
            other => panic!(
                "runtime error: expected a Pair in Fst, but found {:?}",
                other
            ),
        },
        Term::Snd(m) => match eval(*m, env) {
            Term::Pair(_a, b) => *b,
            other => panic!(
                "runtime error: expected a Pair in Snd, but found {:?}",
                other
            ),
        },

        Term::App(m, n) => match eval(*m, env) {
            Term::Lambda(v, _, m_2) => {
                let value = eval(*n, env);
                let new_env = Rc::new(Env::Cons(v, value, Rc::clone(&env)));

                eval(*m_2, &new_env)
            }
            other => panic!(
                "runtime error: expected a Lambda in App, but found {:?}",
                other
            ),
        },

        Term::Let(v, m, n) => {
            let value = eval(*m, env);
            let new_env = Rc::new(Env::Cons(v, value, Rc::clone(&env)));

            eval(*n, &new_env)
        }

        Term::If(b, m, n) => match eval(*b, env) {
            Term::Const(Const::Bool(true)) => eval(*m, env),
            Term::Const(Const::Bool(false)) => eval(*n, env),
            other => panic!(
                "runtime error: expected a Bool in If condition, but found {:?}",
                other
            ),
        },

        Term::BinOp(BinOp::Eq, m, n) => {
            let m2 = eval(*m, env);
            let n2 = eval(*n, env);

            match (m2, n2) {
                (Term::Const(Const::Bool(b_m)), Term::Const(Const::Bool(b_n))) => {
                    Term::Const(Const::Bool(b_m == b_n))
                }

                (Term::Const(Const::Int(i_m)), Term::Const(Const::Int(i_n))) => {
                    Term::Const(Const::Bool(i_m == i_n))
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
                (BinOp::Plus, Term::Const(Const::Int(i_m)), Term::Const(Const::Int(i_n))) => {
                    Term::Const(Const::Int(i_m + i_n))
                }

                (BinOp::Minus, Term::Const(Const::Int(i_m)), Term::Const(Const::Int(i_n))) => {
                    Term::Const(Const::Int(i_m - i_n))
                }
                (op, left, right) => panic!(
                    "runtime error: unsupported operation {:?} with operands {:?} and {:?}",
                    op, left, right
                ),
            }
        }
    }
}

fn find(v: &str, env: &Rc<Env>) -> Term {
    match env.as_ref() {
        Env::Empty => panic!("runtime error: variable '{}' not found in environment", v),
        Env::Cons(name, term, rest) => {
            if name == v {
                term.clone()
            } else {
                find(v, &rest)
            }
        }
    }
}
