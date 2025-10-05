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
            _ => panic!("runtime error"),
        },
        Term::Snd(m) => match eval(*m, env) {
            Term::Pair(_a, b) => *b,
            _ => panic!("runtime error"),
        },

        Term::App(m, n) => match eval(*m, env) {
            Term::Lambda(v, _, m_2) => {
                let value = eval(*n, env);
                let new_env = Rc::new(Env::Cons(v, value, Rc::clone(&env)));

                eval(*m_2, &new_env)
            }
            _ => panic!("runtime error"),
        },

        Term::Let(v, m, n) => {
            let value = eval(*m, env);
            let new_env = Rc::new(Env::Cons(v, value, Rc::clone(&env)));

            eval(*n, &new_env)
        }

        Term::If(b, m, n) => match eval(*b, env) {
            Term::Const(Const::Bool(true)) => eval(*m, env),
            Term::Const(Const::Bool(false)) => eval(*n, env),
            _ => panic!("runtime error"),
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

                _ => panic!("runtime error"),
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
                _ => panic!("runtime error"),
            }
        }
    }
}

fn find(v: &str, env: &Rc<Env>) -> Term {
    match env.as_ref() {
        Env::Empty => panic!("runtime error"),
        Env::Cons(name, term, rest) => {
            if name == v {
                term.clone()
            } else {
                find(v, &rest)
            }
        }
    }
}
