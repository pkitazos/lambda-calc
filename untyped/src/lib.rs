use std::fmt;

pub mod interpreter;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<(String, Term)>,
    pub expression: Option<Term>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Var(String),
    Lambda(String, Box<Term>),
    App(Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var(v) => write!(f, " {} ", v),
            Term::Lambda(v, m) => write!(f, " λ{}.{} ", v, m),
            Term::App(m, n) => write!(f, " {} {} ", m, n),
        }
    }
}
