use std::fmt;

pub mod interpreter;
pub mod parser;

#[derive(Debug, Clone)]
pub struct Program {
    pub definitions: Vec<(String, Term)>,
    pub expression: Option<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Var(String),
    Lambda(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    // Comment(String),
}

/// Wrapper that makes `Debug` (incl. the pretty `{:#?}` form) defer to a
/// `Term`'s `Display`. Wrap each element of a collection — e.g.
/// `stack.iter().map(Show).collect::<Vec<_>>()` — to get the default list
/// formatting around terms printed in lambda notation.
pub struct Show<'a>(pub &'a Term);

impl fmt::Debug for Show<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Var(v) => write!(f, "{}", v),
            Term::Lambda(v, m) => write!(f, "(λ{}.{})", v, m),
            Term::App(m, n) => write!(f, "({} {})", m, n),
            // Term::Comment(x) => write!(f, "{}", x),
        }
    }
}
