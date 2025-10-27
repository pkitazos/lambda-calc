use std::fmt;

pub mod interpreter;
pub mod parser;
pub mod typechecker;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Pair(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Pair(a, b) => write!(f, "{} × {}", a, b),
            Type::Arrow(a, b) => write!(f, "{} -> {}", a, b),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Const {
    Int(i32),
    Bool(bool),
    Unit,
}

impl fmt::Display for Const {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Const::Unit => write!(f, "()"),
            Const::Int(n) => write!(f, "{}", n),
            Const::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Eq,
    Plus,
    Minus,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Eq => write!(f, "=="),
            BinOp::Plus => write!(f, "+"),
            BinOp::Minus => write!(f, "-"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Const(Const),
    Var(String),
    Pair(Box<Term>, Box<Term>),
    Lambda(String, Type, Box<Term>),
    Fst(Box<Term>),
    Snd(Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    BinOp(BinOp, Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Term::Const(c) => write!(f, "{}", c),
            Term::Var(v) => write!(f, "{}", v),
            Term::Pair(m, n) => write!(f, "({}, {})", m, n),
            Term::Lambda(v, t, m) => write!(f, " fn {}: {} .{} ", v, t, m),
            Term::Fst(m) => write!(f, " fst {} ", m),
            Term::Snd(m) => write!(f, " snd {} ", m),
            Term::App(m, n) => write!(f, " {} {} ", m, n),
            Term::Let(v, m, n) => write!(f, " let {} = {} in {} ", v, m, n),
            Term::If(b, m, n) => write!(f, " if {} then {} else {} ", b, m, n),
            Term::BinOp(op, m, n) => write!(f, " {} {} {} ", m, op, n),
        }
    }
}
