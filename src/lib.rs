pub mod typechecker;

pub mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Unit,
    Int,
    Bool,
    Pair(Box<Type>, Box<Type>),
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(i32),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Eq,
    Plus,
    Minus,
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
