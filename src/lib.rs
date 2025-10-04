use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, space0},
    combinator::{map, recognize, verify},
    multi::{many0_count, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair},
};

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

const BUILTINS: &[&str] = &[
    "fn", "let", "in", "fst", "snd", "if", "then", "else", "==", "+", "-",
];

fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(space0, inner, space0)
}

pub fn parse_term(input: &str) -> IResult<&str, Term> {
    alt((parse_lambda, parse_let, parse_bin_op, parse_if, parse_app)).parse(input)
}

fn parse_atom(input: &str) -> IResult<&str, Term> {
    alt((parse_const, parse_var, parse_pair, parse_paren)).parse(input)
}

fn parse_paren(input: &str) -> IResult<&str, Term> {
    delimited(ws(char('(')), parse_term, ws(char(')'))).parse(input)
}

fn parse_const(input: &str) -> IResult<&str, Term> {
    alt((parse_unit, parse_int, parse_bool)).parse(input)
}

fn parse_pair(input: &str) -> IResult<&str, Term> {
    map(
        delimited(
            ws(char('(')),
            separated_pair(parse_term, ws(char(',')), parse_term),
            ws(char(')')),
        ),
        |(fst, snd): (Term, Term)| Term::Pair(Box::new(fst), Box::new(snd)),
    )
    .parse(input)
}

fn parse_int(input: &str) -> IResult<&str, Term> {
    map(digit1, |n: &str| {
        Term::Const(Const::Int(n.parse().unwrap()))
    })
    .parse(input)
}

fn parse_bool(input: &str) -> IResult<&str, Term> {
    alt((
        map(ws(tag("false")), |_| Term::Const(Const::Bool(false))),
        map(ws(tag("true")), |_| Term::Const(Const::Bool(true))),
    ))
    .parse(input)
}

fn parse_unit(input: &str) -> IResult<&str, Term> {
    map(ws(tag("()")), |_| Term::Const(Const::Unit)).parse(input)
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    verify(
        recognize(pair(
            alt((alpha1, tag("_"))),
            many0_count(alt((alphanumeric1, tag("_")))),
        )),
        |id: &str| !BUILTINS.contains(&id),
    )
    .parse(input)
}

fn parse_var(input: &str) -> IResult<&str, Term> {
    map(parse_identifier, |v: &str| Term::Var(v.to_string())).parse(input)
}

fn parse_lambda(input: &str) -> IResult<&str, Term> {
    map(
        (ws(tag("fn")), parse_lambda_args, ws(char('.')), parse_term),
        |(_, args, _, body)| desugar_lambda(args, body),
    )
    .parse(input)
}

fn parse_lambda_args(input: &str) -> IResult<&str, Vec<(String, Type)>> {
    separated_list1(ws(char(',')), parse_lambda_arg).parse(input)
}

fn parse_lambda_arg(input: &str) -> IResult<&str, (String, Type)> {
    separated_pair(
        map(parse_identifier, |s: &str| s.to_string()),
        ws(char(':')),
        parse_type,
    )
    .parse(input)
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((parse_arrow_type, parse_pair_type, parse_base_type)).parse(input)
}

fn parse_base_type(input: &str) -> IResult<&str, Type> {
    alt((
        map(tag("unit"), |_| Type::Unit),
        map(tag("int"), |_| Type::Int),
        map(tag("bool"), |_| Type::Bool),
    ))
    .parse(input)
}

fn parse_pair_type(input: &str) -> IResult<&str, Type> {
    map(
        delimited(
            ws(char('(')),
            separated_pair(parse_type, ws(char(',')), parse_type),
            ws(char(')')),
        ),
        |(fst, snd): (Type, Type)| Type::Pair(Box::new(fst), Box::new(snd)),
    )
    .parse(input)
}

fn parse_arrow_type(input: &str) -> IResult<&str, Type> {
    map(
        separated_pair(parse_base_type, ws(tag("->")), parse_type),
        |(lhs, rhs): (Type, Type)| Type::Arrow(Box::new(lhs), Box::new(rhs)),
    )
    .parse(input)
}

fn desugar_lambda(args: Vec<(String, Type)>, body: Term) -> Term {
    args.into_iter()
        .rev()
        .fold(body, |acc, (v, t)| Term::Lambda(v, t, Box::new(acc)))
}

fn parse_app(input: &str) -> IResult<&str, Term> {
    alt((parse_pair_destructor, parse_arbitrary_app)).parse(input)
}

fn parse_pair_destructor(input: &str) -> IResult<&str, Term> {
    alt((
        map(
            pair(ws(tag("fst")), parse_atom),
            |(_, term): (&str, Term)| Term::Fst(Box::new(term)),
        ),
        map(
            pair(ws(tag("snd")), parse_atom),
            |(_, term): (&str, Term)| Term::Snd(Box::new(term)),
        ),
    ))
    .parse(input)
}

fn parse_arbitrary_app(input: &str) -> IResult<&str, Term> {
    map(many1(ws(parse_atom)), |atoms: Vec<Term>| {
        atoms
            .into_iter()
            .reduce(|acc, term| Term::App(Box::new(acc), Box::new(term)))
            .unwrap()
    })
    .parse(input)
}

fn parse_let(input: &str) -> IResult<&str, Term> {
    map(
        (
            preceded(ws(tag("let")), parse_identifier),
            preceded(ws(char('=')), parse_term),
            preceded(ws(tag("in")), parse_term),
        ),
        |(v, t2, t3): (&str, Term, Term)| Term::Let(v.to_string(), Box::new(t2), Box::new(t3)),
    )
    .parse(input)
}

fn parse_if(input: &str) -> IResult<&str, Term> {
    map(
        (
            preceded(ws(tag("if")), parse_term),
            preceded(ws(tag("then")), parse_term),
            preceded(ws(tag("else")), parse_term),
        ),
        |(t1, t2, t3): (Term, Term, Term)| Term::If(Box::new(t1), Box::new(t2), Box::new(t3)),
    )
    .parse(input)
}

fn parse_bin_op(input: &str) -> IResult<&str, Term> {
    map(
        (
            parse_atom,
            alt((
                map(ws(tag("==")), |_| BinOp::Eq),
                map(ws(char('+')), |_| BinOp::Plus),
                map(ws(char('-')), |_| BinOp::Minus),
            )),
            parse_atom,
        ),
        |(t1, op, t2): (Term, BinOp, Term)| Term::BinOp(op, Box::new(t1), Box::new(t2)),
    )
    .parse(input)
}

use std::collections::HashMap;

pub fn typecheck(env: &HashMap<String, Type>, term: &Term) -> Result<Type, String> {
    match term {
        Term::Const(Const::Unit) => Ok(Type::Unit),
        Term::Const(Const::Int(_)) => Ok(Type::Int),
        Term::Const(Const::Bool(_)) => Ok(Type::Bool),

        Term::Var(var) => match env.get(var) {
            Some(t) => Ok(t.clone()),
            None => Err("No variable with this name has been initialised".to_string()),
        },

        Term::Pair(lhs, rhs) => typecheck_pair(env, lhs, rhs),

        Term::Lambda(var, t, term) => {
            let mut temp_env = env.clone();
            temp_env.insert(var.clone(), t.clone());
            let body_type = typecheck(&temp_env, term)?;
            Ok(Type::Arrow(Box::new(t.clone()), Box::new(body_type)))
        }

        Term::Fst(term) => {
            if let Ok(Type::Pair(lhs_type, _)) = typecheck(env, term) {
                Ok(*lhs_type)
            } else {
                Err("can't take first element of non-pair".to_string())
            }
        }

        Term::Snd(term) => {
            if let Ok(Type::Pair(_, rhs_type)) = typecheck(env, term) {
                Ok(*rhs_type)
            } else {
                Err("can't take second element of non-pair".to_string())
            }
        }

        Term::App(m, n) => {
            if let Ok(Type::Arrow(from, to)) = typecheck(env, m) {
                if let Ok(t) = typecheck(env, n) {
                    if *from == t {
                        Ok(*to)
                    } else {
                        Err(format!(
                            "Can't apply term of type {t:#?} to function of type {from:#?} -> {to:#?}"
                        ))
                    }
                } else {
                    Err("error in N term".to_string())
                }
            } else {
                Err("error in M term".to_string())
            }
        }

        Term::Let(var, m, n) => {
            if let Ok(t) = typecheck(env, m) {
                let mut temp_env = env.clone();
                temp_env.insert(var.clone(), t.clone());
                typecheck(&temp_env, n)
            } else {
                Err("error in term m".to_string())
            }
        }

        Term::If(b, m, n) => {
            if let Ok(Type::Bool) = typecheck(env, b) {
                if let Ok(t1) = typecheck(env, m) {
                    if let Ok(t2) = typecheck(env, n) {
                        if t1 == t2 {
                            Ok(t1)
                        } else {
                            Err("both branches of if-expression mus return the same type"
                                .to_string())
                        }
                    } else {
                        Err("error in expression n".to_string())
                    }
                } else {
                    Err("error in expression m".to_string())
                }
            } else {
                Err("boolean expression required as scrutinee of if-expression".to_string())
            }
        }

        Term::BinOp(BinOp::Eq, m, n) => {
            if let Ok(t1) = typecheck(env, m) {
                if let Ok(t2) = typecheck(env, n) {
                    if t1 == t2 {
                        Ok(Type::Bool)
                    } else {
                        Err("both branches of equality comparison must be the same type"
                            .to_string())
                    }
                } else {
                    Err("error in expression n".to_string())
                }
            } else {
                Err("error in expression m".to_string())
            }
        }

        Term::BinOp(_, m, n) => {
            if let Ok(Type::Int) = typecheck(env, m) {
                if let Ok(Type::Int) = typecheck(env, n) {
                    Ok(Type::Int)
                } else {
                    Err("expression n must be Int".to_string())
                }
            } else {
                Err("expression m must be Int".to_string())
            }
        }
    }
}

fn typecheck_pair(
    env: &HashMap<String, Type>,
    lhs: &Box<Term>,
    rhs: &Box<Term>,
) -> Result<Type, String> {
    if let Ok(lhs_type) = typecheck(env, lhs) {
        if let Ok(rhs_type) = typecheck(env, rhs) {
            Ok(Type::Pair(Box::new(lhs_type), Box::new(rhs_type)))
        } else {
            Err("Rhs had an error".to_string())
        }
    } else {
        Err("Lhs had an error".to_string())
    }
}
