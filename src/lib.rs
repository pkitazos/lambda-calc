use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, space0},
    combinator::{map, recognize, verify},
    multi::{many0_count, many1, separated_list1},
    sequence::{delimited, pair, preceded, separated_pair},
};
use std::collections::HashMap;

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

pub fn typecheck(env: &HashMap<String, Type>, term: &Term) -> Result<Type, String> {
    match term {
        Term::Const(Const::Unit) => Ok(Type::Unit),
        Term::Const(Const::Int(_)) => Ok(Type::Int),
        Term::Const(Const::Bool(_)) => Ok(Type::Bool),

        Term::Var(var) => env
            .get(var)
            .map(|x| x.clone())
            .ok_or("No variable with this name has been initialised".to_string()),

        Term::Pair(lhs, rhs) => typecheck_pair(env, lhs, rhs),

        Term::Lambda(var, t, term) => typecheck_in_extended_env(env, var.clone(), &t, term, |t2| {
            Ok(Type::Arrow(Box::new(t.clone()), Box::new(t2)))
        }),

        Term::Fst(term) => match typecheck(env, term)? {
            Type::Pair(lhs_type, _) => Ok(*lhs_type),
            _ => Err("can't take first element of non-pair".to_string()),
        },

        Term::Snd(term) => match typecheck(env, term)? {
            Type::Pair(_, rhs_type) => Ok(*rhs_type),
            _ => Err("can't take second element of non-pair".to_string()),
        },

        Term::App(m, n) => {
            let t_m = typecheck(env, m)?;
            let t_n = typecheck(env, n)?;

            match t_m {
                Type::Arrow(from, to) if *from == t_n => Ok(*to),
                Type::Arrow(from, to) => Err(format!(
                    "Can't apply term of type {t_n:#?} to function of type {from:#?} -> {to:#?}"
                )),
                _ => Err("term m must be a function".to_string()),
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
                (Type::Bool, false) => Err("branches must have same type".to_string()),
                _ => Err("scrutinee must be bool".to_string()),
            }
        }

        Term::BinOp(op, m, n) => {
            let t_m = typecheck(env, m)?;
            let t_n = typecheck(env, n)?;

            match (op, t_m, t_n) {
                (BinOp::Eq, t1, t2) if t1 == t2 => Ok(Type::Bool),
                (BinOp::Eq, _, _) => {
                    Err("both branches of equality comparison must be the same type".to_string())
                }

                (BinOp::Plus, Type::Int, Type::Int) => Ok(Type::Int),
                (BinOp::Minus, Type::Int, Type::Int) => Ok(Type::Int),
                _ => Err(
                    "arithemetic operations require integers on both sides of the operator"
                        .to_string(),
                ),
            }
        }
    }
}

fn typecheck_pair(env: &HashMap<String, Type>, lhs: &Term, rhs: &Term) -> Result<Type, String> {
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
) -> Result<Type, String>
where
    F: Fn(Type) -> Result<Type, String>,
{
    let mut temp_env = env.clone();
    temp_env.insert(var, t.clone());
    typecheck(&temp_env, term).and_then(modifier)
}
