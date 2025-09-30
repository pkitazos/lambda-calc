use std::{fs::File, io::Read};

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric0, char, digit1, space0},
    combinator::map,
    multi::{many1, separated_list1},
    sequence::{delimited, pair, separated_pair},
};

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Unit,
    Int,
    Bool,
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
enum Const {
    Int(i32),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
enum Term {
    Const(Const),
    Var(String),
    Lambda(String, Type, Box<Term>),
    App(Box<Term>, Box<Term>),
}

fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(space0, inner, space0)
}

fn parse_term(input: &str) -> IResult<&str, Term> {
    alt((parse_lambda, parse_app)).parse(input)
}

fn parse_atom(input: &str) -> IResult<&str, Term> {
    alt((parse_const, parse_var, parse_paren)).parse(input)
}

fn parse_paren(input: &str) -> IResult<&str, Term> {
    delimited(ws(char('(')), parse_term, ws(char(')'))).parse(input)
}

fn parse_const(input: &str) -> IResult<&str, Term> {
    alt((parse_unit, parse_int, parse_bool)).parse(input)
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

fn parse_var(input: &str) -> IResult<&str, Term> {
    let (input, (v1, v2)) = pair(alpha1, alphanumeric0).parse(input)?;
    let var = format!("{v1}{v2}");

    Ok((input, Term::Var(var)))
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
        map(alpha1, |s: &str| s.to_string()),
        ws(char(':')),
        parse_type,
    )
    .parse(input)
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    alt((parse_arrow_type, parse_base_type)).parse(input)
}

fn parse_base_type(input: &str) -> IResult<&str, Type> {
    alt((
        map(tag("unit"), |_| Type::Unit),
        map(tag("int"), |_| Type::Int),
        map(tag("bool"), |_| Type::Bool),
    ))
    .parse(input)
}

fn parse_arrow_type(input: &str) -> IResult<&str, Type> {
    let (input, lhs) = parse_base_type(input)?;
    let (input, _) = ws(tag("->")).parse(input)?;
    let (input, rhs) = parse_type(input)?;

    Ok((input, Type::Arrow(Box::new(lhs), Box::new(rhs))))
}

fn desugar_lambda(args: Vec<(String, Type)>, body: Term) -> Term {
    args.into_iter()
        .rev()
        .fold(body, |acc, (v, t)| Term::Lambda(v, t, Box::new(acc)))
}

fn parse_app(input: &str) -> IResult<&str, Term> {
    let (input, atoms) = many1(ws(parse_atom)).parse(input)?;

    let app = atoms
        .into_iter()
        .reduce(|acc, term| Term::App(Box::new(acc), Box::new(term)))
        .unwrap();

    Ok((input, app))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    match read_code("test/test.lc") {
        Some(code) => {
            println!("{:#?}", code);
            if let Ok((_, expr)) = parse_term(&code) {
                println!("{:#?}", expr);
            }
        }
        None => println!("something went wrong"),
    }

    Ok(())
}

fn read_code(filename: &str) -> Option<String> {
    let mut file = File::open(filename).ok()?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).ok()?;

    Some(contents.strip_suffix('\n').unwrap_or(&contents).to_string())
}
