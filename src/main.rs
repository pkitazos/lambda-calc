use std::{fs::File, io::Read};

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, digit1, space0},
    combinator::{map, recognize, verify},
    multi::{many0_count, many1, separated_list1},
    sequence::{delimited, pair, separated_pair},
};

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Unit,
    Int,
    Bool,
    Pair(Box<Type>, Box<Type>),
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
    Pair(Box<Term>, Box<Term>),
    Lambda(String, Type, Box<Term>),
    Fst(Box<Term>),
    Snd(Box<Term>),
    App(Box<Term>, Box<Term>),
    Let(String, Box<Term>, Box<Term>),
}

const KEYWORDS: &[&str] = &["fn", "let", "in", "fst", "snd"];

fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(space0, inner, space0)
}

fn parse_term(input: &str) -> IResult<&str, Term> {
    alt((parse_lambda, parse_let, parse_app)).parse(input)
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
        |id: &str| !KEYWORDS.contains(&id),
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
            delimited(
                ws(tag("let")),
                separated_pair(parse_identifier, ws(char('=')), parse_term),
                ws(tag("in")),
            ),
            parse_term,
        ),
        |((v, t2), t3): ((&str, Term), Term)| Term::Let(v.to_string(), Box::new(t2), Box::new(t3)),
    )
    .parse(input)
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
