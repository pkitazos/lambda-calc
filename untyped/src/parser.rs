use crate::Term;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, char, space0},
    combinator::{map, recognize},
    multi::{many0_count, many1},
    sequence::{delimited, pair},
};

fn ws<'a, F, O>(inner: F) -> impl Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>
where
    F: Parser<&'a str, Output = O, Error = nom::error::Error<&'a str>>,
{
    delimited(space0, inner, space0)
}

pub fn parse(input: &str) -> IResult<&str, Term> {
    parse_term(input)
}

fn parse_term(input: &str) -> IResult<&str, Term> {
    alt((parse_lambda, parse_app)).parse(input)
}

fn parse_atom(input: &str) -> IResult<&str, Term> {
    alt((parse_var, parse_paren)).parse(input)
}

fn parse_paren(input: &str) -> IResult<&str, Term> {
    delimited(ws(char('(')), parse_term, ws(char(')'))).parse(input)
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))
    .parse(input)
}

fn parse_var(input: &str) -> IResult<&str, Term> {
    map(parse_identifier, |v: &str| Term::Var(v.to_string())).parse(input)
}

fn parse_lambda(input: &str) -> IResult<&str, Term> {
    map(
        (ws(tag("λ")), parse_lambda_args, ws(char('.')), parse_term),
        |(_, args, _, body)| desugar_lambda(args, body),
    )
    .parse(input)
}

fn parse_lambda_args(input: &str) -> IResult<&str, Vec<String>> {
    map(many1(ws(parse_identifier)), |args| {
        args.into_iter().map(|x| x.to_string()).collect()
    })
    .parse(input)
}

fn desugar_lambda(args: Vec<String>, body: Term) -> Term {
    args.into_iter()
        .rev()
        .fold(body, |acc, v| Term::Lambda(v, Box::new(acc)))
}

fn parse_app(input: &str) -> IResult<&str, Term> {
    map(many1(ws(parse_atom)), |atoms: Vec<Term>| {
        atoms
            .into_iter()
            .reduce(|acc, term| Term::App(Box::new(acc), Box::new(term)))
            .unwrap()
    })
    .parse(input)
}
