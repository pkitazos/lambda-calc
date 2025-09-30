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
    Int,
    Bool,
    Arrow(Box<Type>, Box<Type>),
}

#[derive(Debug, Clone, PartialEq)]
enum Const {
    Int(i32),
    Bool(bool),
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
    println!("--> hi from parse_term");
    let a = alt((parse_lambda, parse_app)).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_atom(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_atom");
    let a = alt((parse_const, parse_var, parse_paren)).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_paren(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_paren");
    let a = delimited(ws(char('(')), parse_term, ws(char(')'))).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_const(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_const");
    let a = alt((parse_bool, parse_int)).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_int(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_int");
    let a = map(digit1, |n: &str| {
        Term::Const(Const::Int(n.parse().unwrap()))
    })
    .parse(input);
    println!("{:#?}", a);
    a
}

fn parse_bool(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_bool");
    let a = alt((
        map(ws(tag("false")), |_| Term::Const(Const::Bool(false))),
        map(ws(tag("true")), |_| Term::Const(Const::Bool(true))),
    ))
    .parse(input);
    println!("{:#?}", a);
    a
}

fn parse_var(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_var");
    let (input, (v1, v2)) = pair(alpha1, alphanumeric0).parse(input)?;
    let var = format!("{v1}{v2}");

    println!("{:#?}", var);

    Ok((input, Term::Var(var)))
}

fn parse_lambda(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_lambda");
    let a = map(
        (ws(tag("fn")), parse_lambda_args, ws(char('.')), parse_term),
        |(_, args, _, body)| desugar_lambda(args, body),
    )
    .parse(input);
    println!("{:#?}", a);
    a
}

fn parse_lambda_args(input: &str) -> IResult<&str, Vec<(String, Type)>> {
    println!("--> hi from parse_lambda_args");
    let a = separated_list1(ws(char(',')), parse_lambda_arg).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_lambda_arg(input: &str) -> IResult<&str, (String, Type)> {
    println!("--> hi from parse_lambda_arg");
    let a = separated_pair(
        map(alpha1, |s: &str| s.to_string()),
        ws(char(':')),
        parse_type,
    )
    .parse(input);
    println!("{:#?}", a);
    a
}

fn parse_type(input: &str) -> IResult<&str, Type> {
    println!("--> hi from parse_type");
    let a = alt((parse_arrow_type, parse_base_type)).parse(input);
    println!("{:#?}", a);
    a
}

fn parse_base_type(input: &str) -> IResult<&str, Type> {
    println!("--> hi from parse_base_type");
    let a = alt((
        map(tag("int"), |_| Type::Int),
        map(tag("bool"), |_| Type::Bool),
    ))
    .parse(input);
    println!("{:#?}", a);
    a
}

fn parse_arrow_type(input: &str) -> IResult<&str, Type> {
    println!("--> hi from parse_arrow_type");
    let (input, lhs) = parse_base_type(input)?;
    let (input, _) = ws(tag("->")).parse(input)?;
    let (input, rhs) = parse_type(input)?;

    let a = Type::Arrow(Box::new(lhs), Box::new(rhs));
    println!("{:#?}", a);
    Ok((input, a))
}

fn desugar_lambda(args: Vec<(String, Type)>, body: Term) -> Term {
    args.into_iter()
        .rev()
        .fold(body, |acc, (v, t)| Term::Lambda(v, t, Box::new(acc)))
}

fn parse_app(input: &str) -> IResult<&str, Term> {
    println!("--> hi from parse_app");
    let (input, atoms) = many1(parse_atom).parse(input)?;

    let app = atoms
        .into_iter()
        .reduce(|acc, term| Term::App(Box::new(acc), Box::new(term)))
        .unwrap();
    println!("{:#?}", app);
    Ok((input, app))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test = "fn x:int . f x y";
    match parse_term(test) {
        Ok((_, term)) => println!("{:#?}", term),
        Err(e) => println!("Parse error: {:?}", e),
    }

    Ok(())
}
