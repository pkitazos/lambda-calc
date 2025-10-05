use stlc::*;

#[test]
fn test_const_int() {
    let input = "42";
    let expected = Term::Const(Const::Int(42));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_const_bool_true() {
    let input = "true";
    let expected = Term::Const(Const::Bool(true));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_const_bool_false() {
    let input = "false";
    let expected = Term::Const(Const::Bool(false));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_const_unit() {
    let input = "()";
    let expected = Term::Const(Const::Unit);
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_x() {
    let input = "x";
    let expected = Term::Var("x".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_foo_bar() {
    let input = "foo_bar";
    let expected = Term::Var("foo_bar".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_underscore() {
    let input = "_underscore";
    let expected = Term::Var("_underscore".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_pair_int() {
    let input = "(1, 2)";
    let expected = Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_pair_bool() {
    let input = "(true, false)";
    let expected = Term::Pair(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Bool(false))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_pair_nested() {
    let input = "((1, 2), 3)";
    let expected = Term::Pair(
        Box::new(Term::Pair(
            Box::new(Term::Const(Const::Int(1))),
            Box::new(Term::Const(Const::Int(2))),
        )),
        Box::new(Term::Const(Const::Int(3))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_pair_var() {
    let input = "(x, y)";
    let expected = Term::Pair(
        Box::new(Term::Var("x".to_string())),
        Box::new(Term::Var("y".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_simple() {
    let input = "fn x: int . x";
    let expected = Term::Lambda(
        "x".to_string(),
        Type::Int,
        Box::new(Term::Var("x".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_multi_arg() {
    let input = "fn x: int, y: int . x";
    let expected = Term::Lambda(
        "x".to_string(),
        Type::Int,
        Box::new(Term::Lambda(
            "y".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_arrow_type() {
    let input = "fn f: int -> int, x: int . f x";
    let expected = Term::Lambda(
        "f".to_string(),
        Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::App(
                Box::new(Term::Var("f".to_string())),
                Box::new(Term::Var("x".to_string())),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_fst() {
    let input = "fst (1, 2)";
    let expected = Term::Fst(Box::new(Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    )));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_snd() {
    let input = "snd (x, y)";
    let expected = Term::Snd(Box::new(Term::Pair(
        Box::new(Term::Var("x".to_string())),
        Box::new(Term::Var("y".to_string())),
    )));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_fst_nested() {
    let input = "fst (fst ((1, 2), 3))";
    let expected = Term::Fst(Box::new(Term::Fst(Box::new(Term::Pair(
        Box::new(Term::Pair(
            Box::new(Term::Const(Const::Int(1))),
            Box::new(Term::Const(Const::Int(2))),
        )),
        Box::new(Term::Const(Const::Int(3))),
    )))));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_simple() {
    let input = "f x";
    let expected = Term::App(
        Box::new(Term::Var("f".to_string())),
        Box::new(Term::Var("x".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_multi() {
    let input = "f x y";
    let expected = Term::App(
        Box::new(Term::App(
            Box::new(Term::Var("f".to_string())),
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Var("y".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_nested() {
    let input = "f (g x)";
    let expected = Term::App(
        Box::new(Term::Var("f".to_string())),
        Box::new(Term::App(
            Box::new(Term::Var("g".to_string())),
            Box::new(Term::Var("x".to_string())),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_lambda() {
    let input = "(fn x: int . x) 5";
    let expected = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Const(Const::Int(5))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_let_simple() {
    let input = "let x = 5 in x";
    let expected = Term::Let(
        "x".to_string(),
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Var("x".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_let_lambda() {
    let input = "let f = fn x: int . x in f 10";
    let expected = Term::Let(
        "f".to_string(),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::App(
            Box::new(Term::Var("f".to_string())),
            Box::new(Term::Const(Const::Int(10))),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_let_nested() {
    let input = "let x = 1 in let y = 2 in x";
    let expected = Term::Let(
        "x".to_string(),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Let(
            "y".to_string(),
            Box::new(Term::Const(Const::Int(2))),
            Box::new(Term::Var("x".to_string())),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_if_simple() {
    let input = "if true then 1 else 2";
    let expected = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_if_with_binop() {
    let input = "if x == 5 then x else 0";
    let expected = Term::If(
        Box::new(Term::BinOp(
            BinOp::Eq,
            Box::new(Term::Var("x".to_string())),
            Box::new(Term::Const(Const::Int(5))),
        )),
        Box::new(Term::Var("x".to_string())),
        Box::new(Term::Const(Const::Int(0))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_if_complex() {
    let input = "if (x == 2) then (x + 4) else (x - 2)";
    let expected = Term::If(
        Box::new(Term::BinOp(
            BinOp::Eq,
            Box::new(Term::Var("x".to_string())),
            Box::new(Term::Const(Const::Int(2))),
        )),
        Box::new(Term::BinOp(
            BinOp::Plus,
            Box::new(Term::Var("x".to_string())),
            Box::new(Term::Const(Const::Int(4))),
        )),
        Box::new(Term::BinOp(
            BinOp::Minus,
            Box::new(Term::Var("x".to_string())),
            Box::new(Term::Const(Const::Int(2))),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_binop_plus() {
    let input = "1 + 2";
    let expected = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_binop_minus() {
    let input = "x - y";
    let expected = Term::BinOp(
        BinOp::Minus,
        Box::new(Term::Var("x".to_string())),
        Box::new(Term::Var("y".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_binop_eq() {
    let input = "5 == 5";
    let expected = Term::BinOp(
        BinOp::Eq,
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Int(5))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_binop_plus_var() {
    let input = "x + 1";
    let expected = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Var("x".to_string())),
        Box::new(Term::Const(Const::Int(1))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_let_add() {
    let input = "let add = fn x: int, y: int . x + y in add 3 5";
    let expected = Term::Let(
        "add".to_string(),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Lambda(
                "y".to_string(),
                Type::Int,
                Box::new(Term::BinOp(
                    BinOp::Plus,
                    Box::new(Term::Var("x".to_string())),
                    Box::new(Term::Var("y".to_string())),
                )),
            )),
        )),
        Box::new(Term::App(
            Box::new(Term::App(
                Box::new(Term::Var("add".to_string())),
                Box::new(Term::Const(Const::Int(3))),
            )),
            Box::new(Term::Const(Const::Int(5))),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_lambda_if() {
    let input = "(fn x: int . if x == 0 then 1 else x - 1) 10";
    let expected = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::If(
                Box::new(Term::BinOp(
                    BinOp::Eq,
                    Box::new(Term::Var("x".to_string())),
                    Box::new(Term::Const(Const::Int(0))),
                )),
                Box::new(Term::Const(Const::Int(1))),
                Box::new(Term::BinOp(
                    BinOp::Minus,
                    Box::new(Term::Var("x".to_string())),
                    Box::new(Term::Const(Const::Int(1))),
                )),
            )),
        )),
        Box::new(Term::Const(Const::Int(10))),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_let_pair() {
    let input = "let pair = (1, 2) in (fst pair) + (snd pair)";
    let expected = Term::Let(
        "pair".to_string(),
        Box::new(Term::Pair(
            Box::new(Term::Const(Const::Int(1))),
            Box::new(Term::Const(Const::Int(2))),
        )),
        Box::new(Term::BinOp(
            BinOp::Plus,
            Box::new(Term::Fst(Box::new(Term::Var("pair".to_string())))),
            Box::new(Term::Snd(Box::new(Term::Var("pair".to_string())))),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_lambda_pair_type() {
    let input = "fn p: (int, int) . (fst p) + (snd p)";
    let expected = Term::Lambda(
        "p".to_string(),
        Type::Pair(Box::new(Type::Int), Box::new(Type::Int)),
        Box::new(Term::BinOp(
            BinOp::Plus,
            Box::new(Term::Fst(Box::new(Term::Var("p".to_string())))),
            Box::new(Term::Snd(Box::new(Term::Var("p".to_string())))),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_if_lambda() {
    let input = "if true then fn x: int . x else fn y: int . y + 1";
    let expected = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Lambda(
            "y".to_string(),
            Type::Int,
            Box::new(Term::BinOp(
                BinOp::Plus,
                Box::new(Term::Var("y".to_string())),
                Box::new(Term::Const(Const::Int(1))),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_complex_nested_let() {
    let input = "let f = fn x: int . x + 1 in let g = fn y: int . y - 1 in f (g 10)";
    let expected = Term::Let(
        "f".to_string(),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::BinOp(
                BinOp::Plus,
                Box::new(Term::Var("x".to_string())),
                Box::new(Term::Const(Const::Int(1))),
            )),
        )),
        Box::new(Term::Let(
            "g".to_string(),
            Box::new(Term::Lambda(
                "y".to_string(),
                Type::Int,
                Box::new(Term::BinOp(
                    BinOp::Minus,
                    Box::new(Term::Var("y".to_string())),
                    Box::new(Term::Const(Const::Int(1))),
                )),
            )),
            Box::new(Term::App(
                Box::new(Term::Var("f".to_string())),
                Box::new(Term::App(
                    Box::new(Term::Var("g".to_string())),
                    Box::new(Term::Const(Const::Int(10))),
                )),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}
