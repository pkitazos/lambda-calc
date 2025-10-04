use std::collections::HashMap;
use stlc::*;

fn empty_env() -> HashMap<String, Type> {
    HashMap::new()
}

fn env_with(bindings: Vec<(&str, Type)>) -> HashMap<String, Type> {
    bindings
        .into_iter()
        .map(|(k, v)| (k.to_string(), v))
        .collect()
}

#[test]
fn test_typecheck_const_int() {
    let term = Term::Const(Const::Int(42));
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_const_bool_true() {
    let term = Term::Const(Const::Bool(true));
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_const_bool_false() {
    let term = Term::Const(Const::Bool(false));
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_const_unit() {
    let term = Term::Const(Const::Unit);
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Unit));
}

#[test]
fn test_typecheck_var_found() {
    let env = env_with(vec![("x", Type::Int)]);
    let term = Term::Var("x".to_string());
    let result = typecheck(&env, &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_var_not_found() {
    let term = Term::Var("x".to_string());
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_pair_int_int() {
    let term = Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(
        result,
        Ok(Type::Pair(Box::new(Type::Int), Box::new(Type::Int)))
    );
}

#[test]
fn test_typecheck_pair_bool_int() {
    let term = Term::Pair(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(5))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(
        result,
        Ok(Type::Pair(Box::new(Type::Bool), Box::new(Type::Int)))
    );
}

#[test]
fn test_typecheck_pair_nested() {
    let term = Term::Pair(
        Box::new(Term::Pair(
            Box::new(Term::Const(Const::Int(1))),
            Box::new(Term::Const(Const::Int(2))),
        )),
        Box::new(Term::Const(Const::Bool(true))),
    );
    let result = typecheck(&empty_env(), &term);
    let expected = Type::Pair(
        Box::new(Type::Pair(Box::new(Type::Int), Box::new(Type::Int))),
        Box::new(Type::Bool),
    );
    assert_eq!(result, Ok(expected));
}

#[test]
fn test_typecheck_lambda_identity() {
    let term = Term::Lambda(
        "x".to_string(),
        Type::Int,
        Box::new(Term::Var("x".to_string())),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(
        result,
        Ok(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)))
    );
}

#[test]
fn test_typecheck_lambda_const_function() {
    let term = Term::Lambda(
        "x".to_string(),
        Type::Int,
        Box::new(Term::Const(Const::Int(42))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(
        result,
        Ok(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)))
    );
}

#[test]
fn test_typecheck_lambda_nested() {
    let term = Term::Lambda(
        "x".to_string(),
        Type::Int,
        Box::new(Term::Lambda(
            "y".to_string(),
            Type::Bool,
            Box::new(Term::Var("x".to_string())),
        )),
    );
    let result = typecheck(&empty_env(), &term);
    let expected = Type::Arrow(
        Box::new(Type::Int),
        Box::new(Type::Arrow(Box::new(Type::Bool), Box::new(Type::Int))),
    );
    assert_eq!(result, Ok(expected));
}

#[test]
fn test_typecheck_fst_pair() {
    let term = Term::Fst(Box::new(Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Bool(true))),
    )));
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_snd_pair() {
    let term = Term::Snd(Box::new(Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Bool(true))),
    )));
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_fst_non_pair() {
    let term = Term::Fst(Box::new(Term::Const(Const::Int(5))));
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_snd_non_pair() {
    let term = Term::Snd(Box::new(Term::Const(Const::Bool(true))));
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_app_identity() {
    let term = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Const(Const::Int(5))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_app_wrong_type() {
    let term = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Const(Const::Bool(true))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_app_non_function() {
    let term = Term::App(
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Int(10))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_app_curried() {
    let term = Term::App(
        Box::new(Term::App(
            Box::new(Term::Lambda(
                "x".to_string(),
                Type::Int,
                Box::new(Term::Lambda(
                    "y".to_string(),
                    Type::Int,
                    Box::new(Term::Var("x".to_string())),
                )),
            )),
            Box::new(Term::Const(Const::Int(5))),
        )),
        Box::new(Term::Const(Const::Int(10))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_let_simple() {
    let term = Term::Let(
        "x".to_string(),
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Var("x".to_string())),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_let_lambda() {
    let term = Term::Let(
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
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_let_shadowing() {
    let term = Term::Let(
        "x".to_string(),
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Let(
            "x".to_string(),
            Box::new(Term::Const(Const::Bool(true))),
            Box::new(Term::Var("x".to_string())),
        )),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_if_simple() {
    let term = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_if_non_bool_condition() {
    let term = Term::If(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_if_branch_type_mismatch() {
    let term = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Bool(false))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_if_with_lambda() {
    let term = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Lambda(
            "y".to_string(),
            Type::Int,
            Box::new(Term::Var("y".to_string())),
        )),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(
        result,
        Ok(Type::Arrow(Box::new(Type::Int), Box::new(Type::Int)))
    );
}

#[test]
fn test_typecheck_binop_plus() {
    let term = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_binop_minus() {
    let term = Term::BinOp(
        BinOp::Minus,
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Int(3))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_binop_plus_wrong_lhs_type() {
    let term = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_binop_plus_wrong_rhs_type() {
    let term = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Bool(false))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_binop_eq_int() {
    let term = Term::BinOp(
        BinOp::Eq,
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Int(5))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_binop_eq_bool() {
    let term = Term::BinOp(
        BinOp::Eq,
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Bool(false))),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Bool));
}

#[test]
fn test_typecheck_binop_eq_type_mismatch() {
    let term = Term::BinOp(
        BinOp::Eq,
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Bool(true))),
    );
    let result = typecheck(&empty_env(), &term);
    assert!(result.is_err());
}

#[test]
fn test_typecheck_complex_let_add() {
    let term = Term::Let(
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
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_complex_lambda_if() {
    let term = Term::App(
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
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_complex_pair_operations() {
    let term = Term::Let(
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
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_complex_nested_let() {
    let term = Term::Let(
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
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}

#[test]
fn test_typecheck_higher_order_function() {
    let term = Term::Let(
        "apply".to_string(),
        Box::new(Term::Lambda(
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
        )),
        Box::new(Term::App(
            Box::new(Term::App(
                Box::new(Term::Var("apply".to_string())),
                Box::new(Term::Lambda(
                    "y".to_string(),
                    Type::Int,
                    Box::new(Term::BinOp(
                        BinOp::Plus,
                        Box::new(Term::Var("y".to_string())),
                        Box::new(Term::Const(Const::Int(1))),
                    )),
                )),
            )),
            Box::new(Term::Const(Const::Int(5))),
        )),
    );
    let result = typecheck(&empty_env(), &term);
    assert_eq!(result, Ok(Type::Int));
}
