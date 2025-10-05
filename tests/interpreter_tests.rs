use std::rc::Rc;
use stlc::interpreter::{empty_env, eval};
use stlc::{BinOp, Const, Term, Type};

#[test]
fn test_eval_const() {
    let term = Term::Const(Const::Int(42));
    let result = eval(term.clone(), &empty_env());
    assert_eq!(result, term);
}

#[test]
fn test_eval_var() {
    let env = Rc::new(stlc::interpreter::Env::Cons(
        "x".to_string(),
        Term::Const(Const::Int(10)),
        empty_env(),
    ));
    let term = Term::Var("x".to_string());
    let result = eval(term, &env);
    assert_eq!(result, Term::Const(Const::Int(10)));
}

#[test]
#[should_panic(expected = "runtime error: variable 'y' not found in environment")]
fn test_eval_var_not_found() {
    let term = Term::Var("y".to_string());
    eval(term, &empty_env());
}

#[test]
fn test_eval_pair() {
    let term = Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(
        result,
        Term::Pair(
            Box::new(Term::Const(Const::Int(1))),
            Box::new(Term::Const(Const::Int(2)))
        )
    );
}

#[test]
fn test_eval_fst() {
    let term = Term::Fst(Box::new(Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    )));
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(1)));
}

#[test]
fn test_eval_snd() {
    let term = Term::Snd(Box::new(Term::Pair(
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    )));
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(2)));
}

#[test]
fn test_eval_app() {
    let term = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Type::Int,
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Const(Const::Int(5))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(5)));
}

#[test]
fn test_eval_let() {
    let term = Term::Let(
        "x".to_string(),
        Box::new(Term::Const(Const::Int(10))),
        Box::new(Term::Var("x".to_string())),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(10)));
}

#[test]
fn test_eval_if() {
    let term = Term::If(
        Box::new(Term::Const(Const::Bool(true))),
        Box::new(Term::Const(Const::Int(1))),
        Box::new(Term::Const(Const::Int(2))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(1)));
}

#[test]
fn test_eval_binop_eq() {
    let term = Term::BinOp(
        BinOp::Eq,
        Box::new(Term::Const(Const::Int(3))),
        Box::new(Term::Const(Const::Int(3))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Bool(true)));
}

#[test]
fn test_eval_binop_plus() {
    let term = Term::BinOp(
        BinOp::Plus,
        Box::new(Term::Const(Const::Int(2))),
        Box::new(Term::Const(Const::Int(3))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(5)));
}

#[test]
fn test_eval_binop_minus() {
    let term = Term::BinOp(
        BinOp::Minus,
        Box::new(Term::Const(Const::Int(5))),
        Box::new(Term::Const(Const::Int(3))),
    );
    let result = eval(term, &empty_env());
    assert_eq!(result, Term::Const(Const::Int(2)));
}
