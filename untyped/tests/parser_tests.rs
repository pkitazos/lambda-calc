use ulc::*;

// ============================================================================
// Basic Variable Tests
// ============================================================================

#[test]
fn test_var_single() {
    let input = "x";
    let expected = Term::Var("x".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_multi_char() {
    let input = "foo";
    let expected = Term::Var("foo".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_with_underscore() {
    let input = "foo_bar";
    let expected = Term::Var("foo_bar".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_var_starting_with_underscore() {
    let input = "_x";
    let expected = Term::Var("_x".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

// ============================================================================
// Lambda Abstraction Tests - Single Argument
// ============================================================================

#[test]
fn test_lambda_identity() {
    let input = "λx. x";
    let expected = Term::Lambda("x".to_string(), Box::new(Term::Var("x".to_string())));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_const() {
    let input = "λx. y";
    let expected = Term::Lambda("x".to_string(), Box::new(Term::Var("y".to_string())));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_nested_manual() {
    let input = "λx. λy. x";
    let expected = Term::Lambda(
        "x".to_string(),
        Box::new(Term::Lambda(
            "y".to_string(),
            Box::new(Term::Var("x".to_string())),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_three_nested() {
    let input = "λx. λy. λz. z";
    let expected = Term::Lambda(
        "x".to_string(),
        Box::new(Term::Lambda(
            "y".to_string(),
            Box::new(Term::Lambda(
                "z".to_string(),
                Box::new(Term::Var("z".to_string())),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_lambda_with_app_body() {
    let input = "λf. λx. f x";
    let expected = Term::Lambda(
        "f".to_string(),
        Box::new(Term::Lambda(
            "x".to_string(),
            Box::new(Term::App(
                Box::new(Term::Var("f".to_string())),
                Box::new(Term::Var("x".to_string())),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

// ============================================================================
// Application Tests
// ============================================================================

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
fn test_app_left_associative() {
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
fn test_app_three_terms() {
    let input = "a b c d";
    let expected = Term::App(
        Box::new(Term::App(
            Box::new(Term::App(
                Box::new(Term::Var("a".to_string())),
                Box::new(Term::Var("b".to_string())),
            )),
            Box::new(Term::Var("c".to_string())),
        )),
        Box::new(Term::Var("d".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_with_parens() {
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
fn test_app_nested_right() {
    let input = "f (g (h x))";
    let expected = Term::App(
        Box::new(Term::Var("f".to_string())),
        Box::new(Term::App(
            Box::new(Term::Var("g".to_string())),
            Box::new(Term::App(
                Box::new(Term::Var("h".to_string())),
                Box::new(Term::Var("x".to_string())),
            )),
        )),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_app_lambda() {
    let input = "(λx. x) y";
    let expected = Term::App(
        Box::new(Term::Lambda(
            "x".to_string(),
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Var("y".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

// ============================================================================
// Parentheses Tests
// ============================================================================

#[test]
fn test_paren_var() {
    let input = "(x)";
    let expected = Term::Var("x".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_paren_lambda() {
    let input = "(λx. x)";
    let expected = Term::Lambda("x".to_string(), Box::new(Term::Var("x".to_string())));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_paren_nested() {
    let input = "((x))";
    let expected = Term::Var("x".to_string());
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_paren_app() {
    let input = "(f x)";
    let expected = Term::App(
        Box::new(Term::Var("f".to_string())),
        Box::new(Term::Var("x".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

// ============================================================================
// Whitespace Tests
// ============================================================================

#[test]
fn test_whitespace_around_lambda() {
    let input = "λx . x";
    let expected = Term::Lambda("x".to_string(), Box::new(Term::Var("x".to_string())));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_whitespace_minimal() {
    let input = "λx.x";
    let expected = Term::Lambda("x".to_string(), Box::new(Term::Var("x".to_string())));
    assert_eq!(parser::parse(input), Ok(("", expected)));
}

#[test]
fn test_whitespace_in_app() {
    let input = "f   x   y";
    let expected = Term::App(
        Box::new(Term::App(
            Box::new(Term::Var("f".to_string())),
            Box::new(Term::Var("x".to_string())),
        )),
        Box::new(Term::Var("y".to_string())),
    );
    assert_eq!(parser::parse(input), Ok(("", expected)));
}
