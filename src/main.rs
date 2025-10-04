use std::{collections::HashMap, fs::File, io::Read};
use stlc::{BinOp, Const, Term, Type, parse_term};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    match read_code("examples/scratch.lc") {
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

fn typecheck<'a>(env: &'a HashMap<String, Type>, term: &'a Term) -> Result<Type, String> {
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
            typecheck(&temp_env, term)
        }

        Term::Fst(term) => match &**term {
            Term::Pair(lhs, rhs) => {
                if let Ok(Type::Pair(lhs_type, _)) = typecheck_pair(env, &lhs, &rhs) {
                    Ok(*lhs_type)
                } else {
                    Err("unreachable lhs error".to_string())
                }
            }
            _ => Err("can't take first element of non-pair".to_string()),
        },

        Term::Snd(term) => match &**term {
            Term::Pair(lhs, rhs) => {
                if let Ok(Type::Pair(_, rhs_type)) = typecheck_pair(env, &lhs, &rhs) {
                    Ok(*rhs_type)
                } else {
                    Err("unreachable rhs error".to_string())
                }
            }
            _ => Err("can't take second element of non-pair".to_string()),
        },

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

fn typecheck_const(term: Const) -> Type {
    match term {
        Const::Unit => Type::Unit,
        Const::Int(_) => Type::Int,
        Const::Bool(_) => Type::Bool,
    }
}

fn typecheck_pair<'a>(
    env: &'a HashMap<String, Type>,
    lhs: &'a Box<Term>,
    rhs: &'a Box<Term>,
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
