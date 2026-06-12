use crate::Term;
use std::{collections::HashSet, fmt, rc::Rc};

#[derive(Clone, Debug)]
pub enum Env {
    Empty,
    Cons(String, Closure, Rc<Env>),
}

impl Env {
    fn new() -> Rc<Self> {
        Rc::new(Env::Empty)
    }

    fn push(env: &Rc<Env>, name: String, val: Closure) -> Rc<Env> {
        Rc::new(Env::Cons(name, val, Rc::clone(env)))
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Env::Empty => write!(f, ""),
            Env::Cons(v, m, env) => write!(f, "{}: {}, {}", v, m, env),
        }
    }
}

pub fn env_from_defs(defs: &Vec<(String, Term)>) -> Result<Rc<Env>, String> {
    let mut env = Env::new();

    for (id, expr) in defs {
        let value = eval(expr.clone(), &env)?;
        env = Env::push(&env, id.to_owned(), value);
    }

    Ok(env)
}

#[derive(Clone, Debug)]
pub struct Closure {
    arg: String,
    body: Box<Term>,
    env: Rc<Env>,
}

impl fmt::Display for Closure {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, " λ{}.{} Env{{{}}}", self.arg, self.body, self.env)
    }
}

pub fn eval(term: Term, env: &Rc<Env>) -> Result<Closure, String> {
    match term {
        Term::Var(v) => find(&v, env),

        Term::Lambda(v, m) => Ok(Closure {
            arg: v,
            body: m,
            env: Rc::clone(env),
        }),

        Term::App(m, n) => {
            let arg_value = eval(*n, env)?;
            let new_m = eval(*m, env)?;
            let new_env = Env::push(&new_m.env, new_m.arg, arg_value);
            eval(*new_m.body, &new_env)
        }
    }
}

fn find(v: &str, env: &Rc<Env>) -> Result<Closure, String> {
    match env.as_ref() {
        Env::Empty => Err(format!(
            "runtime error: variable '{}' not found in environment",
            v
        )),
        Env::Cons(name, val, rest) => {
            if name == v {
                Ok(val.clone())
            } else {
                find(v, &rest)
            }
        }
    }
}

fn step(term: &Term) -> Option<Term> {
    match term.clone() {
        Term::Var(_) => None,

        Term::Lambda(_, _) => None,

        Term::App(m, n) => match (*m.to_owned(), *n.to_owned()) {
            // if we are trying apply an application to something:
            //      _ (M N)
            // then we gotta step through the application first
            (_, Term::App(_, _)) => step(&n).map(|new_n| Term::App(m, Box::new(new_n))),

            // if we are trying to apply something to a variable:
            //      x _
            // then we cannot reduce any further
            (Term::Var(_), _) => None,

            // if we are trying to apply some term to a lambda term
            //      (λx. x) z
            //      (λx. x) (λy. y)
            // then we need to substitute our term n into the lambda term's body
            (Term::Lambda(arg, body), _) => Some(substitute(&body, &arg, &n)),

            // if we have an application on the left (but not the right):
            //      (M N) x
            //      (M N) (λx. x)
            // then we step through the lhs application
            (Term::App(_, _), _) => step(&m).map(|new_m| Term::App(Box::new(new_m), n)),
        },
    }
}

fn free_vars(term: &Term) -> HashSet<String> {
    match term {
        Term::Var(x) => HashSet::from([x.clone()]),
        Term::App(m, n) => {
            let s1 = free_vars(&m);
            let s2 = free_vars(&n);
            s1.union(&s2).cloned().collect()
        }
        Term::Lambda(x, body) => {
            let mut s = free_vars(&body);
            s.remove(x);
            s
        }
    }
}

fn substitute(term: &Term, var: &str, replacement: &Term) -> Term {
    match term {
        Term::Var(x) => {
            if x == var {
                replacement.to_owned()
            } else {
                Term::Var(x.to_owned())
            }
        }

        Term::App(m, n) => {
            let m = substitute(&m, var, replacement);
            let n = substitute(&n, var, replacement);
            Term::App(Box::new(m), Box::new(n))
        }

        Term::Lambda(x, body) => {
            if x == var {
                // shadowing, return unchanged
                Term::Lambda(x.to_owned(), body.to_owned())
            } else if free_vars(replacement).contains(x) {
                // capture risk, rename then substitute

                let free_in_body = free_vars(&body);
                let free_in_replacement = free_vars(&replacement);
                let mut avoid_set = free_in_body
                    .union(&free_in_replacement)
                    .cloned()
                    .collect::<HashSet<String>>();
                avoid_set.insert(var.to_string());

                let x_fresh = fresh_name(&avoid_set, x);
                let renamed_body = substitute(body, x, &Term::Var(x_fresh.to_owned()));
                let substituted_body = substitute(&renamed_body, var, replacement);
                Term::Lambda(x_fresh, Box::new(substituted_body))
            } else {
                // safe to substitute directly
                let new_body = substitute(body, var, replacement);
                Term::Lambda(x.to_owned(), Box::new(new_body))
            }
        }
    }
}

#[derive(PartialEq, Eq)]
struct InternalVar {
    label: String,
    count: i32,
}

impl InternalVar {
    fn reset_count(&mut self) {
        self.count = 1;
    }

    fn incr(&mut self) {
        self.count += 1;
    }
}

fn fresh_name(avoid: &HashSet<String>, seed: &str) -> String {
    let mut fresh_name = to_internal_var(seed);
    fresh_name.reset_count();

    let avoid_names: Vec<InternalVar> = avoid
        .into_iter()
        .map(|x| to_internal_var(x.as_str()))
        .collect();

    while avoid_names.contains(&fresh_name) {
        fresh_name.incr();
    }

    from_internal_var(fresh_name)
}

fn to_internal_var(var: &str) -> InternalVar {
    // we iterate from the end so we can peel off a trailing numeric suffix.
    let mut rev = var.chars().rev();

    // grab all trailing digits (now at the front of the reversed iterator).
    let digits_rev: String = rev.by_ref().take_while(|c| c.is_ascii_digit()).collect();
    // whatever is left is the non-numeric prefix (still reversed).
    let rest_rev: String = rev.collect();

    // reverse back to get the original label.
    let label: String = rest_rev.chars().rev().collect();
    // if there was no numeric suffix, default to 0; otherwise parse it.
    let count: i32 = if digits_rev.is_empty() {
        0
    } else {
        digits_rev
            .chars()
            .rev()
            .collect::<String>()
            .parse()
            .unwrap()
    };

    InternalVar { label, count }
}

fn from_internal_var(var: InternalVar) -> String {
    format!("{}{}", var.label, var.count)
}

// let's say we have the λ-term:
//      f = λy. x
//
//      var         = x
//      replacement = y
//
// and we try to substitute var for replacement
//
//      free_vars(replacement)  = {y}
//      free_vars(f#body)       = {x}
//
//      avoid_set               = {x, y}
//
//      fresh_name(f#arg)       = y1
//
//      body'                   = sub(f#body, f#arg, y1)
//                              = x -- f#arg does not appear in f#body
//
//      body''                  = sub(body', var, replacement)
//                              = y
//
//      f'#arg  = y1
//      f'#body = body''
//
//      f       -> f'
//      λy. x   -> λy1. y
//
