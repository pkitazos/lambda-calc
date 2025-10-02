## Let-expressions as lambdas

Simple let-expressions are just syntactic sugar for lambda abstraction / application

```
let x = 5 in x + 1
```
is the same as:
```
(λ x .x + 1) 5
```

That means that we don't need our AST to be able to represent let expressions, as the above let-expression could be converted at parse-time into:
```
App(
  Lambda(
    Var("x"),
    App(
      App(
        Var("add"),
        Var("x")
      ),
      Const(
        Int(1)
      )
    )
  ),
  Const(
    Int(5)
  )
)
```

Rather than what our current parser does:
```
Let(
  "x",
  Const(
    Int(5)
  ),
  App(
    App(
      Var("add"),
      Var("x")
    ),
    Const(
      Int(1)
    )
  )
)
```

If we were working with the untyped / pure lambda calculus, that would be a pretty nifty optimisation to make our interpreter even simpler. However, since we're working with the simply typed lambda calculus, our lambda expressions need to also include the type of our variable. So let's try to de-sugar our let-expression again.
```
App(
  Lambda(
    Var("x"),
    <what goes here?>,
    App(
      App(
        Var("add"),
        Var("x")
      ),
      Const(
        Int(1)
      )
    )
  ),
  Const(
    Int(5)
  )
)
```

And there's our problem, we don't know what the type of `x` should be. Of course by looking at our expression hard enough we're able to infer that `x` needs to be an `int`, but our parser is not able to do that.

Our _typechecker_ would be able to figure this out no problem, but typically we don't also perform typechecking during the parsing phase.

```rs
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
        |((v, t1), t2): ((&str, Term), Term)| {
            Term::App(
                Box::new(Term::Lambda(v.to_string(), Typechecker::infer_type(t1), Box::new(t2))),
                //                                      ^ we don't have this yet
                Box::new(t1),
            )
        },
    )
    .parse(input)
}
```

So we have two options:

1) we keep let-expressions as a separate term that our AST knows about all the way through
2) we keep let-expressions as a separate term up until our typechecking phase, at which point we can desugar them into application - abstraction


#### Option 1.

**Pros**
- We don't have 2 separate ASTs to manage.

**Cons**
- When we eventually get to the evaluation phase of our interpreter, we will need a separate clause for evaluating let-expressions

#### Option 2.

**Pros**
- Our evaluation phase becomes simpler, all we need to know how to do is handle abstraction and evaluation

**Cons**
- We have desugaring functions sprinkled all over the place.
