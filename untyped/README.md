# Untyped Lambda Calculus

Here I'm building an interactive interpreter for the untyped lambda calculus because I think it would be cool to have a way to step through the evaluation of expressions.

## Syntax

```
M, N  := x
      | λ x . M
      | M N
```

## next:

- [x] add top level expression aliasing to make life a little bit easier
- [ ] comments could be helpful since code is often totally unreadable
- [ ] finish formatter
- [ ] look into de bruijn indices to be able to print out definition name rather than internal expression in result
