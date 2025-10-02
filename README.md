# Simply Typed Lambda Calculus

As a learning exercise I wanted to build an environment in which I can mess around with different variants of the lambda calculus. The goal is to build the core of the langauge and then branch off in different directions to add different language features.

The core of the language will just be the STLC + a tiny bit of quality of life (let-expressions, pairs). I might add arbitrary-sized tuples or pattern matching as the final addition to the core before I start exploring the different type systems I want to mess around with.

## Syntax

```
T     := unit | int | bool
τ     := τ -> τ | (τ, τ) | T

M, N  := x
      | let x = M in N
      | fn x:τ . M
      | M N
      | c
```
