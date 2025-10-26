# Simply Typed Lambda Calculus

As a learning exercise I wanted to build an environment in which I can mess around with different variants of the lambda calculus. The goal is to build the core of the langauge and then branch off in different directions to add different language features.

The core of the language will just be the STLC + a tiny bit of quality of life (let-expressions, if-expressions, pairs). I might add arbitrary-sized tuples and pattern matching as the final addition to the core before I start exploring the different type systems I want to mess around with.

## Syntax

```
C     := unit | int | bool
A, B     := A -> B | A × B | C

M, N  := x
      | M ◇ N
      | if M then N else O
      | (M, N)
      | fst M
      | snd M
      | let x = M in N
      | fn x:A . M
      | M N
      | c

◇    := + | - | ==
```

## next:

- recursion construct / fixpoint operator
- arbitrary-size tuples
- lists
- sum types / variants
- pattern matching

<!-- squarely leaving STLC territory -->
- polymorphism Λα.[α] -> (α -> bool) -> [α]
