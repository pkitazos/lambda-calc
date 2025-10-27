# The λ-calculus

As a learning exercise I wanted to build an environment in which I can mess around with different variants of the lambda calculus. The goal is to build the core of the langauge and then branch off in different directions to add different language features.

## Untyped

```
M, N  := x
      | λ x . M
      | M N
```

## Simply Typed

```
C     := unit | int | bool
A, B  := A -> B | C

M, N  := x
      | λ x:A . M
      | M N
      | c
```
