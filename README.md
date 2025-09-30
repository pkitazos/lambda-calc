# Simply Typed Lambda Calculus

## syntax

```
T     := unit | int | bool
τ     := τ -> τ | (τ, τ) | T

M, N  := x
      | let x = M in N
      | fn x:τ . M
      | M N
      | c
```

## next up:

- Let expressions
- Pairs (product types)
