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


## Types

### Pairs

```
Γ ⊢ M : τ_1     Γ ⊢ N : τ_2
---------------------------
  Γ ⊢ (M, N) : (τ_1, τ_2)


  Γ ⊢ M : (τ_1, τ_2)
---------------------------
  Γ ⊢ fst M : τ_1

  Γ ⊢ M : (τ_1, τ_2)
---------------------------
  Γ ⊢ snd M : τ_2
```
