# Parser Test Cases

## Constants

```
42
---
Const(Int(42))
```

```
true
---
Const(Bool(true))
```

```
false
---
Const(Bool(false))
```

```
()
---
Const(Unit)
```

## Variables

```
x
---
Var("x")
```

```
foo_bar
---
Var("foo_bar")
```

```
_underscore
---
Var("_underscore")
```

## Pairs

```
(1, 2)
---
Pair(Const(Int(1)), Const(Int(2)))
```

```
(true, false)
---
Pair(Const(Bool(true)), Const(Bool(false)))
```

```
((1, 2), 3)
---
Pair(Pair(Const(Int(1)), Const(Int(2))), Const(Int(3)))
```

```
(x, y)
---
Pair(Var("x"), Var("y"))
```

## Lambda Expressions

```
fn x: int . x
---
Lambda("x", Int, Var("x"))
```

```
fn x: int, y: int . x
---
Lambda("x", Int, Lambda("y", Int, Var("x")))
```

```
fn f: int -> int, x: int . f x
---
Lambda("f", Arrow(Int, Int), Lambda("x", Int, App(Var("f"), Var("x"))))
```

## Pair Destructors

```
fst (1, 2)
---
Fst(Pair(Const(Int(1)), Const(Int(2))))
```

```
snd (x, y)
---
Snd(Pair(Var("x"), Var("y")))
```

```
fst (fst ((1, 2), 3))
---
Fst(Fst(Pair(Pair(Const(Int(1)), Const(Int(2))), Const(Int(3)))))
```

## Function Application

```
f x
---
App(Var("f"), Var("x"))
```

```
f x y
---
App(App(Var("f"), Var("x")), Var("y"))
```

```
f (g x)
---
App(Var("f"), App(Var("g"), Var("x")))
```

```
(fn x: int . x) 5
---
App(Lambda("x", Int, Var("x")), Const(Int(5)))
```

## Let Expressions

```
let x = 5 in x
---
Let("x", Const(Int(5)), Var("x"))
```

```
let f = fn x: int . x in f 10
---
Let("f", Lambda("x", Int, Var("x")), App(Var("f"), Const(Int(10))))
```

```
let x = 1 in let y = 2 in x
---
Let("x", Const(Int(1)), Let("y", Const(Int(2)), Var("x")))
```

## If Expressions

```
if true then 1 else 2
---
If(Const(Bool(true)), Const(Int(1)), Const(Int(2)))
```

```
if x == 5 then x else 0
---
If(BinOp(Eq, Var("x"), Const(Int(5))), Var("x"), Const(Int(0)))
```

```
if (x == 2) then (x + 4) else (x - 2)
---
If(BinOp(Eq, Var("x"), Const(Int(2))), BinOp(Plus, Var("x"), Const(Int(4))), BinOp(Minus, Var("x"), Const(Int(2))))
```

## Binary Operations

```
1 + 2
---
BinOp(Plus, Const(Int(1)), Const(Int(2)))
```

```
x - y
---
BinOp(Minus, Var("x"), Var("y"))
```

```
5 == 5
---
BinOp(Eq, Const(Int(5)), Const(Int(5)))
```

```
x + 1
---
BinOp(Plus, Var("x"), Const(Int(1)))
```

## Complex Expressions

```
let add = fn x: int, y: int . x + y in add 3 5
---
Let("add", Lambda("x", Int, Lambda("y", Int, BinOp(Plus, Var("x"), Var("y")))), App(App(Var("add"), Const(Int(3))), Const(Int(5))))
```

```
(fn x: int . if x == 0 then 1 else x - 1) 10
---
App(Lambda("x", Int, If(BinOp(Eq, Var("x"), Const(Int(0))), Const(Int(1)), BinOp(Minus, Var("x"), Const(Int(1))))), Const(Int(10)))
```

```
let pair = (1, 2) in (fst pair) + (snd pair)
---
Let("pair", Pair(Const(Int(1)), Const(Int(2))), BinOp(Plus, Fst(Var("pair")), Snd(Var("pair"))))
```

```
fn p: (int, int) . (fst p) + (snd p)
---
Lambda("p", Pair(Int, Int), BinOp(Plus, Fst(Var("p")), Snd(Var("p"))))
```

```
if true then fn x: int . x else fn y: int . y + 1
---
If(Const(Bool(true)), Lambda("x", Int, Var("x")), Lambda("y", Int, BinOp(Plus, Var("y"), Const(Int(1)))))
```

```
let f = fn x: int . x + 1 in let g = fn y: int . y - 1 in f (g 10)
---
Let("f", Lambda("x", Int, BinOp(Plus, Var("x"), Const(Int(1)))), Let("g", Lambda("y", Int, BinOp(Minus, Var("y"), Const(Int(1)))), App(Var("f"), App(Var("g"), Const(Int(10))))))
```
