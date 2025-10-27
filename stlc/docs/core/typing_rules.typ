#import "@preview/curryst:0.5.1": rule, prooftree

*T-Const*
#prooftree(rule(
  [$Gamma tack c : T $],
  [$c$ has base type $T$],
))

*T-Let*
#prooftree(rule(
  [$Gamma tack$ let $x = M$ in $N : tau_2 $],
  [$Gamma tack M : tau_1$],
  [$Gamma,x:tau_1 tack N : tau_2$],
))

*T-App*
#prooftree(rule(
  [$Gamma tack M N : tau_2$],
  [$Gamma tack M : tau_1 -> tau_2$],
  [$Gamma tack N : tau_1$],
))

*T-Lambda*
#prooftree(rule(
  [$Gamma tack (lambda x : tau_1 . M) : tau_1 -> tau_2 $],
  [$Gamma,x:tau_1 tack M : tau_2$],
))

*T-If*
#prooftree(rule(
  [$Gamma tack$ if $L$ then $M$ else $N : tau $],
  [$Gamma tack L :$ Bool],
  [$Gamma tack M : tau$],
  [$Gamma tack N : tau$],
))

*T-Pair*
#prooftree(rule(
  [$Gamma tack (M, N): tau_1 times tau_2$],
  [$Gamma tack M : tau_1$],
  [$Gamma tack N : tau_2$]
))

*T-Fst*
#prooftree(rule(
  [$Gamma tack$ fst $M : tau_1$],
  [$Gamma tack M: tau_1 times tau_2$],
))

*T-Snd*
#prooftree(rule(
  [$Gamma tack$ snd $M : tau_2$],
  [$Gamma tack M: tau_1 times tau_2$],
))

*T-Arith*
#prooftree(rule(
  [$Gamma tack M diamond.small N :$ Int],
  [$Gamma tack M :$ Int],
  [$Gamma tack N :$ Int],
  [$diamond.small in {+,-}$]
))

*T-Eq*
#prooftree(rule(
  [$Gamma tack M == N :$ Bool],
  [$Gamma tack M :$ T],
  [$Gamma tack N :$ T],
  [$T in {$Int, Bool$}$]
))
