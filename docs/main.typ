#import "@preview/curryst:0.5.1": prooftree, rule

#set text(size: 12pt, font: "New Computer Modern")

#show math.phi: math.phi.alt

#let unit = $mono("unit")$
#let bool = $mono("bool")$
#let i64 = $mono("i64")$
#let type = $mono("type")$
#let of = $mono("of")$
#let mylet = $mono("let")$
#let myin = $mono("in")$
#let ctor = $c_"dec"$
#let carm = $c_"arm"$
#let tsum = $plus.circle$
#let tprod = $times.circle$
#let btree(body) = box(prooftree(body))
#let tack_sub(sub) = $thick #box($tack$) _sub thick$
#let rules(body) = align(center)[
  #set par(leading: 1em)
  #show math.equation: set par(leading: 0.25em)
  #body
]

*Grammar*

$
  & "(Constructor arm)"         && wide & carm ::= & c | c of A                           \
  & "(Constructor declaration)" &&      & ctor ::= & (type X = carm | ... | carm)         \
  & "(Base types)"              &&      & A, B ::= & unit
                                                     | A_1 tsum ... tsum A_n
                                                     | A_1 tprod ... tprod A_n
                                                     | mu X . A
                                                     | X                                  \
  & "(Isos)"                    &&      &    T ::= & A <-> B
                                                     | T_1 -> T_2                         \
  & "(Values)"                  &&      &    v ::= & ()
                                                     | x
                                                     | c thick v
                                                     | (v_1, ..., v_n)                    \
  & "(Patterns)"                &&      &    p ::= & x
                                                     | (p_1, ..., p_n)                    \
  & "(Expressions)"             &&      &    e ::= & v
                                                     | mylet p_1 = omega thick p_2 myin e \
$

\

*Typing Rules - Terms*

#rules[
  #btree(rule($Psi; emptyset tack (): unit$))
  #h(1em)
  #btree(rule($Psi; x: A tack x: A$))
  #h(1em)
  #btree(rule(
    $Psi; Delta tack (t_1, ..., t_n): A_1 tprod ... tprod A_n$,
    $Psi; Delta_1 tack t_1: A_1$,
    $...$,
    $Psi; Delta_n tack t_n: A_n$,
  ))
  #btree(rule(
    $Psi; Delta tack omega thick t: B$,
    $Psi #tack_sub($omega$) omega: A <-> B$,
    $Psi; Delta tack t: A$,
  ))
  #btree(rule(
    $Psi; Delta_1, Delta_2 tack #`let` (x_1, .., x_n) = t_1 #`in` t_2: B$,
    $Psi; Delta_1 tack t_1: A_1 tprod ... tprod A_n$,
    $Psi; Delta_2 tack x_1: A_1, ..., x_n: A_n tack t_2: B$,
  ))
]

\

*Typing Rules - Isos*

#rules[
  #btree(rule($Psi; phi: T tack (): unit$))
  #h(1em)
  #btree(rule($Psi; x: A tack x: A$))
]
