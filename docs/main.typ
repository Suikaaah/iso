#import "@preview/curryst:0.5.1": prooftree, rule
#set text(size: 13pt, font: "New Computer Modern")
#show raw: set text(font: "CaskaydiaMono NF")
#show math.phi: math.phi.alt
#let abox(a, body) = align(a, box(body))
#let btree(body) = box(prooftree(body))
#let tack_sub(sub) = $thick #box($tack$) _sub thick$
#let unit = $bb(1)$
#let tsum = $plus.circle$
#let tprod = $times.circle$

*Grammar*

#abox(left)[
  $
    & "(Types?)"      & thick &&     y ::= & x                                      \
    & "(Base types)"  &       &&     A ::= & unit
                                             | A_1 tsum ... tsum A_n
                                             | A_1 tprod ... tprod A_n
                                             | mu X . A
                                             | X                                    \
    & "(Values)"      &       &&     v ::= & x
                                             | (v_1, ..., v_n)
                                             | c thick v                            \
    & "(Patterns)"    &       &&     p ::= & x
                                             | (p_1, ..., p_n)                      \
    & "(Expressions)" &       &&     e ::= & v
                                             | #`let` p_1 = omega thick p_2 #`in` e \
    & "(Isos)"        &       && omega ::= & { v_1 <-> e_1 | ... | v_n <-> e_n }
                                             | #`fix` phi . omega
                                             | lambda psi . omega
                                             | phi
                                             | omega_1 thick omega_2                \
    & "(Terms)"       &       &&     t ::= & x
                                             | (t_1, ..., t_n)
                                             | c thick t
                                             | omega thick t
                                             | #`let` p = t_1 #`in` t_2             \
  $
]

\

*Typing Rules - Terms*

#align(center)[
  #set par(leading: 1em)
  #show math.equation: set par(leading: 0.25em)

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


