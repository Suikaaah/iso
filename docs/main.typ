#import "@preview/curryst:0.5.1": prooftree, rule
#set text(size: 12pt, font: "New Computer Modern")
#show raw: set text(font: "CaskaydiaMono NF")
#show math.phi: math.phi.alt
#let abox(a, body) = align(a, box(body))
#let btree(body) = box(prooftree(body))
#let tack_sub(sub) = $thick #box($tack$) _sub thick$
#let unit = `unit`
#let bool = `bool`
#let i64 = `i64`
#let tsum = $plus.circle$
#let tprod = $times.circle$
#let rules(body) = align(center)[
  #set par(leading: 1em)
  #show math.equation: set par(leading: 0.25em)
  #body
]

*Grammar*

#{
  set raw(syntaxes: "bnf.sublime-syntax")

  ```bnf
           <program> ::= <type definitions> program = <term>

  <type definitions> ::= <type definition> |
                         <type definition> <type definitions>

   <type definition> ::= (type <type variable> = <constr> | ... | <constr>)

            <constr> ::= <constr variable> | <constr variable> of <type>

              <type> ::= unit | bool | i64 | <type> * ... * <type> |
                         <type variable>

           <literal> ::= () | false | true | -9223372036854775808 | ... |
                         9223372036854775807

             <value> ::= <literal> | <variable> | (<value>, ..., <value>)

           <pattern> ::= <variable> | (<pattern>, ..., <pattern>)

              <expr> ::= <value> | let <pattern> = <iso> <pattern> in <expr>

               <iso> ::= add | sub | negate |
                         (iso <value> <-> <expr> | ... | <value> <-> <expr>) |
                         fun <iso variable> -> <iso> | <iso variable> |
                         <iso> <iso>

              <term> ::= <literal> | <variable> | (<term>, ..., <term>) |
                         <iso> <term> | let <pattern> = <term> in <term>
  ```
}

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
