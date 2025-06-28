module StrMap = Map.Make (String)
module StrSet = Set.Make (String)

let value_or_false = Option.value ~default:false
let println = print_endline
let println_if p s = if p then println s else ()

let union_nuts a b =
  let merger _ _ y =
    println "warning: union_nuts detected a collision";
    Some y
  in
  StrMap.union merger a b
