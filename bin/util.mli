module StrMap : module type of Map.Make (String)
module StrSet : module type of Set.Make (String)

val value_or_false : bool option -> bool
val println : string -> unit
val println_if : bool -> string -> unit
val union_nuts : 'a StrMap.t -> 'a StrMap.t -> 'a StrMap.t
