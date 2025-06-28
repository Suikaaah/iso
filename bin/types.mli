open Util

type base_type =
  | Unit
  | Sum of base_type * base_type
  | Product of base_type * base_type
  | Inductive of { x : string; a : base_type }
  | Variable of string

type iso_type = Pair of base_type * base_type | Arrow of iso_type * iso_type

type value =
  | Unit
  | Variable of string
  | InjLeft of value
  | InjRight of value
  | Pair of value * value
  | Fold of value

type pattern = Variable of string | Pair of pattern * pattern

type expr =
  | Value of value
  | Let of {
      p_1 : pattern;
      omega : iso;
      p_2 : pattern;
      e : expr;
      a : base_type;
      products : base_type;
    }

and pairs = (value * expr) list

and iso =
  | Pairs of pairs
  | Fix of { phi : string; omega : iso }
  | Lambda of { psi : string; omega : iso }
  | Variable of string
  | App of { omega_1 : iso; omega_2 : iso; t_1 : iso_type }
  | Invert of iso

type term =
  | Unit
  | Variable of string
  | InjLeft of term
  | InjRight of term
  | Pair of term * term
  | Fold of term
  | App of { omega : iso; t : term; a : base_type }
  | Let of { p : pattern; t_1 : term; t_2 : term; products : base_type }

type program = { ts : (string * base_type) list; t : term; a : base_type }

val pp_base_type : out_channel -> base_type -> unit
val pp_iso_type : out_channel -> iso_type -> unit
val pp_value : out_channel -> value -> unit
val pp_pattern : out_channel -> pattern -> unit
val pp_expr : out_channel -> expr -> unit
val pp_iso : out_channel -> iso -> unit
val pp_term : out_channel -> term -> unit
val term_of_value : value -> term
val term_of_pattern : pattern -> term
val term_of_expr : expr -> term
val value_of_term : term -> value option
val invert : iso -> iso
val are_orthogonal : term -> term -> bool
val extract_value : expr -> value
val free_vars_base_type : base_type -> StrSet.t
val free_vars_expr : expr -> StrSet.t
val free_vars_iso : iso -> StrSet.t
val subst_base_type : what:base_type -> src:string -> dst:base_type -> base_type
val subst_term : what:term -> src:string -> dst:term -> term
val subst_iso_in_expr : what:expr -> src:string -> dst:iso -> expr
val subst_iso : what:iso -> src:string -> dst:iso -> iso

val subst_base_type_in_iso_type :
  what:iso_type -> src:string -> dst:base_type -> iso_type

val subst_base_type_in_expr : what:expr -> src:string -> dst:base_type -> expr
val subst_base_type_in_iso : what:iso -> src:string -> dst:base_type -> iso
val subst_base_type_in_term : what:term -> src:string -> dst:base_type -> term
val unify_value : value -> value -> value StrMap.t option
val unify_pattern : pattern -> value -> value StrMap.t option
val associate : pattern -> base_type -> base_type StrMap.t option
val build_delta : value -> base_type -> base_type StrMap.t
val sigma : (value * expr) list -> value -> term option

(* these definitions break the syntax hightlighting lmao *)
val ( let* ) : 'a option -> ('a -> 'b option) -> 'b option
val ( let+ ) : 'a option -> ('a -> 'b) -> 'b option
