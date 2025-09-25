type base_type =
  | Unit
  | Sum of base_type list
  | Product of base_type list
  | Inductive of { x : string; a : base_type }
  | Variable of string

type iso_type = Pair of base_type * base_type | Arrow of iso_type * iso_type

type value =
  | Unit
  | Variable of string
  | Constructed of { c : string; v : value }
  | Tuple of value list

type pattern = Variable of string | Tuple of pattern list

type expr =
  | Value of value
  | Let of { p_1 : pattern; omega : iso; p_2 : pattern; e : expr }

and pairs = (value * expr) list

and iso =
  | Pairs of pairs
  | Fix of { phi : string; omega : iso }
  | Lambda of { psi : string; omega : iso }
  | Variable of string
  | App of { omega_1 : iso; omega_2 : iso }
  | Invert of iso

type term =
  | Unit
  | Variable of string
  | Tuple of term list
  | App of { omega : iso; t : term }
  | Let of { p : pattern; t_1 : term; t_2 : term }

type program = { ts : (string * (string * base_type) list) list; t : term }
