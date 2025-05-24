open Printf

let ( let* ) = Option.bind
let ( let+ ) x f = Option.map f x

type base_type =
  | Unit
  | Sum of base_type * base_type
  | Product of base_type * base_type
  | Inductive of { x : string; a : base_type }
  | Variable of string

let rec pp_base_type out = function
  | Unit -> fprintf out "1"
  | Sum (a, b) -> fprintf out "(%a + %a)" pp_base_type a pp_base_type b
  | Product (a, b) -> fprintf out "(%a * %a)" pp_base_type a pp_base_type b
  | Inductive { x; a } -> fprintf out "(μ%s. %a)" x pp_base_type a
  | Variable x -> fprintf out "%s" x

type iso_type = Pair of base_type * base_type | Arrow of iso_type * iso_type

let rec pp_iso_type out = function
  | Pair (a, b) -> fprintf out "(%a <-> %a)" pp_base_type a pp_base_type b
  | Arrow (t_1, t_2) -> fprintf out "(%a -> %a)" pp_iso_type t_1 pp_iso_type t_2

type value =
  | Unit
  | Variable of string
  | InjLeft of value
  | InjRight of value
  | Pair of value * value
  | Fold of value

let rec pp_value out = function
  | Unit -> fprintf out "()"
  | Variable x -> fprintf out "%s" x
  | InjLeft v -> fprintf out "injl %a" pp_value v
  | InjRight v -> fprintf out "injr %a" pp_value v
  | Pair (v_1, v_2) -> fprintf out "(%a, %a)" pp_value v_1 pp_value v_2
  | Fold v -> fprintf out "fold %a" pp_value v

type pattern = Variable of string | Pair of pattern * pattern

let rec pp_pattern out = function
  | Variable x -> fprintf out "%s" x
  | Pair (p_1, p_2) -> fprintf out "(%a, %a)" pp_pattern p_1 pp_pattern p_2

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

let rec pp_expr out = function
  | Value v -> fprintf out "%a" pp_value v
  | Let { p_1; omega; p_2; e; _ } ->
      fprintf out "let %a = %a %a in\n%a" pp_pattern p_1 pp_iso omega pp_pattern
        p_2 pp_expr e

and pp_iso out = function
  | Pairs p ->
      let pp_pair out (v, e) =
        fprintf out "\n| %a <-> %a" pp_value v pp_expr e
      in
      let pp_pairs out pairs = List.iter (pp_pair out) pairs in
      fprintf out "iso%a\nend" pp_pairs p
  | Fix { phi; omega } -> fprintf out "(fix %s. %a)" phi pp_iso omega
  | Lambda { psi; omega } -> fprintf out "(λ%s. %a)" psi pp_iso omega
  | Variable phi -> fprintf out "%s" phi
  | App { omega_1; omega_2; _ } ->
      fprintf out "(%a %a)" pp_iso omega_1 pp_iso omega_2
  | Invert omega -> fprintf out "(invert %a)" pp_iso omega

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

let rec pp_term out = function
  | Unit -> fprintf out "()"
  | Variable x -> fprintf out "%s" x
  | InjLeft t -> fprintf out "injl %a" pp_term t
  | InjRight t -> fprintf out "injr %a" pp_term t
  | Pair (t_1, t_2) -> fprintf out "(%a, %a)" pp_term t_1 pp_term t_2
  | Fold t -> fprintf out "fold %a" pp_term t
  | App { omega; t; _ } -> fprintf out "(%a %a)" pp_iso omega pp_term t
  | Let { p; t_1; t_2; _ } ->
      fprintf out "let %a = %a in\n%a" pp_pattern p pp_term t_1 pp_term t_2

let rec term_of_value : value -> term = function
  | Unit -> Unit
  | Variable x -> Variable x
  | InjLeft v -> InjLeft (term_of_value v)
  | InjRight v -> InjRight (term_of_value v)
  | Pair (v_1, v_2) -> Pair (term_of_value v_1, term_of_value v_2)
  | Fold v -> Fold (term_of_value v)

let rec term_of_pattern : pattern -> term = function
  | Variable x -> Variable x
  | Pair (p_1, p_2) -> Pair (term_of_pattern p_1, term_of_pattern p_2)

let rec term_of_expr = function
  | Value v -> term_of_value v
  | Let { p_1; omega; p_2; e; a; products } ->
      Let
        {
          p = p_1;
          t_1 = App { omega; t = term_of_pattern p_2; a };
          t_2 = term_of_expr e;
          products;
        }

let rec value_of_term : term -> value option =
  let open Option in
  function
  | Unit -> Some Unit
  | Variable x -> Some (Variable x)
  | InjLeft t ->
      let+ v = value_of_term t in
      (InjLeft v : value)
  | InjRight t ->
      let+ v = value_of_term t in
      (InjRight v : value)
  | Pair (t_1, t_2) ->
      let* v_1 = value_of_term t_1 in
      let+ v_2 = value_of_term t_2 in
      (Pair (v_1, v_2) : value)
  | Fold t ->
      let+ v = value_of_term t in
      (Fold v : value)
  | App _ | Let _ -> None

let rec invert =
  let rec invert_expr e acc =
    match e with
    | Value v -> (v, acc)
    | Let ({ p_1; omega; p_2; e; _ } as l) ->
        invert_expr e
          (Let { l with p_1 = p_2; omega = invert omega; p_2 = p_1; e = acc }
            : expr)
  in
  let invert_pair (v, e) = invert_expr e (Value v) in
  function
  | Pairs p -> Pairs (List.map invert_pair p)
  | Fix { phi; omega } -> Fix { phi; omega = invert omega }
  | Lambda { psi; omega } -> Lambda { psi; omega = invert omega }
  | Variable phi -> Variable phi
  | App { omega_1; omega_2; t_1 } ->
      App { omega_1 = invert omega_1; omega_2 = invert omega_2; t_1 }
  | Invert omega -> invert omega
