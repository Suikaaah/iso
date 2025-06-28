open Printf
open Util

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
  | Inductive { x; a } -> fprintf out "(mu %s. %a)" x pp_base_type a
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
  | Lambda { psi; omega } -> fprintf out "(\\%s. %a)" psi pp_iso omega
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

let rec are_orthogonal t_1 t_2 =
  match t_1 with
  | Unit | Variable _ | App _ -> false
  | InjLeft t_1 -> begin
      match t_2 with
      | Unit | Variable _ | App _ -> false
      | InjLeft t_2 | Fold t_2 | Let { t_2; _ } -> are_orthogonal t_1 t_2
      | InjRight _ -> true
      | Pair (t_2_1, t_2_2) ->
          are_orthogonal t_1 t_2_1 || are_orthogonal t_1 t_2_2
    end
  | InjRight t_1 -> begin
      match t_2 with
      | Unit | Variable _ | App _ -> false
      | InjLeft _ -> true
      | InjRight t_2 | Fold t_2 | Let { t_2; _ } -> are_orthogonal t_1 t_2
      | Pair (t_2_1, t_2_2) ->
          are_orthogonal t_1 t_2_1 || are_orthogonal t_1 t_2_2
    end
  | Pair (t_1_1, t_1_2) -> begin
      match t_2 with
      | Unit | Variable _ | App _ -> false
      | InjLeft t_2 | InjRight t_2 | Fold t_2 | Let { t_2; _ } ->
          are_orthogonal t_1_1 t_2 || are_orthogonal t_1_2 t_2
      | Pair (t_2_1, t_2_2) ->
          are_orthogonal t_1_1 t_2_1 || are_orthogonal t_1_1 t_2_2
          || are_orthogonal t_1_2 t_2_1 || are_orthogonal t_1_2 t_2_2
    end
  | Fold t_1 | Let { t_2 = t_1; _ } -> begin
      match t_2 with
      | Unit | Variable _ | App _ -> false
      | InjLeft t_2 | InjRight t_2 | Fold t_2 | Let { t_2; _ } ->
          are_orthogonal t_1 t_2
      | Pair (t_2_1, t_2_2) ->
          are_orthogonal t_1 t_2_1 || are_orthogonal t_1 t_2_2
    end

let rec extract_value = function
  | Value v -> v
  | Let { e; _ } -> extract_value e

let rec free_vars_base_type =
  let open StrSet in
  function
  | Sum (a, b) | Product (a, b) ->
      union (free_vars_base_type a) (free_vars_base_type b)
  | Inductive { x; a } -> filter (( <> ) x) (free_vars_base_type a)
  | Variable x -> singleton x
  | Unit -> empty

let rec free_vars_expr =
  let open StrSet in
  function
  | Value _ -> empty
  | Let { omega; e; _ } -> union (free_vars_iso omega) (free_vars_expr e)

and free_vars_iso =
  let open StrSet in
  function
  | Pairs p ->
      List.fold_left (fun acc (_, e) -> union acc (free_vars_expr e)) empty p
  | Fix { phi; omega } | Lambda { psi = phi; omega } ->
      filter (( <> ) phi) (free_vars_iso omega)
  | Variable phi -> singleton phi
  | App { omega_1; omega_2; _ } ->
      union (free_vars_iso omega_1) (free_vars_iso omega_2)
  | Invert omega -> free_vars_iso omega

let rec subst_base_type ~what ~src ~dst =
  let subst = fun what -> subst_base_type ~what ~src ~dst in
  match what with
  | Sum (a, b) -> Sum (subst a, subst b)
  | Product (a, b) -> Product (subst a, subst b)
  | Inductive { x; a } when x <> src ->
      let invalid = StrSet.exists (( = ) x) (free_vars_base_type dst) in
      println_if invalid "warning: a variable has been bound incorrectly";
      Inductive { x; a = subst a }
  | Variable x when x = src -> dst
  | Unit | Inductive _ | Variable _ -> what

let rec subst_term ~what ~src ~dst =
  let subst = fun what -> subst_term ~what ~src ~dst in
  match what with
  | Variable x when x = src -> dst
  | InjLeft t -> InjLeft (subst t)
  | InjRight t -> InjRight (subst t)
  | Pair (t_1, t_2) -> Pair (subst t_1, subst t_2)
  | Fold t -> Fold (subst t)
  | App ({ t; _ } as app) -> App { app with t = subst t }
  | Let ({ t_1; t_2; _ } as l) ->
      Let { l with t_1 = subst t_1; t_2 = subst t_2 }
  | Unit | Variable _ -> what

let rec subst_iso_in_expr ~what ~src ~dst =
  match what with
  | Value _ -> what
  | Let ({ omega; e; _ } as l) ->
      Let
        {
          l with
          omega = subst_iso ~what:omega ~src ~dst;
          e = subst_iso_in_expr ~what:e ~src ~dst;
        }

and subst_iso ~what ~src ~dst =
  let subst = fun what -> subst_iso ~what ~src ~dst in
  let warn invalid =
    println_if invalid "warning: a variable has been bound incorrectly"
  in
  match what with
  | Pairs p ->
      let f (v, e) = (v, subst_iso_in_expr ~what:e ~src ~dst) in
      Pairs (List.map f p)
  | Fix { phi; omega } when phi <> src ->
      StrSet.exists (( = ) phi) (free_vars_iso omega) |> warn;
      Fix { phi; omega = subst omega }
  | Lambda { psi; omega } when psi <> src ->
      StrSet.exists (( = ) psi) (free_vars_iso omega) |> warn;
      Lambda { psi; omega = subst omega }
  | Variable phi when phi = src -> dst
  | App { omega_1; omega_2; t_1 } ->
      App { omega_1 = subst omega_1; omega_2 = subst omega_2; t_1 }
  | Invert omega -> Invert (subst omega)
  | Fix _ | Lambda _ | Variable _ -> what

let rec subst_base_type_in_iso_type ~what ~src ~dst =
  let subst_i what = subst_base_type_in_iso_type ~what ~src ~dst in
  let subst_b what = subst_base_type ~what ~src ~dst in
  match what with
  | Arrow (t_1, t_2) -> Arrow (subst_i t_1, subst_i t_2)
  | Pair (a, b) -> Pair (subst_b a, subst_b b)

let rec subst_base_type_in_expr ~what ~src ~dst =
  match what with
  | Value _ -> what
  | Let ({ omega; e; a; products; _ } as l) ->
      Let
        {
          l with
          omega = subst_base_type_in_iso ~what:omega ~src ~dst;
          e = subst_base_type_in_expr ~what:e ~src ~dst;
          a = subst_base_type ~what:a ~src ~dst;
          products = subst_base_type ~what:products ~src ~dst;
        }

and subst_base_type_in_iso ~what ~src ~dst =
  let subst_in_pair (v, e) = (v, subst_base_type_in_expr ~what:e ~src ~dst) in
  let subst = fun what -> subst_base_type_in_iso ~what ~src ~dst in
  match what with
  | Variable _ -> what
  | Pairs p -> Pairs (List.map subst_in_pair p)
  | Fix { phi; omega } -> Fix { phi; omega = subst omega }
  | Lambda { psi; omega } -> Lambda { psi; omega = subst omega }
  | App { omega_1; omega_2; t_1 } ->
      App
        {
          omega_1 = subst omega_1;
          omega_2 = subst omega_2;
          t_1 = subst_base_type_in_iso_type ~what:t_1 ~src ~dst;
        }
  | Invert omega -> Invert (subst omega)

let rec subst_base_type_in_term ~what ~src ~dst =
  let subst = fun what -> subst_base_type_in_term ~what ~src ~dst in
  match what with
  | Unit | Variable _ -> what
  | InjLeft t -> InjLeft (subst t)
  | InjRight t -> InjRight (subst t)
  | Pair (t_1, t_2) -> Pair (subst t_1, subst t_2)
  | Fold t -> Fold (subst t)
  | App { omega; t; a } ->
      App
        {
          omega = subst_base_type_in_iso ~what:omega ~src ~dst;
          t = subst t;
          a = subst_base_type ~what:a ~src ~dst;
        }
  | Let { p; t_1; t_2; products } ->
      Let
        {
          p;
          t_1 = subst t_1;
          t_2 = subst t_2;
          products = subst_base_type ~what:products ~src ~dst;
        }

let rec unify_value (v_i : value) (v : value) =
  match (v_i, v) with
  | Variable x, _ -> Some (StrMap.singleton x v)
  | Unit, Unit -> Some StrMap.empty
  | InjLeft v_i, InjLeft v | InjRight v_i, InjRight v | Fold v_i, Fold v ->
      unify_value v_i v
  | Pair (v_i_1, v_i_2), Pair (v_1, v_2) ->
      let* u_1 = unify_value v_i_1 v_1 in
      let+ u_2 = unify_value v_i_2 v_2 in
      union_nuts u_1 u_2
  | _ -> None

let rec unify_pattern (p : pattern) (v : value) =
  match (p, v) with
  | Variable x, _ -> Some (StrMap.singleton x v)
  | Pair (p_1, p_2), Pair (v_1, v_2) ->
      let* u_1 = unify_pattern p_1 v_1 in
      let+ u_2 = unify_pattern p_2 v_2 in
      union_nuts u_1 u_2
  | _ -> None

let rec associate (pattern : pattern) products =
  match (pattern, products) with
  | Variable x, a -> Some (StrMap.singleton x a)
  | Pair (p_1, p_2), Product (a, b) ->
      let* l = associate p_1 a in
      let+ r = associate p_2 b in
      union_nuts l r
  | _ -> None

let rec build_delta (value : value) a =
  let open StrMap in
  match (value, a) with
  | Variable x, _ -> singleton x a
  | InjLeft v, Sum (a, _) -> build_delta v a
  | InjRight v, Sum (_, b) -> build_delta v b
  | Pair (v_1, v_2), Product (a, b) ->
      union_nuts (build_delta v_1 a) (build_delta v_2 b)
  | Fold v, (Inductive { x; a } as dst) ->
      subst_base_type ~what:a ~src:x ~dst |> build_delta v
  | _ -> empty

let rec sigma (pairs : (value * expr) list) (value : value) =
  match pairs with
  | [] -> None
  | (v_i, e_i) :: tl -> begin
      match unify_value v_i value with
      | None -> sigma tl value
      | Some sigma ->
          let f src dst what = subst_term ~what ~src ~dst:(term_of_value dst) in
          StrMap.fold f sigma (term_of_expr e_i) |> Option.some
    end
