open Printf
open Types
module StrMap = Map.Make (String)
module StrSet = Set.Make (String)

let value_or_false = Option.value ~default:false
let println = print_endline
let println_if p s = if p then println s else ()

let union_nuts a b =
  let merger _ _ y =
    println "union_nuts detected a collision";
    Some y
  in
  StrMap.union merger a b

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
      List.map (fun (_, e) -> free_vars_expr e) p |> List.fold_left union empty
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
      println_if invalid "a variable has been bound incorrectly";
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
  match what with
  | Pairs p ->
      let f (v, e) = (v, subst_iso_in_expr ~what:e ~src ~dst) in
      Pairs (List.map f p)
  | Fix { phi; omega } when phi <> src ->
      let invalid = StrSet.exists (( = ) phi) (free_vars_iso omega) in
      println_if invalid "a variable has been bound incorrectly";
      Fix { phi; omega = subst omega }
  | Lambda { psi; omega } when psi <> src ->
      let invalid = StrSet.exists (( = ) psi) (free_vars_iso omega) in
      println_if invalid "a variable has been bound incorrectly";
      Lambda { psi; omega = subst omega }
  | Variable phi when phi = src -> dst
  | App { omega_1; omega_2; t_1 } ->
      App { omega_1 = subst omega_1; omega_2 = subst omega_2; t_1 }
  | Invert omega -> subst omega
  | Fix _ | Lambda _ | Variable _ -> what

let rec subst_base_type_in_iso_type ~what ~src ~dst =
  let subst what = subst_base_type_in_iso_type ~what ~src ~dst in
  match what with
  | Arrow (t_1, t_2) -> Arrow (subst t_1, subst t_2)
  | Pair (a, b) ->
      Pair (subst_base_type ~what:a ~src ~dst, subst_base_type ~what:b ~src ~dst)

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
  | Invert omega -> subst omega

let rec subst_base_type_in_term ~what ~src ~dst =
  let subst = fun what -> subst_base_type_in_term ~what ~src ~dst in
  match what with
  | Unit -> Unit
  | Variable _ -> what
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

let rec associate (pattern : pattern) products =
  match (pattern, products) with
  | Variable x, a -> Some (StrMap.singleton x a)
  | Pair (p_1, p_2), Product (a, b) ->
      let* l = associate p_1 a in
      let+ r = associate p_2 b in
      union_nuts l r
  | _ -> None

type psi = iso_type StrMap.t
type delta = base_type StrMap.t
type context = { psi : psi; delta : delta }

let empty_context = { psi = StrMap.empty; delta = StrMap.empty }

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

let rec validate_term context term (expected : base_type) =
  match (term, expected) with
  | Unit, Unit -> true
  | Variable x, _ ->
      Option.map (( = ) expected) (StrMap.find_opt x context.delta)
      |> value_or_false
  | InjLeft t, Sum (a, _) -> validate_term context t a
  | InjRight t, Sum (_, b) -> validate_term context t b
  | Pair (t_1, t_2), Product (a, b) ->
      validate_term context t_1 a && validate_term context t_2 b
  | Fold t, (Inductive { x; a } as dst) ->
      validate_term context t (subst_base_type ~what:a ~src:x ~dst)
  | App { omega; t; a }, _ ->
      validate_iso context.psi omega (Pair (a, expected))
      && validate_term context t a
  | Let { p; t_1; t_2; products }, _ ->
      value_or_false
      @@
      let+ associated = associate p products in
      let extended =
        { context with delta = union_nuts context.delta associated }
      in
      validate_term context t_1 products && validate_term extended t_2 expected
  | _ -> false

and validate_iso psi iso (expected : iso_type) =
  match (iso, expected) with
  | Variable phi, _ ->
      Option.map (( = ) expected) (StrMap.find_opt phi psi) |> value_or_false
  | Fix { phi; omega }, _ ->
      validate_iso (StrMap.add phi expected psi) omega expected
  | App { omega_2; omega_1; t_1 }, _ ->
      validate_iso psi omega_1 t_1
      && validate_iso psi omega_2 (Arrow (t_1, expected))
  | Lambda { psi = phi; omega }, Arrow (t_1, t_2) ->
      validate_iso (StrMap.add phi t_1 psi) omega t_2
  | Pairs p, Pair (a, b) ->
      let f (v, e) =
        let delta = build_delta v a in
        let context = { psi; delta } in
        validate_term context (term_of_value v) a
        && validate_term context (term_of_expr e) b
      in
      List.map f p |> List.fold_left ( && ) true
  | Invert omega, Pair (a, b) -> validate_iso psi omega (Pair (b, a))
  | Invert omega, Arrow (t_1, t_2) -> validate_iso psi omega (Arrow (t_2, t_1))
  | _ -> false

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

let rec sigma p v =
  match p with
  | [] -> None
  | (v_i, e_i) :: tl -> begin
      match unify_value v_i v with
      | None -> sigma tl v
      | Some sigma ->
          let f src dst what = subst_term ~what ~src ~dst in
          StrMap.fold f (StrMap.map term_of_value sigma) (term_of_expr e_i)
          |> Option.some
    end

let rec eval_iso iso =
  let rec step = function
    | Fix { phi; omega } as dst -> Some (subst_iso ~what:omega ~src:phi ~dst)
    | App { omega_1 = Lambda { psi = phi; omega = omega_1 }; omega_2; _ } ->
        Some (subst_iso ~what:omega_1 ~src:phi ~dst:omega_2)
    | App ({ omega_1; _ } as app) ->
        let+ omega_1' = step omega_1 in
        (App { app with omega_1 = omega_1' } : iso)
    | Invert omega -> Some (invert omega)
    | _ -> None
  in
  match step iso with Some reduced -> eval_iso reduced | None -> iso

let rec eval_term term =
  let rec step = function
    | App { omega = Pairs p; t; _ } ->
        let* v' = value_of_term (eval_term t) in
        sigma p v'
    | App ({ omega; _ } as app) ->
        App { app with omega = eval_iso omega } |> Option.some
    | Let { p; t_1; t_2; _ } ->
        let* v = value_of_term (eval_term t_1) in
        let+ sigma = unify_pattern p v in
        let f src dst what = subst_term ~what ~src ~dst:(term_of_value dst) in
        StrMap.fold f sigma t_2
    | InjLeft t -> Option.map (fun t -> InjLeft t) (step t)
    | InjRight t -> Option.map (fun t -> InjRight t) (step t)
    | Fold t -> Option.map (fun t -> Fold t) (step t)
    | Pair (t_1, t_2) -> begin
        match step t_1 with
        | Some t_1 -> Some (Pair (t_1, t_2))
        | None ->
            let+ t_2 = step t_2 in
            Pair (t_1, t_2)
      end
    | Unit | Variable _ -> None
  in
  match step term with Some reduced -> eval_term reduced | None -> term

let () =
  let file = open_in "./source.iso" in
  let { ts; t; a } = Parser.program Lexer.token (Lexing.from_channel file) in
  let t =
    List.fold_left
      (fun what (src, dst) -> subst_base_type_in_term ~what ~src ~dst)
      t ts
  in
  let a =
    List.fold_left (fun what (src, dst) -> subst_base_type ~what ~src ~dst) a ts
  in
  let well_typed = validate_term empty_context t a in
  if well_typed then printf "%a\n" pp_term (eval_term t)
  else println "error: ill-typed"
