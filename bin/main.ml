open Printf
open Types
open Util

type psi = iso_type StrMap.t
type delta = base_type StrMap.t
type context = { psi : psi; delta : delta }

let empty_context = { psi = StrMap.empty; delta = StrMap.empty }

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
      let validate_pair (v, e) =
        let context = { psi; delta = build_delta v a } in
        validate_term context (term_of_value v) a
        && validate_term context (term_of_expr e) b
      in
      (* let rec validate_ortho = function
        | [] -> true
        | (v, e) :: tl ->
            let t_v = term_of_value v in
            let t_e = extract_value e |> term_of_value in
            let ortho_each (v', e') =
              are_orthogonal t_v (term_of_value v')
              && are_orthogonal t_e (term_of_value @@ extract_value e')
            in
            validate_ortho tl && List.for_all ortho_each tl
      in *)
      List.for_all validate_pair p (* && validate_ortho p *)
  | Invert omega, Pair (a, b) -> validate_iso psi omega (Pair (b, a))
  | Invert omega, Arrow (t_1, t_2) -> validate_iso psi omega (Arrow (t_2, t_1))
  | _ -> false

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

let read_program path =
  let file = open_in path in
  let { ts; t; a } = Parser.program Lexer.token (Lexing.from_channel file) in
  let ts = List.rev ts in
  let folder_term what (src, dst) = subst_base_type_in_term ~what ~src ~dst in
  let folder_base_type what (src, dst) = subst_base_type ~what ~src ~dst in
  (List.fold_left folder_term t ts, List.fold_left folder_base_type a ts)

let () =
  let t, a = read_program "source.iso" in
  let well_typed = validate_term empty_context t a in
  if well_typed then printf "output:\n%a\n" pp_term (eval_term t)
  else println "error: ill-typed"
