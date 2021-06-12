(* Simple propositional logic formulas *)
type prop =
  | True
  | False
  | Lit of string
  | Neg of prop
  | And of prop * prop
  | Or of prop * prop
  | Imp of prop * prop
  | Iff of prop * prop

(* Updates current interpretation *)
let update env (x, v) y = if x = y then Some v else env y

(* Empty interpretation *)
let empty_env x = None

(* Evaluate propositional equation under an interpretation *)
let rec eval env = function
  | True -> Some true
  | False -> Some false
  | Lit p -> env p
  | Neg p -> ( match eval env p with Some b -> Some (not b) | None -> None )
  | And (p1, p2) -> (
    match (eval env p1, eval env p2) with
    | Some b1, Some b2 -> Some (b1 && b2)
    | _ -> None )
  | Or (p1, p2) -> (
    match (eval env p1, eval env p2) with
    | Some b1, Some b2 -> Some (b1 || b2)
    | _ -> None )
  | Imp (p1, p2) -> eval env (Or (Neg p1, p2))
  | Iff (p1, p2) -> eval env (And (Imp (p1, p2), Imp (p2, p1)))

(* Utils function - get unique elements from a sorted list *)
let get_unique =
  let rec get_unique_aux acc = function
    | x :: y :: xs -> get_unique_aux (if x = y then acc else x :: acc) (y :: xs)
    | [x] -> x :: acc
    | [] -> acc
  in
  get_unique_aux []

(* Get a set of atomic formulas (literals) from an equation *)
let get_lit =
  let rec get_lit_aux acc = function
    | Lit x -> get_lit_aux (x :: acc) True
    | Neg p -> get_lit_aux acc p
    | And (p1, p2) | Or (p1, p2) | Imp (p1, p2) | Iff (p1, p2) ->
        get_lit_aux (get_lit_aux acc p1) p2
    | _ -> get_unique (List.sort compare acc)
  in
  get_lit_aux []

(* Generate truth table from list of literals *)
let truth_table =
  let rec truth_table_aux acc = function
    | x :: xs ->
        let rec iter acc = function
          | y :: ys -> iter (((x, true) :: y) :: ((x, false) :: y) :: acc) ys
          | [] -> acc
        in
        truth_table_aux (iter [] acc) xs
    | [] -> acc
  in
  truth_table_aux [[]]

(* Get interpretation from truth table entry *)
let rec get_env = function x :: xs -> update (get_env xs) x | [] -> empty_env

(* Get all satisfying interpretations of a formula *)
let get_sat_env p =
  let lit_list = get_lit p in
  let table = truth_table lit_list in
  List.filter
    (fun x -> match eval (get_env x) p with Some b -> b | None -> false)
    table

let prec = function
  | True -> 0
  | False -> 1
  | Lit _ -> 2
  | Neg _ -> 3
  | And _ -> 4
  | Or _ -> 5
  | Imp _ -> 6
  | Iff _ -> 7

let rec prettyprint x =
  match x with
  | True -> "t"
  | False -> "f"
  | Lit x -> x
  | Neg p ->
      let s = prettyprint p in
      if prec x < prec p then "~(" ^ s ^ ")" else "~" ^ s
  | And (p1, p2) ->
      let s1 = prettyprint p1 in
      let s2 = prettyprint p2 in
      (if prec x < prec p1 then "(" ^ s1 ^ ")" else s1)
      ^ "&"
      ^ if prec x < prec p2 then "(" ^ s2 ^ ")" else s2
  | Or (p1, p2) ->
      let s1 = prettyprint p1 in
      let s2 = prettyprint p2 in
      (if prec x < prec p1 then "(" ^ s1 ^ ")" else s1)
      ^ "|"
      ^ if prec x < prec p2 then "(" ^ s2 ^ ")" else s2
  | Imp (p1, p2) ->
      let s1 = prettyprint p1 in
      let s2 = prettyprint p2 in
      (if prec x <= prec p1 then "(" ^ s1 ^ ")" else s1)
      ^ "->"
      ^ if prec x <= prec p2 then "(" ^ s2 ^ ")" else s2
  | Iff (p1, p2) ->
      let s1 = prettyprint p1 in
      let s2 = prettyprint p2 in
      (if prec x <= prec p1 then "(" ^ s1 ^ ")" else s1)
      ^ "<->"
      ^ if prec x <= prec p2 then "(" ^ s2 ^ ")" else s2

let exp1 = Imp (Imp (Imp (Lit "P", Lit "Q"), Lit "P"), Lit "P")

let exp2 = Imp (Or (Lit "P", Lit "Q"), Lit "P")

let exp3 = Or (Neg (Lit "P"), Lit "P")

let _ = prettyprint exp1

let _ = prettyprint exp2

let _ = prettyprint exp3
