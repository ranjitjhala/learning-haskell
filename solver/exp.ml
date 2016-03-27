(*
 * DPLL(T) SMT Solver -- Expression Grammar
 *
 * We distinguish between high-level boolean expressions and arithmetic
 * expressions (i.e., expressions in a special 'theory'). Boolean
 * expressions may contain arithmetic expressions but not the other way
 * around.
 *
 * This means that we have both propositional variables (e.g., "p = true")
 * and arithmetic variables (e.g., "x = 5"). 
 *)
open Printf
open Cnf

type exp = (* evaluates to a Boolean *) 
  | Variable of Cnf.symbol
  | And of exp * exp 
  | Or of exp * exp
  | Implies of exp * exp 
  | Iff of exp * exp 
  | Not of exp 
  | Arithmetic of aexp 

and aexp = (* Evaluates to a Boolean *)
  | Le of anum * anum
  | Ge of anum * anum 
  | Eq of anum * anum 

and anum = (* Evaluates to a Number *)
  | Number of float 
  | Arithmetic_Variable of Cnf.symbol  
  | Plus of anum * anum 
  | Minus of anum * anum 
  | Times of anum * anum 

let rec exp_to_str exp = 
  match exp with
  | Variable(x) -> x
  | And(e1,e2) -> sprintf "(%s) && (%s)" (exp_to_str e1) (exp_to_str e2) 
  | Or(e1,e2) -> sprintf "(%s) || (%s)" (exp_to_str e1) (exp_to_str e2) 
  | Not(e1) -> sprintf "!(%s)" (exp_to_str e1) 
  | Implies(e1,e2) -> sprintf "(%s) -> (%s)" (exp_to_str e1) (exp_to_str e2) 
  | Iff(e1,e2) -> sprintf "(%s) <-> (%s)" (exp_to_str e1) (exp_to_str e2) 
  | Arithmetic(a) -> aexp_to_str a 
and aexp_to_str aexp =
  match aexp with
  | Le(a1,a2) -> sprintf "(%s) <= (%s)" (anum_to_str a1) (anum_to_str a2) 
  | Ge(a1,a2) -> sprintf "(%s) >= (%s)" (anum_to_str a1) (anum_to_str a2) 
  | Eq(a1,a2) -> sprintf "(%s) = (%s)" (anum_to_str a1) (anum_to_str a2) 
and anum_to_str anum =
  match anum with
  | Number(f) -> sprintf "%g" f 
  | Arithmetic_Variable(v) -> v 
  | Plus(a1,a2) -> sprintf "(%s) + (%s)" (anum_to_str a1) (anum_to_str a2) 
  | Minus(a1,a2) -> sprintf "(%s) - (%s)" (anum_to_str a1) (anum_to_str a2) 
  | Times(a1,a2) -> sprintf "(%s) * (%s)" (anum_to_str a1) (anum_to_str a2) 

(* We sometimes need to associate arithmetic expression (in our special
 * "theory") with SAT variables. We use a mapping to track such an
 * association. *) 
type mapping = (string * aexp) list 

module OrderedString =
struct
  type t = string
  let compare = compare
end
module StringSet = Set.Make(OrderedString) 
module StringMap = Map.Make(OrderedString) 

(*
 * Transform an arbitrary boolean expression into conjunctive normal form.
 *
 * This is useful because many SAT solvers require CNF input. 
 *
 * Subexpressions involving arithmetic cannot be converted to CNF. Instead,
 * each such subexpression is associated with a special new variable and
 * mapping is maintained to keep track of such associations.
 *
 * http://en.wikipedia.org/wiki/Conjunctive_normal_form#Converting_from_first-order_logic
 *)
let rec exp_to_cnf (exp : exp) : (cnf * mapping) = 

  let rec fixpoint convert exp =
    let exp' = convert exp in
    if exp' = exp then exp
    else fixpoint convert exp' 
  in

  (* Step 1 - remove Implies and IFF *) 
  let rec convert exp = 
    match exp with
    | Implies(a,b) -> Or(Not(convert a), convert b)
    | Iff(a,b) -> 
      let a = convert a in 
      let b = convert b in
      And(Or(a,Not(b)) , Or(Not(a), b))

    | And(a,b) -> And(convert a, convert b) 
    | Or(a,b) -> Or(convert a, convert b) 
    | Not(a) -> Not(convert a) 
    | x -> x
  in
  let exp = fixpoint convert exp in 

  (* Step 2 - push Not inward *)
  let rec convert exp =
    match exp with
    | Not(Or(p,q)) -> And(Not(convert p), Not(convert q))
    | Not(And(p,q)) -> Or(Not(convert p), Not(convert q))
    | Not(Not(p)) -> p 
    | Not(e) -> Not(convert e) 
    | And(a,b) -> And(convert a, convert b)
    | Or(a,b) -> Or(convert a, convert b)
    | x -> x
  in 
  let exp = fixpoint convert exp in

  (* Step 3 - distribute Or inward over And *)
  let rec convert exp =
    match exp with
    | Or(p, And(q,r))
    | Or(And(q,r), p) -> 
      let p = convert p in
      let q = convert q in
      let r = convert r in 
      And(Or(p,q), Or(p,r))
    | Not(e) -> Not(convert e)
    | And(a,b) -> And(convert a, convert b)
    | Or(a,b) -> Or(convert a, convert b)
    | x -> x
  in 
  let exp = fixpoint convert exp in 

  let fresh_variable_counter = ref 0 in 
  let new_variable () = 
    let var = sprintf "_%d" !fresh_variable_counter in
    incr fresh_variable_counter ;
    var
  in

  let rec exp_to_cnf exp : cnf * ((Cnf.symbol * aexp) list) =
    match exp with
    | Variable(sym) -> 
      let clause = [ (sym, true) ] in
      [ clause ], [] 
    | Not(Variable(sym)) -> 
      let clause = [ (sym, false) ] in
      [ clause ], [] 
    | And(e1, e2) ->
      let cnf1, mapping1 = exp_to_cnf e1 in 
      let cnf2, mapping2 = exp_to_cnf e2 in 
      cnf1 @ cnf2, mapping1 @ mapping2 
    | Or(_, _) -> begin 
      (* 
       * By the transformation above, subexpressions here can
       * only contain Or, Not or Variable (at the bare logic level)
       * or Theory expressions (such as Arithmetic). 
       *)
      let rec exp_to_clause exp : clause * mapping = 
        match exp with 
        | Variable(sym) -> [(sym, true)], [] 
        | Not(Variable(sym)) -> [(sym, false)], [] 
        | Or(e1,e2) -> 
          let c1, m1 = exp_to_clause e1 in 
          let c2, m2 = exp_to_clause e2 in 
          c1 @ c2, m1 @ m2 
        | Arithmetic(a) -> 
          let v = new_variable () in 
          [(v, true)], [(v, a)]
        | Not(Arithmetic(a)) -> 
          let v = new_variable () in 
          [(v, false)], [(v, a)]
        | _ -> failwith "exp_to_clause: impossible (1)" 
      in
      let clause, mapping = exp_to_clause exp in
      [ clause ], mapping
    end 
    | Arithmetic(a) -> 
      let v = new_variable () in
      let clause = [ (v, true) ] in
      [ clause ], [(v, a)] 
    | Not(Arithmetic(a)) -> 
      let v = new_variable () in
      let clause = [ (v, false) ] in
      [ clause ], [(v, a)] 
    | _ -> failwith "exp_to_cnf: impossible (2)" 
  in
  exp_to_cnf exp 

