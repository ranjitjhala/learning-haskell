(*
 * Bounded Integer Arithmetic Theory 
 *
 * This is a very simple, but very inefficient, integer arithmetic
 * constraint solver.
 *
 * We consider all possible of integer valuations to all variables
 * in the constraints, but only those values that fall between a 
 * given lower bound and a given upper bound.
 *)

let lower_bound = -127 (* smallest variable value considered *) 
let upper_bound = 128  (* largest variable value considered *) 

open Exp 

(* Find all variables in an arithmetic constraint *) 
let rec variables_of_aexp aexp = match aexp with
  | Le(a,b) | Ge(a,b) | Eq(a,b) -> 
    StringSet.union (variables_of_anum a) (variables_of_anum b)
and variables_of_anum anum = match anum with
  | Arithmetic_Variable(x) -> StringSet.singleton x
  | Plus(x,y) | Minus(x,y) | Times(x,y) -> 
    StringSet.union (variables_of_anum x) (variables_of_anum y)
  | _ -> StringSet.empty 

(* Evaluate an arithmetic constraint with respect to a model *) 
let rec eval_aexp model aexp = match aexp with
  | Le(a,b) -> eval_anum model a <= eval_anum model b 
  | Ge(a,b) -> eval_anum model a >= eval_anum model b 
  | Eq(a,b) -> eval_anum model a  = eval_anum model b 
and eval_anum model anum = match anum with
  | Number(x) -> x
  | Arithmetic_Variable(v) -> float_of_int (StringMap.find v model)
  | Plus(x,y) -> eval_anum model x +. eval_anum model y 
  | Minus(x,y) -> eval_anum model x -. eval_anum model y 
  | Times(x,y) -> eval_anum model x *. eval_anum model y 

(* Stop enumerating candidate models once we have found a solution. 
 * To break out of our nested computation early we throw an exception. *)
exception Solution_Found of (int StringMap.t)

type arithmetic_model = (Cnf.symbol * int) list 

(* Our bounded integer arithmetic constraint solver. *) 
let arith (constraints : ((aexp * bool) list)) : arithmetic_model option = 

  try 

    let variables = List.fold_left (fun acc (aexp,b) ->
      StringSet.union acc (variables_of_aexp aexp)
    ) StringSet.empty constraints in

    (* if we ever find a valid model, stop immediately *) 
    let consider model = 
      if List.for_all 
        (fun (aexp,b) -> b = eval_aexp model aexp) constraints then
        raise (Solution_Found(model))
    in 

    (* for each variable, try all of values for it in a bounded range *)
    let rec bounded_search variables model_sofar = 
      if StringSet.is_empty variables then
        consider model_sofar
      else begin
        let variable = StringSet.choose variables in
        let variables = StringSet.remove variable variables in 
        for i = lower_bound to upper_bound do
          let model = StringMap.add variable i model_sofar in 
          bounded_search variables model
        done 
      end 
    in 

    bounded_search variables StringMap.empty ;

    (* if that "exhaustive" bounded search did not find a model,
     * we return that there is no such model *) 
    None 

  with 
  | Solution_Found(model) -> 
      let model_as_list = StringMap.fold 
        (fun vname vval acc -> (vname,vval) :: acc) model [] in 
      Some(model_as_list) 
  | x -> Printf.printf "Arith: %s\n" (Printexc.to_string x) ; raise x
