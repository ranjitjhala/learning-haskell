(*
 * DPLL(T) Satisfiability Modulo Theories Solver
 *
 * Author: Westley Weimer (this base implementation)
 *
 * http://en.wikipedia.org/wiki/DPLL_algorithm
 *
 * In this particular implementation, the only special theory considered is
 * simple bounded integer arithmetic (via an inefficient but simple
 * solver). 
 *)

open Cnf
open Exp 
open Printf

(* Since we are solving constraints over both propositional and arithmetic
 * constraints, we will need to find a satisfying assignment (a model)
 * for both logical variables and for arithmetic variables.
 *)
type logic_model = Cnf.model
type arithmetic_model = Arith.arithmetic_model 
type solver_output = (logic_model * arithmetic_model) option 

(* Our solver takes as input a CNF formula encoding mixed constraints
 * as well as a mapping that relates some special CNF variables to
 * expressions in other theories (in our case, in our simple
 * bounded arithmetic theory). 
 *
 * If the CNF formula cannot be satisfied, it returns None.
 *
 * Otherwise, if the CNF formula can be satisfied it returns Some
 * of both a logical model (an assignment of boolean values to
 * propositional variables) and an arithmetic model (an assignment
 * of integers to arithmetic varibles) that simultaneously satisfy
 * the CNF formula (including the mapping).
 *
 * For example, "(p <-> (x < 5)) && !!p" could be satisfied by: 
 *  logic_model:      p = true
 *  arithmetic_model: x = 3 
 *)
let rec solver (cnf : Cnf.cnf) (mapping : Exp.mapping) : solver_output = 
  (*
  Printf.printf "solver: %s\n" (cnf_to_str cnf) ;  
  *) 

  (* Step 1: Ask our SAT solver to try to satisfy the CNF clauses. *) 
  let model_option = Dpll.dpll cnf in 

  match model_option with
  | Some(model) -> begin 
    (* Check our Theories *) 

    let (theory_clauses : (aexp * bool) list) = 
      List.fold_left (fun acc (v, b) ->
      try
        let exp = List.assoc v mapping in
        (exp, b) :: acc
      with Not_found -> acc
    ) [] model in 

    if theory_clauses = [] then begin
      (* We can satify these CNF clauses without involving any 
       * variables that relate to our special arithmetic theory
       * (i.e., any variables in the mapping). That means that
       * this is purely a question of propositional logic (i.e.,
       * basic SAT), so we are done. *)
      Some(model, [] (* no arithmetic model needed *))
    end else begin

      (* You must add code here to handle the integration between 
       * our simple bounded arithmetic theory (Arith.arith) 
       * and our SAT solver. *) 

      None (* FIXME! *)  


    end 
  end 

  | None -> 
    (* The SAT solver could not satisfy the CNF clauses, so regardless of
     * what the mapping contains (i.e., regardless of what might be going
     * on with our arithmetic theory) this is not satisfiable. *) 
    None 


(*
 * A simple driver for our DPLL(T) SMT solver. We parse the input from
 * stdin and pass it to the solver defined above. If this results
 * in a satisfying model, we print out the model. 
 *)
let main () = begin

  let lexbuf = Lexing.from_channel stdin in
  try 
  while true do
    let exp = Parser.main Lexer.token lexbuf in 
    (*
    printf "\nExpression:\n%s\n" (exp_to_str exp) ; 
    *) 
    let cnf, mapping = exp_to_cnf exp in 
    (*
    printf "\nCNF:\n%s\n" (Cnf.cnf_to_str cnf) ; 
    List.iter (fun (v,a) -> 
      printf "%s ==\t%s\n" v (aexp_to_str a) 
    ) mapping ; 
    *)
    match solver cnf mapping with
    | Some(model, amodel) -> 
      printf "\n\nSatisfiable!\n\n" ;
      (* No need to print the values for temporary variables that we
       * made up ourselves. *) 
      let model = List.filter (fun (name,asgn) -> name.[0] <> '_') model in
      Cnf.print_model (List.sort compare model) ; 
      List.iter (fun (name,number) -> 
        Printf.printf "%s = %d\n" name number ;
      ) (List.sort compare amodel) ; 
      flush stdout 
    | None -> 
      printf "\n\nUnsatisfiable!\n\n" ; 
      flush stdout 
  done 
  with _ -> () 

end ;; 

main () ;;
