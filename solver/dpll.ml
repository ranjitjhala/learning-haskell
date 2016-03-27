(*
 * Davis-Putnam-Logemann-Loveland (DPLL) algorithm
 *
 * This algorithm determines if an instance of boolean SAT in conjunctive
 * normal form is satisfiable. If so, we return a satifying assignment (a
 * model). If not, we return None. 
 *
 * In the worst case this has the usual exponential behavior. In practice,
 * a heuristic handling of unit clauses and pure literals leads to quick
 * running times on many actual instanecs.
 *
 * http://en.wikipedia.org/wiki/DPLL_algorithm
 *
 * Author: Westley Weimer 
 *)
open Cnf

let dpll (clauses : clause list) : model option =

  let rec dpll_sat clauses model : model option =

(*
    Printf.printf "--\n" ;
    Printf.printf "dpll_sat: %s\n" (cnf_to_str clauses) ;
    print_model model;
    Printf.printf "--\n" ;
    flush stdout ; 
    *)

    (* 1. If we have a consistent model, we are done *) 
    if is_cnf_sat clauses model then Some(model)  

    (* 2. If there is an empty clause, the system is not satisfiable *) 
    else if List.exists (fun clause -> clause = []) clauses then begin
    (*
      Printf.printf "dpll_sat: empty clause -> unsat\n" ; 
      *) 
      None 
    end 

    else 
    (* 3. repeatedly find unit clases and propagate them *) 
    let is_unit_clause clause model = 
      (* a unit clause contains a single unassigned literal *) 
      match clause with
      | [singleton] when not (List.mem singleton model) -> true
      | _ -> false 
    in 
    let rec handle_unit_clauses clauses model = 
      try
        let unit_clause = 
          List.find (fun c -> is_unit_clause c model) clauses 
        in 
        let [(lit, v)] = unit_clause in 
        (*
        Printf.printf "unit_clause: %s %b\n" lit v ; flush stdout ; 
        *)
        let unit_propagate clause =
          (* 
           * (a) every clause (other than the unit clause itself)
           *     containing lit is removed
           * (b) in every clause that contains !lit, that literal
           *     is removed
           *) 
          match clause with
          | clause when List.mem (lit, v) clause -> None
          | clause -> 
            let new_clause = List.filter (fun (lit',v') -> 
              if lit' = lit && v' = not v then false
              else true
            ) clause in
            Some(new_clause) 
        in
        let clauses = List.fold_left (fun acc clause ->
          match unit_propagate clause with
          | None -> acc
          | Some(clause) -> clause :: acc
        ) [] clauses in 
        let model = (lit,v) :: model in 
        handle_unit_clauses clauses model 

      with Not_found -> (* no unit clause found *) 
        clauses, model
    in
    (*
    Printf.printf "unit_clause: pre: %s\n" (cnf_to_str clauses) ;
    *)
    let clauses, model = handle_unit_clauses clauses model in 
    (*
    Printf.printf "unit_clause: pos: %s\n" (cnf_to_str clauses) ;
    *)

    (* 4. repeatedly eliminate pure literals *) 
    let literals = symbols_in_cnf clauses in 
    let purity literal clauses = 
      let has_use b =
        List.exists (fun clause ->
          List.exists (fun (lit, v) -> lit = literal && v = b) clause
        ) clauses
      in
      let used_positively = has_use true in
      let used_negatively = has_use false in
      match used_positively, used_negatively with
      | true, false -> Some(true) 
      | false, true -> Some(false)
      | _, _ -> None 
    in 
    (*
    Printf.printf "pure: pre: %s\n" (cnf_to_str clauses) ;
    *)
    let clauses, model = List.fold_left (fun (clauses, model) lit ->
      match purity lit clauses with
      | None -> clauses, model 
      | Some(b) ->
        (* add it to the model and eliminate clauses containing it *) 
        (*
        Printf.printf "pure: %s %b\n" lit b ;
        flush stdout ; 
        *)
        let elim_containing_clause clause = 
          if List.mem (lit,b) clause then
            None
          else
            Some(clause) 
        in 
        let clauses = List.fold_left (fun acc clause -> 
          match elim_containing_clause clause with
          | None -> acc
          | Some(c) -> c :: acc
        ) [] clauses in 
        let model = (lit, b) :: model in
        clauses, model 
    ) (clauses, model) literals in 
    (*
    Printf.printf "pure: pos: %s\n" (cnf_to_str clauses) ;
    *)

    let literals = symbols_in_cnf clauses in 
    match literals with
    | [] -> dpll_sat clauses model 
    | hd :: tl -> begin
      let clauses_1 = [(hd,true)] :: clauses in
      match dpll_sat clauses_1 model with
      | Some(m) -> Some(m)
      | None -> 
        Printf.printf "** backtrack\n" ; flush stdout ; 
        let clauses_2 = [(hd,false)] :: clauses in
        dpll_sat clauses_2 model
    end 

  in 
  dpll_sat clauses [] 


(* 
 * DPLL Unit Testing Code
 * Author: Charles Marsh 
 *) 

(* generates a randomized cnf of length num_clauses in which each clause
 * has num_literals literals and the total number of distinct symbols is
 * bounded by num_syms *)
let generate_cnf (num_syms:int) (num_clauses:int) (num_literals:int) : cnf =
  (* utility method to generate clause of length num_literals *)
  let generate_clause () =
    let rec add_literal cnt cl =
      match cnt with
    0 -> cl
  | _ -> let sym = "P" ^ string_of_int (Random.int num_syms) in
         add_literal (cnt-1) ((sym, Random.bool ())::cl) in
    add_literal num_literals [] in
  let rec add_clause cnt cnf =
    match cnt with
  0 -> cnf
      | _ -> add_clause (cnt-1) (generate_clause ()::cnf) in
  add_clause num_clauses []

let unit_test num =
  for i = 1 to num do
    let cnf = generate_cnf (1+(Random.int 128)) 
                           (1+(Random.int 256))
                           (1+(Random.int 16)) in 
    match dpll cnf with
    | Some(model) -> 
      assert(is_cnf_sat cnf model) ;
      Printf.printf "%d - sat\n" i; flush stdout 
    | None -> Printf.printf "%d - unsat\n" i; flush stdout ; 
  done 
