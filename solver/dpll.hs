module DPLL where
    import CNF
    import Exp
    import Theory
    import Data.List
    import Debug.Trace

    deleteClauses :: [Clause] -> Literal -> [Clause]
    deleteClauses clauses l = filter (\c -> Prelude.not (elem l c)) clauses

    -- in: list of clauses
    -- out: list of clauses,
    --      assignment if one unit is found, Nothing otherwise
    -- if one unit literal is found, then we remove all the clauses
    -- containing it and we remove its negation from all
    -- remaining clauses
    unitPropagation :: [Clause] -> ([Clause], Maybe(Model Bool))
    unitPropagation [] = ([],Nothing)
    unitPropagation clauses =
        let unitClauses = filter (\ls -> (length ls) == 1) clauses in
        case unitClauses of
          [] -> (clauses,Nothing)
          ([l]:_) ->
              -- delete every clause containing the literal
              let clauses1 = deleteClauses clauses l in
              -- remove negation of literal from each clause 
              let clauses2 = map (\c -> filter (\l' ->
                                         (literal l) /= (literal l') ) c)
                             clauses1 in
              --let (clauses3,model3) = unitPropagation clauses2 in
              (clauses2, Just (Sat [mkAssignment (literal l) (positive l)]))
         
    -- Creates a list of all positive literals in clauses
    -- if the boolean is true, and a list of all negated literals
    -- otherwise
    mkLiteralList :: [Clause] -> Bool -> [Literal]
    mkLiteralList clauses sign =
        nub $ concat $ map (\clause ->
                          filter (\l -> sign == (positive l)) clause
            ) clauses

    -- in: list of clauses
    -- out: list of clauses,
    --      assignment if one pure literal is found, Nothing otherwise
    -- if one pure literal is found, then all clauses containg it
    -- are removed
    pureLiteralElim :: [Clause] -> ([Clause], Maybe (Model Bool))
    pureLiteralElim clauses =
        let pos = mkLiteralList clauses True in
        let neg = mkLiteralList clauses False in
        let nameCmp = (\l1 l2 -> (literal l1) == (literal l2)) in
        let pureLiterals = (deleteFirstsBy nameCmp pos neg) ++
                           (deleteFirstsBy nameCmp neg pos) in
        case pureLiterals of
          [] -> (clauses, Nothing)
          (l:_) -> (deleteClauses clauses l ,
                    Just (Sat [mkAssignment (literal l) (positive l)]))

    chooseLiteral::[Clause] -> Literal
    chooseLiteral (c:_) =
        case c of
          l:_ -> l

    mkModel :: Model Bool -> Model Bool -> Model Bool
    mkModel Unsat _ = Unsat
    mkModel _ Unsat = Unsat
    mkModel (Sat l1) (Sat l2) = Sat (l1++l2)
         
     -- TODO deal with arith
    dpll :: [Clause] -> Model Bool
    dpll clauses =
        -- if the clauses are empty we're done
        if (null clauses)
        then (Sat [])
        else 
            -- check if there is an empty clause then formula is unsat
            if (or $ map (\c -> null c) clauses)
            then Unsat
            else
                -- unit propagation
                case unitPropagation clauses of
                  (clauses1, Just model) -> mkModel model (dpll clauses1)
                  (_, Nothing) ->
                      --no unit clauses
                      -- pure literal elimination
                      case pureLiteralElim clauses of
                        (clauses2, Just model) -> mkModel model (dpll clauses2)
                        (_, Nothing) -> --no pure literals
                            let l = chooseLiteral clauses in
                            case dpll ([l]:clauses) of
                              Unsat -> dpll([CNF.not l]:clauses)
                              m -> m
              
             

    findLogicalModel :: CNF.Cnf -> Model Bool
    findLogicalModel cnf = dpll (clauses cnf)
