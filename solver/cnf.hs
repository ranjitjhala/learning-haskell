module CNF (Symbol,Literal,Clause,NamedAExp,Cnf,
           CNF.convertToConjuctiveNormalForm) where

    import Exp
    
    type Symbol = String
        
    data Literal = Literal {literal::Symbol,
                            -- positive is true if the literal
                            -- has no negation in front
                            positive::Bool}
    type Clause = [Literal]

    data NamedAExp = NamedAExp {aexpSymbol::String,
                                aexp::Exp.AExp}
        
    data Cnf = Cnf { clauses::[Clause],
                     mapping::[NamedAExp] }

    data Assigment = Assigment {symbol::Symbol,
                                value::Bool}
    type Model = [Assigment]

    newSymbol :: String
    newSymbol = undefined --TODO

    buildClause :: Exp -> Cnf
    buildClause _ = undefined --TODO
        
    cnfToCnf :: Exp -> Cnf
    cnfToCnf (Variable v) =
        Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
              mapping = [] }
    cnfToCnf (Not (Variable v)) =
        Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
              mapping = [] }
    cnfToCnf (And e1 e2) =
        let e1' = cnfToCnf e1 in
        let e2' = cnfToCnf e2 in
        Cnf{ clauses=(clauses e1')++(clauses e2'),
             mapping=(mapping e1')++(mapping e2') }
        
    cnfToCnf e@(Or _ _) = buildClause e

    cnfToCnf (Arithmetic a) =
        let v = newSymbol in
        Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
              mapping = [NamedAExp {aexpSymbol = v,
                                   aexp = a }] }

    cnfToCnf (Not (Arithmetic a)) =
        let v = newSymbol in
        Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
              mapping = [NamedAExp {aexpSymbol = v,
                                   aexp = a }] }
        
    convertToConjuctiveNormalForm :: Exp -> CNF.Cnf
    convertToConjuctiveNormalForm e =
        cnfToCnf $ Exp.convertToConjuctiveNormalForm e 
        
              
