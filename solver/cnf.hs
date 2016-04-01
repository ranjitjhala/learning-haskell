module CNF (Symbol,Literal,Clause,NamedAExp,Cnf,
           CNF.convertToConjuctiveNormalForm) where

    import Control.Monad.State
    import Exp
    
    type Symbol = String
        
    data Literal = Literal {literal::Symbol,
                            -- positive is true if the literal
                            -- has no negation in front
                            positive::Bool} deriving (Eq,Show)
    type Clause = [Literal]

    data NamedAExp = NamedAExp {aexpSymbol::String,
                                aexp::Exp.AExp} deriving (Eq,Show)
        
    data Cnf = Cnf { clauses::[Clause],
                     mapping::[NamedAExp] } deriving (Eq,Show)

    data Assigment = Assigment {symbol::Symbol,
                                value::Bool}
    type Model = [Assigment]

    newSymbol :: Int -> String
    newSymbol crt = "_" ++ (show crt)

                    
    buildClause :: Exp -> State Int Cnf
    buildClause (Or e1 e2) = do
      e1' <- buildClause e1
      e2' <- buildClause e2
      return Cnf{ clauses = [(concat((clauses e1')++(clauses e2')))],
                  mapping = (mapping e1')++(mapping e2') }
    buildClause (Variable v) =
        return (Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
                      mapping = [] })
    buildClause (Not (Variable v)) =
        return (Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
              mapping = [] })
    buildClause (Arithmetic a) =
        state $ \crt ->
            let v = newSymbol crt in
            (Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
                   mapping = [NamedAExp {aexpSymbol = v, aexp = a }] }, crt+1)
    buildClause (Not (Arithmetic a)) =
        state $ \crt ->
            let v = newSymbol crt in
            (Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
                   mapping = [NamedAExp {aexpSymbol = v, aexp = a }] }, crt+1)
    buildClause _ = undefined
                    
        
    cnfToCnf :: Exp -> State Int Cnf
    cnfToCnf (Variable v) =
        return (Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
              mapping = [] })
    cnfToCnf (Not (Variable v)) =
        return (Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
              mapping = [] })
    cnfToCnf (And e1 e2) = do
        e1' <- cnfToCnf e1
        e2' <- cnfToCnf e2
        return (Cnf{ clauses=(clauses e1')++(clauses e2'),
             mapping=(mapping e1')++(mapping e2') })
    cnfToCnf e@(Or _ _) = buildClause e
    cnfToCnf (Arithmetic a) =
        state $ \crt ->
            let v = newSymbol crt in
            (Cnf { clauses = [ [Literal { literal=v, positive=True}] ],
                   mapping = [NamedAExp {aexpSymbol = v, aexp = a }] }, crt+1)
    cnfToCnf (Not (Arithmetic a)) =
        state $ \crt ->
            let v = newSymbol crt in
            (Cnf { clauses = [ [Literal { literal=v, positive=False}] ],
                   mapping = [NamedAExp {aexpSymbol = v, aexp = a }] }, crt+1)
    cnfToCnf _ = undefined

                 
    convertToConjuctiveNormalForm :: Exp -> CNF.Cnf
    convertToConjuctiveNormalForm e =
        let e1 = Exp.convertToConjuctiveNormalForm e in
        let (e2, _) = runState (cnfToCnf e1) 0 in e2
              
