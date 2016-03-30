module Exp (Exp(..),
            AExp(..),
            Anum(..),
            convertToConjuctiveNormalForm) where

    import Control.Monad.Fix

    data Exp =
        Variable String |
        And Exp Exp |
        Or Exp Exp |
        Implies Exp Exp |
        Iff Exp Exp | 
        Not Exp |
        Arithmetic AExp
        deriving (Eq)

    data AExp =
        Le Anum Anum |
        Ge Anum Anum |
        Eq Anum Anum
        deriving (Eq)

    data Anum = 
        Constant Float |
        ArithmeticVariable String |
        Add Anum Anum |
        Minus Anum Anum |
        Times Anum Anum 
        deriving (Eq)
                                  
    eliminateIffAndImplies :: Exp -> Exp
    eliminateIffAndImplies (Iff a b) =
        let a1 = (eliminateIffAndImplies a) in
        let b1 = (eliminateIffAndImplies b) in
        (And (Or (Not a1) b1) (Or (Not b1) a1))
    eliminateIffAndImplies e@(Variable _) = e
    eliminateIffAndImplies (And e1 e2) =
        And (eliminateIffAndImplies e1)
            (eliminateIffAndImplies e2)
    eliminateIffAndImplies (Or e1 e2) =
        Or (eliminateIffAndImplies e1)
           (eliminateIffAndImplies e2)
    eliminateIffAndImplies (Not e) =
        Not (eliminateIffAndImplies e)
    eliminateIffAndImplies (Implies a b) =
        Or (Not (eliminateIffAndImplies a)) (eliminateIffAndImplies b)
    eliminateIffAndImplies e@(Arithmetic _) = e 

    -- assumes no => or <=> in the expression
    applyDeMorgan :: Exp -> Exp
    applyDeMorgan e@(Variable var) = e
    applyDeMorgan (And a b) = And (applyDeMorgan a) (applyDeMorgan b)
    applyDeMorgan (Or a b) = Or (applyDeMorgan a) (applyDeMorgan b)
    applyDeMorgan (Not e) =
        case e of
          Or a b -> And (Not (applyDeMorgan a)) (Not (applyDeMorgan b))
          And a b -> Or (Not (applyDeMorgan a)) (Not (applyDeMorgan b))
          e -> Not (applyDeMorgan e)
    applyDeMorgan e@(Arithmetic _) = e
    applyDeMorgan (Implies _ _ ) = undefined
    applyDeMorgan (Iff _ _) = undefined

    -- assumes no => or <=> in the expression
    -- assumes negations olny in front of literals or
    -- other negations (i.e. we eliminated iff and implies
    -- and we applied DeMorgan laws)
    eliminateDoubleNegation :: Exp -> Exp
    eliminateDoubleNegation e@(Variable _) = e
    eliminateDoubleNegation (And a b) = And (eliminateDoubleNegation a) (eliminateDoubleNegation b)
    eliminateDoubleNegation (Or a b) = Or (eliminateDoubleNegation a) (eliminateDoubleNegation b)
    eliminateDoubleNegation (Implies _ _) = undefined
    eliminateDoubleNegation (Iff _ _) = undefined
    eliminateDoubleNegation (Not e) = case e of
                                        Not e1 -> e1
                                        _ -> Not (eliminateDoubleNegation e)
    eliminateDoubleNegation e@(Arithmetic _) = e

    
    eliminateConjInDisj :: Exp -> Exp
    eliminateConjInDisj e@(Variable _) = e
    eliminateConjInDisj (And e1 e2) = And (eliminateConjInDisj e1) (eliminateConjInDisj e2)
    eliminateConjInDisj (Or (And a b) c) =
        let a' = eliminateConjInDisj a in
        let b' = eliminateConjInDisj b in
        let c' = eliminateConjInDisj c in
        (And (Or a' c') (Or b' c'))
    eliminateConjInDisj (Or a (And b c)) =
        let a' = eliminateConjInDisj a in
        let b' = eliminateConjInDisj b in
        let c' = eliminateConjInDisj c in
        (And (Or a' b') (Or a' c'))
    eliminateConjInDisj (Or a b) = Or (eliminateConjInDisj a) (eliminateConjInDisj b)
    eliminateConjInDisj (Implies _ _ ) = undefined
    eliminateConjInDisj (Iff _ _) = undefined
    eliminateConjInDisj (Not e) = Not (eliminateConjInDisj e)
    eliminateConjInDisj e@(Arithmetic _) = e
          
    fixed :: (Exp ->Exp) -> Exp -> Exp
    fixed f e = let e1 = f e in
                if e1 == e then e1
                else fixed f e1

    --expression to Conjuctive Normal Form
    convertToConjuctiveNormalForm :: Exp -> Exp
    convertToConjuctiveNormalForm e =
        let e1 = eliminateIffAndImplies e in
        let e2 = fixed applyDeMorgan e1 in
        let e3 = fixed eliminateDoubleNegation e2 in
        fixed eliminateConjInDisj e3

    instance Show(Exp) where
           show (Variable var) = var
           show (And e1 e2) = "("++(show e1)++") && ("
                            ++ (show e2) ++ ")"
           show (Or e1 e2) = "("++(show e1)++") || ("
                            ++ (show e2) ++ ")"
           show (Implies e1 e2) = "("++(show e1)++") => ("
                            ++ (show e2) ++ ")"
           show (Iff e1 e2) = "("++(show e1)++") <=> ("
                            ++ (show e2) ++ ")"
           show (Not e) = "!("++ (show e) ++")"
           show (Arithmetic aexp) = show aexp


    instance Show(AExp) where
           show (Le anum1 anum2) = "("++ (show anum1) ++ ")<("
                                   ++ (show anum2) ++ ")"
           show (Ge anum1 anum2) = "("++ (show anum1) ++ ")>("
                                   ++ (show anum2) ++ ")"
           show (Eq anum1 anum2) = "("++ (show anum1) ++ ")==("
                                   ++ (show anum2) ++ ")"

    instance Show(Anum) where
           show (Constant ct) = show ct
           show (ArithmeticVariable var) = var
           show (Add anum1 anum2) = "(" ++ (show anum1) ++ ")+("
                                    ++ (show anum2) ++ ")"
           show (Minus anum1 anum2) = "(" ++ (show anum1) ++ ")-("
                                    ++ (show anum2) ++ ")"
           show (Times anum1 anum2) = "(" ++ (show anum1) ++ ")*("
                                    ++ (show anum2) ++ ")"
