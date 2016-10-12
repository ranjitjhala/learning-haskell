data Exp =
     Variable String |
     And Exp Exp |
     Or Exp Exp |
     Implies Exp Exp |
     Not Exp
     deriving (Eq)

data Hypothesis = String Exp

