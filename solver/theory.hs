module Theory (Assignment,Model(..),mkAssignment) where

    data Assignment a  = Assignment {symbol::String,
                                   value::a} deriving (Show)
    data Model a = Sat [Assignment a] | Unsat deriving (Show)
    
    mkAssignment :: String -> a -> (Assignment a)
    mkAssignment s v = Assignment { symbol=s, value=v}
    
