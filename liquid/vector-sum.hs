import Data.Vector

vectorSum         :: Vector Int -> Int
vectorSum vec     = go 0 0
  where
    go acc i
      | i < sz    = go (acc + (vec ! i)) (i + 1)
      | otherwise = acc
    sz            = Data.Vector.length vec


 -- /home/nora/work/learning-haskell/liquid/vector-sum.hs:7:25-39: Error: Liquid Type Mismatch
 
 -- 7 |       | i < sz    = go (acc + (vec ! i)) (i + 1)
 --                             ^^^^^^^^^^^^^^^
 
 --   Inferred type
 --     VV : {VV : Int | VV == acc + ?a
 --                      && VV == ?b}
  
 --   not a subtype of Required type
 --     VV : {VV : Int | VV < acc
 --                      && VV >= 0}
  
 --   In Context
 --     ?b : {?b : Int | ?b == acc + ?a}
      
 --     ?a : Int
      
 --     acc : Int
