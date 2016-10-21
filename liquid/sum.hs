import Data.Vector

{-@ type Nat = { v:Int | 0 <= v } @-}

{-@ abs1 :: Int -> Nat @-}
abs1 :: Int -> Int
abs1 n 
  | 0 < n = n
  | otherwise = -n

{-@ plus :: Nat -> Nat -> Nat @-}
plus::Int->Int->Int
plus x y = x+y
    
{-@ absoluteSum :: Vector Int -> Nat @-}
absoluteSum vec = absoluteSumRec vec 0 0
{-@ absoluteSumRec :: Vector Int -> Nat -> Nat -> Nat  @-}
absoluteSumRec vec acc index
  | index < sz =
      let elt = vec ! index in
        let absElt = abs1 elt in
          let newAcc = plus acc absElt in
            let newIndex = plus index 1 in
              absoluteSumRec vec newAcc newIndex
  | otherwise =  acc
  where
    sz = Data.Vector.length vec


 -- /home/nora/work/learning-haskell/liquid/sum.hs:24:15-39: Error: Liquid Type Mismatch
 
 -- 24 |               absoluteSumRec vec newAcc newIndex
 --                    ^^^^^^^^^^^^^^^^^^^^^^^^^
 
 --   Inferred type
 --     VV : {VV : Int | VV >= 0
 --                      && VV == newAcc}
  
 --   not a subtype of Required type
 --     VV : {VV : Int | VV >= 0
 --                      && VV < acc
 --                      && VV >= 0}
  
 --   In Context
 --     newAcc : {newAcc : Int | newAcc >= 0}
      
 --     acc : {acc : Int | acc >= 0}

-- Where does the condition VV < acc come from?
