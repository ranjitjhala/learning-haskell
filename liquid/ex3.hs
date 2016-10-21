{-@ type TRUE = {v:Bool| Prop v} @-}
{-@ type FALSE = {v:Bool| not(Prop v)} @-}


{-@ ex3 :: Bool -> Bool -> TRUE @-}
ex3 a b = (a && b) ==> a
-- TODO It doesn't compile. Find out why.

--  Error: GHC Error
 
--  6 | ex3 a b = (a && b) ==> a
--                         ^^^
 
--      Not in scope: ‘==>’
-- Perhaps you meant ‘==’ (imported from Prelude)
