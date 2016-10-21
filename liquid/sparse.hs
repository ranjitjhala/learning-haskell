-- Legal Sparse vectors satisfy two crucial properties.
-- First, the dimension stored in spDim is non-negative.
-- Second, every index in spElems must be valid,
-- i.e. between 0 and the dimension.


-- needs capital letters for Lo and Hi, othewise very confusing error
--  Malformed Type Alias Application
{-@ type Between Lo Hi = {v:Int|Lo <= v && v <Hi} @-}

{-@ type Nat = {v:Int|0<=v} @-}

{-@ data Sparse a = SP {spDim :: Nat,
                    spElems :: [(Between 0 spDim,a)] }
@-}
data Sparse a = SP { spDim :: Int,
                     spElems :: [(Int,a)] }


{-@ type SparseN a N = { v:Sparse a| spDim == N } @-}

{-@ dotProduct :: sp1:SparseN a N ->
                  {v : Sparse a M | M = N } ->
                  (a->a->a) -> (a->a->a) ->  a @-}
dotProduct sp1 sp2 mult add = undefined
