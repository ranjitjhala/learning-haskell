-- worked out examples based on http://ucsd-progsys.github.io/lh-workshop

--  {-@ type Zero = { v:Int | v == 0 } @-}
--  {-@ zero :: Zero @-}
-- zero = 0::Int

-- {-@ type Nat = { v:Int | 0 <= v } @-}
-- {-@ zeroN :: Nat @-}
-- zeroN = zero

{-@ type Zero = {v:Int | v = 0} @-}
{-@ zero :: Zero @-}
zero = 0
