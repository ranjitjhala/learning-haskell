import Data.Vector

-- worked out examples based on
-- http://ucsd-progsys.github.io/liquidhaskell-tutorial

{-@ type TRUE = {v:Bool| Prop v} @-}
{-@ type FALSE = {v:Bool| not(Prop v)} @-}

{-@ pTrue :: TRUE @-}
pTrue = True

-- Exercise
--{-@ ex5 :: Bool -> Bool -> TRUE @-}
--ex5 a b = a ==> a||b
-- TODO It doesn't compile. Find out why.

------------------------------------------------------------------
-- commented to keep later stuff simple
--{-@ type Zero = { v:Int | v == 0 } @-}
--{-@ zero :: Zero @-}
--zero = 0::Int

{-@ type Nat = { v:Int | 0 <= v } @-}
--{-@ zeroN :: Nat @-}
--zeroN = zero

-- old, doesn't work anymore 
-- {-@ type Zero = {v:Int | v = 0} @-}
-- {-@ zero :: Zero @-}
-- zero = 0

------------------------------------------------------------------

{-@ isPositive :: x:Int -> { v:Bool | Prop v <=> x > 0 } @-}
isPositive :: Int -> Bool
isPositive x = x > 0

divide n d 
  | isPositive d = show (n `div` d)
  | otherwise = "nope"

------------------------------------------------------------------

{-@ die :: { v:String | false } -> a @-}
die :: String -> a
die msg = error msg

{-@ lAssert :: {b:Bool| Prop b } -> a -> a @-}
lAssert True x = x
lAssert False _ = die "assertion fails"

yes = lAssert (1 + 1 == 2) ()
--no = lAssert (1 + 1 == 3) ()

------------------------------------------------------------------

-- The function unsafeLookup is a wrapper around the (!) with
-- the arguments flipped. Modify the specification for unsafeLookup
-- so that the implementation is accepted by LiquidHaskell.
-- TODO talk with Wes
--{-@ unsafeLookup :: index:Int -> {v:Vector a|index <= vlen v && 0 <= index} -> a @-}
-- doesn't work because I refine the type vector, not the int
--{-@ unsafeLookup :: index:{ v:Int | v >=0 && v <= vlen vec} -> vec:Vector -> a @-}
-- doesn't compile because vec is used before it's defined
-- some other failed attempt
-- {-@ type LT N = {v:Int| v>=0 && v <= N} @-}
-- {-@ unsafeLookup :: index:LT (vlen vec) -> vec:Vector a -> a @-}
-- unsafeLookup index vec = vec ! index

------------------------------------------------------------------

-- This code is from the tutorial but it doesn't compile
-- TODO check with Wes
loop :: Int -> Int -> a -> (Int -> a -> a) -> a
loop lo hi base f = go base lo
  where
    go acc i
      | i < hi = go (f i acc) (i+1)
      | otherwise = acc

vectorSum :: Vector Int -> Int
vectorSum vec = loop 0 n 0 f
  where
    f i acc = acc + (vec!i)
    n = Data.Vector.length vec

