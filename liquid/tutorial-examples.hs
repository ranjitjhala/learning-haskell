import Data.Vector

-- worked out examples based on
-- http://ucsd-progsys.github.io/liquidhaskell-tutorial

{-@ type TRUE = {v:Bool| Prop v} @-}
{-@ type FALSE = {v:Bool| not(Prop v)} @-}

{-@ pTrue :: TRUE @-}
pTrue = True

--{-@ invalid :: TRUE @-}
-- invalid = False
--  Error: Liquid Type Mismatch
 
--  14 | invalid = False
--       ^^^^^^^
 
--    Inferred type
--      VV : {VV : Bool | not (Prop VV)
--                        && VV == GHC.Types.False}
  
--    not a subtype of Required type
--      VV : {VV : Bool | Prop VV}


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

{-@ average :: { v : [Int] | len v > 0 } -> Int @-}
average :: [Int] -> Int
average xs = Prelude.sum xs `div` Prelude.length xs


-- TODO
-- Want to specify a postcondition, but don't know yet how
-- {-@ buildList :: b:Bool -> { xs : [Int] | !b <=> len xs > 0 } @-}  
-- buildList :: Bool -> [Int]
-- buildList True = []
-- buildList False = [1,2,3]

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
-- TODO Talk to Wes
-- {-@ abs1 :: Int -> Nat @-}
-- abs1 :: Int -> Int
-- abs1 n 
--   | 0 < n = n
--   | otherwise = -n

-- {-@ plus :: Nat -> Nat -> Nat @-}
-- plus::Int->Int->Int
-- plus x y = x+y
    
-- {-@ absoluteSum :: Vector Int -> Nat @-}
-- absoluteSum vec = absoluteSumRec vec 0 0
-- {-@ absoluteSumRec :: Vector Int -> Nat -> Nat -> Nat  @-}
-- absoluteSumRec vec acc index
--   | index < sz =
--       let elt = vec ! index in
--         let absElt = abs1 elt in
--           let newAcc = plus acc absElt in
--             let newIndex = plus index 1 in
--               absoluteSumRec vec newAcc newIndex
--   | otherwise =  acc
--   where
--     sz = Data.Vector.length vec

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

main :: IO ()
main = do
  putStrLn $ show $ average [1,2,3]
  --putStrLn $ show $ average []
  --putStrLn $ show $ average $ buildList False
  --putStrLn $ show $ average $ buildList True
