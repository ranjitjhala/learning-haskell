
{-@ average :: { v : [Int] | len v > 0 } -> Int @-}
average :: [Int] -> Int
average xs = Prelude.sum xs `div` Prelude.length xs

--
--{-@ buildList :: b:Bool -> { xs : [Int] | !b <=> len xs > 0 } @-}  
buildList :: Bool -> [Int]
buildList True = []
buildList False = [1,2,3]

main :: IO ()
main = do
  --putStrLn $ show $ average [1,2,3]
  --putStrLn $ show $ average []
  putStrLn $ show $ average $ buildList False
  --putStrLn $ show $ average $ buildList True


 -- Error: Liquid Type Mismatch
 
 -- 16 |   putStrLn $ show $ average $ buildList False
 --                          ^^^^^^^
 
 --   Inferred type
 --     VV : {VV : [Int] | len VV >= 0}
  
 --   not a subtype of Required type
 --     VV : {VV : [Int] | len VV > 0}
  
 --   In Context
