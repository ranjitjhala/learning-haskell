{-@ twice :: {v:Int|v > 10} -> {v:Int|v>20} @-}
twice :: Int -> Int
twice x = 2*x

{-@ type Twelve = { v:Int | v == 12 } @-}
{-@ twelve :: Twelve @-}
twelve = 12

main = do
  putStrLn $ "twice 11=" ++ (show $ twice 11)
  putStrLn $ "twice 12=" ++ (show $ twice $ twelve)
  putStrLn $ "twice twice 11=" ++ (show $ twice $ twice 11)
  putStrLn $ "twice 1=" ++ (show $ twice 1)
  
