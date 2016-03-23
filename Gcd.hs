import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 =
          (tell ["Finished with " ++ show a])
          >>= (\_ -> (return a))  
    | otherwise =
          (tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)] )
          >>= (\_ -> gcd' b (a `mod` b))
    -- | b == 0 = do  
    --     tell ["Finished with " ++ show a]  
    --     return a  
    -- | otherwise = do  
    --     tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
    --     gcd' b (a `mod` b)  
