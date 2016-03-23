import Data.List

-- instance Functor Maybe where
--     fmap :: (a -> b) -> (Maybe a) -> (Maybe b)
--     fmap f Nothing = Nothing
--     fmap f (Just x) = Just (show x)

main = do
  -- Maybe usage
  -- case (\x->show x) <$> (Just 3) of
  --   Nothing -> putStrLn "Nothing"
  --   Just s -> putStrLn s

  -- Either usage
  -- case (*2) <$> (Right 3) of
  --   Left s -> putStrLn s
  --   Right x -> putStrLn $ show x

  -- IO Functor from LYAH
  -- instance Functor IO where  
  --   fmap f action = do  
  --       result <- action  
  --       return (f result)
               
  -- line <- fmap reverse getLine  
  -- putStrLn $ "You said " ++ line ++ " backwards!"  
  -- putStrLn $ "Yes, you really said " ++ line ++ " backwards!" 

  -- line <- getLine
  -- let line' = reverse line  
  -- putStrLn $ "You said " ++ line' ++ " backwards!"  
  -- putStrLn $ "Yes, you really said " ++ line' ++ " backwards!" 

  -- r -> ?? === (->) r ??
  -- instance Functor ((->) r) where
  --      -- f :: a-> b
  --      -- g :: r-> a
  --      -- result :: r -> b
  --      fmap f g = (\x -> f (g x))
  -- putStrLn ((fmap (intersperse '-') show) 345)
