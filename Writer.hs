{-# LANGUAGE DeriveFunctor #-}
import Control.Monad

newtype MyWriter w a = MyWriter { runWriter :: (a, w) } deriving Functor

instance (Monoid w) => Monad (MyWriter w) where  
    return x = MyWriter (x, mempty)
    -- f :: type of x -> Writer w (type of v)
    (MyWriter (a,w)) >>= f =
        let (MyWriter (a', w')) = f a in MyWriter (a', w `mappend` w')

instance Monoid w => Applicative (MyWriter w) where
    pure = return
    (<*>) = ap

logNumber :: Int -> MyWriter [String] Int  
logNumber x = MyWriter (x, ["Got number: " ++ show x])  
  
multWithLog :: MyWriter [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)

--(logNumber 3) >>= ( \x -> (logNumber 5) >>= (\y -> return x*y))

-- OUTPUT
-- runWriter (return 3 :: MyWriter String Int)
-- (3,"")
-- runWriter (return 3 :: MyWriter (Data.Monoid.Sum Int) Int)  
-- (3,Sum {getSum = 0})
-- runWriter (return 3 :: MyWriter (Data.Monoid.Product Int) Int)  
-- (3,Product {getProduct = 1})
-- "", 0, 1 is just what mempty returns for the Monoids String, Sum, Product
-- runWriter ( (return 3 :: MyWriter String Int) >>= (\n -> show n))
-- runWriter ( (return 3 :: MyWriter (Data.Monoid.Sum Int) Int) >>= (\n -> MyWriter (n, (Data.Monoid.Sum n))))
-- (3,Sum {getSum = 3})
-- runWriter ( (return 3 :: MyWriter (Data.Monoid.Sum Int) Int)
-- >>= (\n -> MyWriter (n, (Data.Monoid.Sum n)))
-- >>= (\n -> MyWriter (n, (Data.Monoid.Sum n))) )
-- (3,Sum {getSum = 6})
