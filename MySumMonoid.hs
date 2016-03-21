newtype MySum a = MySum { getSum::a }

instance Num a => Monoid (MySum a) where

    mempty = MySum (fromInteger 0)

    mappend (MySum x) (MySum y) = MySum (x+y)
