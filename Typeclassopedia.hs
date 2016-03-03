import Prelude hiding (Either, Left, Right, Maybe, Just, Nothing)

data Either a b = Left a
                | Right b

data Maybe a = Nothing
             | Just a

data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)
              deriving (Show, Read, Eq)

instance Functor Tree where
    fmap _ EmptyTree = EmptyTree
    fmap g (Node x left right) =
        Node (g x) (fmap g left) (fmap g right)

instance Functor (Either e) where
    -- g :: a-> b
    -- fmap :: (a->b) -> Either e a -> Either e b
    -- if there is an error do nothing
    fmap _ (Left y) = Left y
    -- if no error apply g
    fmap g (Right x) = Right (g x)

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap g (Just x) = Just $ g x

                       
-- instance Functor ((->) e) where
--     -- g :: a -> b
--     -- fmap :: (a->b) -> (e->a) -> (e->b)
--     fmap =  . 

--
-- ((->) String) (\s -> length s) (\s->s)
