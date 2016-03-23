import Control.Monad.State

-- newtype State s a = State { runState :: s -> (a,s) }

-- instance Monad (State s) where  
--     return x = State $ \s -> (x,s)  
--     (State h) >>= f = State $ \s -> let (a, newState) = h s  
--                                         (State g) = f a  
--                                     in  g newState 

type Stack = [Int] 

pop :: State Stack Int
pop = state $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push x = state $ \xs -> ((),x:xs)

stackManip :: State Stack Int  
-- stackManip = do  
--     push 3  
--     a <- pop  
--     pop
stackManip =
    (push 3) >>= (\_ ->
                      pop  >>= ( \a -> pop)
                 )
    
 
-- runState stackManip [5,8,2,1] 
