import Control.Monad.State
import Debug.Trace

type Stack a = [a]

push :: Show a => a ->State (Stack a) (Maybe a)
push x =
    Debug.Trace.trace ("Push " ++ (show x) ++ "\n")
    state $ \xs -> (Just x,x:xs)

pop :: Show a => State (Stack a) (Maybe a)
pop =
    state $ \lst -> case lst of
                      (x:xs) ->
                          Debug.Trace.trace ("Pop " ++ (show x) ++ "\n")
                          (Just x,xs)
                      [] ->
                          Debug.Trace.trace ("Pop empty\n")
                          (Nothing,[])

empty :: State (Stack a) (Maybe a)
empty = do
  put []
  return Nothing

replaceTop:: Show a => (a -> a -> a) -> State (Stack a) (Maybe a)
replaceTop op = do
    x1 <- pop
    x2 <- pop
    case (x1,x2) of
      (Just n1, Just n2) ->
          push (op n1 n2)
      (_,_) -> return Nothing

evalExpr::[String] -> State (Stack Int) (Maybe Int)
evalExpr [] =
    Debug.Trace.trace ("evalExpr: empty\n") 
    return Nothing
evalExpr (elt:rest) =
    do
      result <-
          case elt of
            "*" -> replaceTop (*)
            "+" -> replaceTop (+)
            "-" -> replaceTop (-)
            _ -> let x = (read elt)::Int in
                 push x
      case rest of
        [] -> return result
        _ -> evalExpr rest
                 
main :: IO()

main = do
  let (result,stack) = runState (evalExpr (words "10 4 3 + 2 * -")) []
  --let (result,stack) = runState (evalExpr (words "10 4 +")) []
  case result of
    Just y -> putStrLn $ show y
    Nothing -> putStrLn "Nothing"
