type Stack a = [a]

push :: Stack a -> a -> Stack a
push s a = a:s

pop :: Stack a -> (Maybe a , Stack a)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

empty :: Stack a
empty = []

replaceTop::Stack a -> (a -> a -> a) -> Stack a
replaceTop s op =
    let (x,s1) = pop s in
    let (y,s2) = pop s1 in
    case (x,y) of
      (_,Nothing) -> s1
      (Nothing,_) -> s1
      (Just x, Just y) -> push s1 (op x y)
        
main :: IO()

main = do
  let result = foldl (\ s elt ->
                          case elt of
                            "*" -> replaceTop s (*)
                            "+" -> replaceTop s (+)
                            "-" -> replaceTop s (-)
                            _ -> push s ((read elt)::Int)
                     ) (empty::Stack Int) (words "10 4 3 + 2 * -") 
  let (x,_) = (pop result)
  case x of
    Just y -> putStrLn $ show y
    Nothing -> return ()
