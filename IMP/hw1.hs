module Main(main) where
    
    import Lexer
    import Parser
    import System.IO
    import Debug.Trace

    type State = String -> Int

    extend :: State -> String -> Int -> State
    extend s name value =
        (\var ->
             if var == name
             then
                 value
             else s $ var )

    empty :: State
    empty = (\_ -> 0) 

    evalAexpr :: State-> AExp -> Int
    evalAexpr _ (Constant c) = c
    evalAexpr state (Variable name) = state name
    evalAexpr state (Add e1 e2) = (evalAexpr state e1) + (evalAexpr state e2)
    evalAexpr state (Minus e1 e2) = (evalAexpr state e1) - (evalAexpr state e2)
    evalAexpr state (Times e1 e2) = (evalAexpr state e1) * (evalAexpr state e2)
    evalAexpr state (Paren e) = evalAexpr state e
    evalAexpr state (Negate e) = -(evalAexpr state e)

    evalBexpr :: State -> BExp -> Bool
    evalBexpr state Btrue = True
    evalBexpr state Bfalse = False
    evalBexpr state (Not bexp) = not (evalBexpr state bexp)
    evalBexpr state (And bexp1 bexp2) = (evalBexpr state bexp1) && (evalBexpr state bexp2)
    evalBexpr state (Or bexp1 bexp2) = (evalBexpr state bexp1) || (evalBexpr state bexp2)
    evalBexpr state (Le aexp1 aexp2) = (evalAexpr state aexp1) <= (evalAexpr state aexp2)
    evalBexpr state (Eqtest aexp1 aexp2) = (evalAexpr state aexp1) == (evalAexpr state aexp2)
                          

    eval :: State -> Parser.Com -> IO State
            
    eval state Skip = return state

    eval state (Print aexpr) = do
                    putStr ((show (evalAexpr state aexpr)) ++ " ")
                    return state

    eval state (Let var aexpr com) =
         let oldValue = (state var) in
         let letState = (extend state var (evalAexpr state aexpr)) in
         do
           newState <- (eval letState com) 
           return (extend newState var oldValue)

    eval state (Set var aexpr) =
        return (extend state var (evalAexpr state aexpr))

    eval state (Seq c1 c2) = do
        state1 <- (eval state c1)
        eval state1 c2

    eval state (Brace c) =
        eval state c

    eval state (If bexpr cthen celse) =
        if (evalBexpr state bexpr)
        then eval state cthen
        else eval state celse

    eval state w@(While bexpr c) = 
        if (evalBexpr state bexpr)
        then
            do
              state1 <- eval state c 
              eval state1 w
        else return state
             
    run :: Parser.Com -> IO ()
    run c = do
      eval empty c
      return ()
                      
    main :: IO()
    main = do
      content <- getContents
      case (runAlex content imp) of
        Left s -> hPutStrLn stderr s
        Right c -> run c

    
