module Main(main) where
    
    import Lexer
    import Parser
    import System.IO
    import Debug.Trace
    import Control.Monad.State

    type Env = String -> Int

    extend :: String -> Int -> StateT Env IO ()
    extend name value =
        state $ \s -> 
                  ((),
                   (\var ->
                       if var == name
                       then value
                       else s $ var
                     ) )

    empty :: Env
    empty = \_ -> 0

    evalBinary :: (Int->Int->Int) -> AExp -> AExp -> StateT Env IO Int
    evalBinary op e1 e2 = do
                    n1 <- evalAexpr e1
                    n2 <- evalAexpr e2
                    return (op n1 n2)
            
    evalAexpr :: AExp -> StateT Env IO Int

    evalAexpr (Constant c) = return c
                             
    evalAexpr (Variable name) = state $ \s -> (s name, s)
                                                                  
    evalAexpr (Add e1 e2) = evalBinary (+) e1 e2

    evalAexpr (Minus e1 e2) = evalBinary (-) e1 e2
                              
    evalAexpr (Times e1 e2) = evalBinary (*) e1 e2
                                    
    evalAexpr (Paren e) = evalAexpr e
                          
    evalAexpr (Negate e) = do
                    n <- evalAexpr e
                    return (-n)

    evalBexpr :: BExp -> StateT Env IO Bool
                 
    evalBexpr Btrue = return True
                      
    evalBexpr Bfalse = return False
                       
    evalBexpr (Not bexp) = do
                    b <- evalBexpr bexp
                    return (not b)
                 
    evalBexpr (And bexp1 bexp2) = do
                    b1 <- evalBexpr bexp1
                    b2 <- evalBexpr bexp2
                    return (b1 && b2)               

    evalBexpr (Or bexp1 bexp2) = do
                    b1 <- evalBexpr bexp1
                    b2 <- evalBexpr bexp2
                    return (b1 || b2)         
                                       
    evalBexpr (Le aexp1 aexp2) = do
                    n1 <- evalAexpr aexp1
                    n2 <- evalAexpr aexp2
                    return (n1 <= n2)

    evalBexpr (Eqtest aexp1 aexp2) = do
                    n1 <- evalAexpr aexp1
                    n2 <- evalAexpr aexp2
                    return (n1 == n2)
                          
    eval :: Parser.Com -> StateT Env IO ()
            
    eval Skip = return ()

    eval (Print aexpr) = do
                    n <- evalAexpr aexpr
                    liftIO . putStr $ (show n) ++ " "

    eval (Let var aexpr com) = do
         oldValue <- evalAexpr (Variable var)
         newValue <- evalAexpr aexpr
         extend var newValue
         eval com
         extend var oldValue

    eval (Set var aexpr) = do
                    n <- evalAexpr aexpr
                    extend var n

    eval (Seq c1 c2) = do
        eval c1
        eval c2

    eval (Brace c) = eval c

    eval (If bexpr cthen celse) = do
                    b <- evalBexpr bexpr
                    if (b)
                    then eval cthen
                    else eval celse

    eval w@(While bexpr c) = do
                    b <- evalBexpr bexpr
                    if (b)
                    then
                        do
                          eval c 
                          eval w
                    else return ()
             
    run :: Parser.Com -> IO ()
    run c = do
      runStateT (eval c) empty
      return ()
                      
    main :: IO()
    main = do
      content <- getContents
      case (runAlex content imp) of
        Left s -> hPutStrLn stderr s
        Right c -> run c

    
