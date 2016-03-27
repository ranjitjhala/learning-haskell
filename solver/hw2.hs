module Main(main) where
    
    import Lexer
    import Parser
    import System.IO
    import Debug.Trace
    import Control.Monad.State

             
    run :: Parser.Exp -> IO ()
    run c = do
      --runStateT (eval c) empty
      return ()
                      
    main :: IO()
    main = do
      content <- getContents
      case (runAlex content solver) of
        Left s -> hPutStrLn stderr s
        Right c -> run c

    
