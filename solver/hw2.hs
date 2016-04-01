module Main(main) where
    
    import Lexer
    import Parser
    import Exp
    import CNF
    import System.IO
    import Debug.Trace
    import Control.Monad.State

             
    run :: Exp -> Cnf
    run e = do
      CNF.convertToConjuctiveNormalForm e
                      
    main :: IO()
    main = do
      content <- getContents
      case (runAlex content solver) of
        Left s -> hPutStrLn stderr s
        Right c -> putStrLn $ show $ run c

    
