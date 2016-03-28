-- (set-logic QF_LIA)
-- (declare-const x Int)
-- (declare-const y Int)
-- (assert (= (- x y) 3))
-- (check-sat)
import Z3.Monad
import Data.Maybe
    
script :: Z3 (Maybe (Integer,Integer))
script = do
  x <- mkFreshIntVar "x"
  y <- mkFreshIntVar "y"
  _3 <- mkInteger 3
  f1 <- mkSub [x,y]
  f2 <- mkEq f1 _3
  assert f2
  -- withModel::(Model -> z3 a) -> z3 (Result, Maybe a)
  (result,ret) <- withModel $ \m -> do
                    xv <- evalInt m x
                    yv <- evalInt m y
                    return (fromJust xv, fromJust yv)
  case result of
    Sat -> return ret
    _ -> return Nothing
      
main :: IO ()
        -- evalZ3With :: Maybe Logic -> Opts -> Z3 a -> IO a
main = evalZ3With (Just QF_LIA) opts script >>= \mbSol ->
        case mbSol of
             Nothing  -> putStrLn "unsat"
             Just (x,y) -> putStrLn ((show x)++" "++(show y))
  where opts = opt "MODEL" True 
    --where opts = opt "print-success" True
