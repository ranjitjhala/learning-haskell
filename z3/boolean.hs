-- Want to implement:
-- (set-option :print-success false)
-- (set-logic QF_UF)
-- (declare-const p Bool)
-- (assert (and p (not p))) 
-- (check-sat) ; returns 'unsat'
-- (exit)
import Z3.Monad

script :: Z3 Result
script = do
  p <- mkFreshBoolVar "p"
  notp <- mkNot p
  formula <- mkAnd [p, notp]
  assert formula
  check 
      
main :: IO ()
        -- evalZ3With :: Maybe Logic -> Opts -> Z3 a -> IO a
main = evalZ3With (Just QF_UF) opts script >>= \mbSol ->
        case mbSol of
             Sat  -> putStrLn "Sat"
             Unsat -> putStrLn "Unsat"
             Undef -> putStrLn "Undef"
  where opts = opt "MODEL" True 
    --where opts = opt "print-success" True
