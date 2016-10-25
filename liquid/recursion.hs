import Data.Vector

vectorSum         :: Vector Int -> Int
vectorSum vec     = go 0 0
  where
    go acc i
      | i < 5    = go (acc + 1) (i + 1)
      | otherwise = acc


-- nora@Nora-Work-Laptop:~/work/learning-haskell/liquid$ liquid -i include/  recursion.hs
-- LiquidHaskell Copyright 2009-15 Regents of the University of California. All Rights Reserved.


-- **** DONE:  A-Normalization ****************************************************
 

-- **** DONE:  Extracted Core using GHC *******************************************
 

-- **** DONE:  Transformed Core ***************************************************
 

-- **** DONE:  Uniqify & Rename ***************************************************
 
-- Done solving.
-- RESULT: Safe

-- **** DONE:  annotate ***********************************************************
 

-- **** RESULT: UNSAFE ************************************************************


--  /home/nora/work/learning-haskell/liquid/recursion.hs:(6,5)-(8,23): Error: Termination Error
--  go
--  No decreasing parameter
