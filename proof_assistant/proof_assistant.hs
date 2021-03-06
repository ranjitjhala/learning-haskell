import Data.List
import Control.Monad.State
import System.IO.Unsafe

data Exp =
  Var String |
  And Exp Exp |
  Or Exp Exp |
  Implies Exp Exp |
  Not Exp
  deriving (Eq)

instance Show Exp where
  show (Var v) = v
  show (And e1 e2) = "("++ (show e1) ++ "/\\" ++ (show e2) ++ ")"
  show (Or e1 e2) = "("++ (show e1) ++ "\\/" ++ (show e2) ++ ")"
  show (Implies e1 e2) = "("++ (show e1) ++ "->" ++ (show e2) ++ ")"
  show (Not e) = "~"++ (show e)

-- Each hypothesis has a name and an expression
data Hypothesis = Hypothesis String Exp

instance Show Hypothesis where
  show (Hypothesis label expression) = label ++":"++ (show expression)

-- each rule specifies which hypothesis
-- to use and the last one is the new hypothesis created
data Rule =
  AndIntro String String String |
  AndElimRight String String |
  AndElimLeft String String |
  OrIntroLeft String Exp String |
  OrIntroRight String Exp String |
  OrElim String String String String |
  ModusPonens String|
  ImplElim String String String |
  NegIntro String String String |
  NegElim String String |
  SubProof Exp [Rule] String
  deriving (Show)

-- list of initial hypothesis
-- goal
-- sequence of rules in the proof
data Proof = Proof [Hypothesis] Exp [Rule]

errorHypNotFound :: String -> IO (Maybe Proof)
errorHypNotFound hypName = do
  putStrLn $ "Hypothesis " ++ hypName ++ " not found"
  return Nothing

findHypothesis :: [Hypothesis] -> String -> Maybe Exp
findHypothesis hypList name = 
  case ( Data.List.find (\(Hypothesis hname _ ) -> hname == name) hypList ) of
    Nothing -> Nothing
    Just (Hypothesis name e1) -> Just e1

applyRule :: Rule -> Proof -> IO (Maybe Proof)

-- checkProof :: Proof -> IO (Maybe Proof)
applyRule rule@(SubProof newGoal subRules hyp) (Proof hypList goal rules) = do
  putStrLn $ "Starting subproof with goal:" ++ (show newGoal) 
  result <- checkProof (Proof hypList newGoal subRules)
  putStrLn $ "Subproof ended"
  case result of
    Nothing -> do
      putStrLn $ "Incorrect subproof:" ++ (show rule)
      return Nothing
    _ -> return (Just (Proof (hypList++[(Hypothesis hyp newGoal)]) goal rules))

-- p
-- q
-- Concl: p /\q
applyRule rule@(AndIntro premise1 premise2 concl) (Proof hypList goal rules) = 
  case (findHypothesis hypList premise1) of
    Nothing -> errorHypNotFound premise1
    Just e1 ->
      case (findHypothesis hypList premise2) of
        Nothing -> errorHypNotFound premise2
        Just e2 -> do
          putStrLn $ show rule
          -- add a new hypothesis
          return (Just (Proof (hypList++[(Hypothesis concl (And e1 e2))]) goal rules))

-- p /\ q
-- Concl: p
applyRule rule@(AndElimRight premise concl) (Proof hypList goal rules) =
  case (findHypothesis hypList premise) of
    Nothing -> errorHypNotFound premise
    Just e ->
      case e of
        And p q ->  do
          putStrLn $ show rule
          -- add a new hypothesis
          return (Just (Proof (hypList++[(Hypothesis concl p)]) goal rules))
        _ -> do
          putStrLn $ "Incorrect rule " ++ (show rule)
          return Nothing

-- p /\ q
-- Concl: q
applyRule rule@(AndElimLeft premise concl) (Proof hypList goal rules) =
  case (findHypothesis hypList premise) of
    Nothing -> errorHypNotFound premise
    Just e ->
      case e of
        And p q ->  do
          putStrLn $ show rule
          -- add a new hypothesis
          return (Just (Proof (hypList++[(Hypothesis concl q)]) goal rules))
        _ -> do
          putStrLn $ "Incorrect rule " ++ (show rule)
          return Nothing

-- p
-- Concl: p \/ rightExp
applyRule rule@(OrIntroLeft premise rightExp concl) (Proof hypList goal rules) =
  case (findHypothesis hypList premise) of
    Nothing -> errorHypNotFound premise
    Just leftExp -> do
      putStrLn $ show rule
      -- add a new hypothesis
      return (Just (Proof (hypList++[(Hypothesis concl (Or leftExp rightExp))]) goal rules))

-- q
-- Concl: leftExp \/ q 
applyRule rule@(OrIntroRight premise leftExp concl) (Proof hypList goal rules) =
  case (findHypothesis hypList premise) of
    Nothing -> errorHypNotFound premise
    Just rightExp -> do
      putStrLn $ show rule
      -- add a new hypothesis
      return (Just (Proof (hypList++[(Hypothesis concl (Or leftExp rightExp))]) goal rules))

-- p1 p \/ q
-- p2 p -> r
-- p3 q -> r
-- concl r
applyRule rule@(OrElim pr1 pr2 pr3 concl) (Proof hypList goal rules) =
  let h1 = (findHypothesis hypList pr1) in
  let h2 = (findHypothesis hypList pr2) in
  let h3 = (findHypothesis hypList pr3) in
  case (h1,h2,h3)  of
    (Nothing,_,_) -> errorHypNotFound pr1
    (_,Nothing,_) -> errorHypNotFound pr2
    (_,_,Nothing) -> errorHypNotFound pr3
    (Just e1, Just e2, Just e3) ->
      case (e1,e2,e3) of
        (Or p1 q1,Implies p2 r2,Implies q3 r3) -> 
          if p1==p2 &&  q1==q3 && r2==r3
            then do
            putStrLn $ show rule
            -- add a new hypothesis
            return (Just (Proof (hypList++[(Hypothesis concl r2)]) goal rules))
            else do
            putStrLn $ "Incorrect rule " ++ (show rule)
            return Nothing
        _ -> do
          putStrLn $ "Incorrect rule " ++ (show rule)
          return Nothing

-- goal is p -> q
-- add p to hypotheses , change goal to q 
applyRule rule@(ModusPonens newHyp) (Proof hypList goal rules) = 
    case goal of
      Implies p q -> do
        putStrLn $ show rule
        return (Just (Proof (hypList++[(Hypothesis newHyp p)]) q rules))
      _ -> do
        putStrLn $ "Goal not and implication: " ++ (show rule)
        return Nothing
  
-- p
-- p-> q
-- Concl: q 
applyRule rule@(ImplElim premise1 premise2 concl) (Proof hypList goal rules) = 
  case (findHypothesis hypList premise1) of
    Nothing -> errorHypNotFound premise1
    Just p ->
      case (findHypothesis hypList premise2) of
        Nothing -> errorHypNotFound premise2
        Just piq -> do
          -- check piq is an implication
          case piq of
            Implies p1 q -> 
              case p1==p of
                True -> do
                  putStrLn $ show rule
                  return (Just (Proof (hypList++[(Hypothesis concl q)]) goal rules))
                False -> do
                  putStrLn $ "Incorrect rule " ++ (show rule)
                  return Nothing
            _ -> do
              putStrLn $ "Incorrect rule " ++ (show rule)
              return Nothing
-- ~q
-- p -> q
-- Concl: ~p
applyRule rule@(NegIntro premise1 premise2 concl) (Proof hypList goal rules) = 
  let h1 = (findHypothesis hypList premise1) in
  let h2 = (findHypothesis hypList premise2) in
  case (h1,h2) of
    (Nothing,_) -> errorHypNotFound premise1
    (_,Nothing) -> errorHypNotFound premise2
    (Just (Not q1), Just (Implies p2 q2)) ->
      if q1 == q2 then do
        putStrLn $ show rule
        return (Just (Proof (hypList++[(Hypothesis concl (Not p2))]) goal rules))
      else do
        putStrLn $ "Incorrect rule " ++ (show rule)
        return Nothing
    (_,_) -> do
      putStrLn $ "Incorrect rule " ++ (show rule)
      return Nothing

-- ~~p
-- Concl p
applyRule rule@(NegElim premise concl) (Proof hypList goal rules) =
  case (findHypothesis hypList premise) of
    Nothing -> errorHypNotFound premise
    Just (Not (Not p)) -> do
      putStrLn $ show rule
      return (Just (Proof (hypList++[(Hypothesis concl p)]) goal rules))
    _ -> do
      putStrLn $ "Incorrect rule " ++ (show rule)
      return Nothing

checkProof :: Proof -> IO (Maybe Proof)
checkProof proof@(Proof hyp goal []) = do
  -- check if the goal is one of the hypothesis
  case (Data.List.any (\(Hypothesis _ exp) -> exp == goal) hyp) of
    True -> return (Just proof)
    False -> return Nothing

checkProof (Proof hypList goal (rule:rulesList)) = do
  newProof <- applyRule rule (Proof hypList goal rulesList)
  case newProof of
    Nothing -> return Nothing
    Just p -> checkProof p

--------------------------------------------------------------------
-- end of proof checking
--------------------------------------------------------------------

buildHypotheses :: [Exp] -> [Hypothesis]
buildHypotheses exprs =
  let (_,l) = foldl (\(counter,acc) exp ->
                       (counter+1,(Hypothesis ("H"++(show counter)) exp):acc)) (0,[]) exprs in l

getHypothesisName::[Hypothesis] -> Exp -> Maybe String
getHypothesisName hypotheses exp =
  case find (\(Hypothesis name e)-> e==exp) hypotheses of
    Just (Hypothesis name _) -> Just name
    Nothing -> Nothing

generateHypothesisName :: State Int String
generateHypothesisName = do
  n <- get
  put (n+1)
  return ("H"++(show n))
 
generateProof :: [Hypothesis] -> Exp -> State Int (Maybe [Rule])
generateProof hypotheses goal = do
  result <- generateProof1 hypotheses goal
  case result of
    Nothing -> return Nothing
    Just (Proof _ _ rules) -> return (Just rules)

generateProof1 :: [Hypothesis] -> Exp -> State Int (Maybe Proof)
generateProof1 hypotheses goal =
  case find (\(Hypothesis name exp)-> exp==goal) hypotheses of
    Just _ -> do return (Just (Proof hypotheses goal []))
    Nothing -> step hypotheses goal

tryAddIntro::[Hypothesis] -> Exp -> Exp -> State Int (Maybe Proof)
tryAddIntro hypotheses p q = do
  result1 <- generateProof1 hypotheses p
  case result1 of
    Nothing -> do return Nothing
    Just (Proof hyp1 _ rules1) -> do
      result2 <- generateProof1 hyp1 q
      case result2 of
        Nothing -> return Nothing
        Just (Proof hyp2 _ rules2) -> 
          -- add rule for AddIntro
          case (getHypothesisName hyp2 p,getHypothesisName hyp2 q) of
            (Just hypStr1, Just hypStr2) -> do
              conclStr <- generateHypothesisName
              let newHypList = hyp2 ++ [Hypothesis conclStr (And p q)] in
                let newRules = rules1++rules2++[AndIntro hypStr1 hypStr2 conclStr] in
                  return (Just (Proof newHypList (And p q)  newRules))          
            (_,_) -> do return Nothing --TODO this is an error

doAndElimination::[Hypothesis]->String->Exp->Exp->Exp->State Int (Maybe Proof)
doAndElimination hypotheses hName p q goal= do
  pName <- generateHypothesisName
  qName <- generateHypothesisName
  -- create hyp p
  -- create hyp q
  -- add AndElimRight && AndElimLeft
  -- try again to generate proof with same goal
  result <- generateProof1 (hypotheses++[(Hypothesis pName p),(Hypothesis qName q)]) goal
  case result of
    Nothing ->do return Nothing
    Just (Proof newHyp _ newRules) -> do
      return( Just (Proof newHyp goal
                     ([AndElimRight hName pName,
                        AndElimLeft hName qName]
                       ++newRules)))

doImplElim :: [Hypothesis] -> String -> Exp -> Exp -> Exp -> State Int (Maybe Proof)
doImplElim hypotheses hName p q goal = do
  result1 <- generateProof1 hypotheses p
  case result1 of
    Nothing -> return Nothing
    Just (Proof pHypList _ pRules) ->
      case (getHypothesisName pHypList p) of
        Nothing -> return Nothing -- This should not happen TODO Add some error
        Just pName -> do
          qName <- generateHypothesisName
          result2 <- generateProof1 (pHypList++[Hypothesis qName q]) goal
          case result2 of
            Nothing ->return Nothing
            Just (Proof qHypList _ qRules) ->
              return (Just (Proof qHypList goal (pRules++[ImplElim pName hName qName]++qRules)))

doOrElim :: [Hypothesis] -> String -> Exp -> Exp -> Exp -> State Int (Maybe Proof) 
doOrElim allHypotheses hName p q goal = do
  -- have Hypothesis hName p\/q
  -- look for Hypothesis p->r and q->r
  return Nothing -- TODO

tryOrIntroLeft :: [Hypothesis] -> Exp -> Exp -> Exp -> State Int (Maybe Proof)
tryOrIntroLeft hypotheses p q goal= do
  resultP <- generateProof1 hypotheses p
  case resultP of
    Nothing -> return Nothing
    Just (Proof pHypotheses _ pRules) -> do
      newHypName <- generateHypothesisName
      case (getHypothesisName pHypotheses p) of
        Nothing -> return Nothing -- TODO error
        Just pName ->
          return (Just (Proof (pHypotheses++[Hypothesis newHypName (Or p q)])
                         goal (pRules++[OrIntroLeft pName q newHypName])))

tryOrIntroRight :: [Hypothesis] -> Exp -> Exp -> Exp -> State Int (Maybe Proof)
tryOrIntroRight hypotheses p q goal= do
  resultQ <- generateProof1 hypotheses q
  case resultQ of
    Nothing -> return Nothing
    Just (Proof qHypotheses _ qRules) -> do
      newHypName <- generateHypothesisName
      case (getHypothesisName qHypotheses q) of
        Nothing -> return Nothing -- TODO error
        Just qName ->
          return (Just (Proof (qHypotheses++[Hypothesis newHypName (Or p q)])
                         goal (qRules++[OrIntroLeft qName p newHypName])))

tryHypothesis::[Hypothesis]->[Hypothesis]-> Exp ->State Int (Maybe Proof)
tryHypothesis allHypotheses worklist goal= do
  case find (\h -> isNotProcessedHyp allHypotheses h) worklist of
    Nothing -> do return Nothing
    Just (Hypothesis hName (And p q)) -> 
      doAndElimination allHypotheses hName p q goal
    Just (Hypothesis hName (Implies p q)) ->
      doImplElim allHypotheses hName p q goal
    Just (Hypothesis hName (Not (Not p))) -> do
      newHypName <- generateHypothesisName
      return (Just (Proof (allHypotheses++[Hypothesis newHypName p])
                    goal [NegElim hName newHypName]))
    Just (Hypothesis hName (Or p q)) ->
      doOrElim allHypotheses hName p q goal


step::[Hypothesis] -> Exp -> State Int (Maybe Proof)
step hypotheses goal@(And p q) = do
  -- check if last rule can be andIntro
  result <- tryAddIntro hypotheses p q 
  case result of
    Just proof -> return (Just proof)
    Nothing -> do
      resultH <- tryHypothesis hypotheses hypotheses goal
      case resultH of
        Just (Proof hypothesesH _ rulesH) -> do
          newResult <- generateProof1 hypothesesH goal
          case newResult of 
            Nothing -> return Nothing
            Just (Proof newHyp _ newRules) -> 
              return (Just (Proof newHyp goal (rulesH++newRules)))
                              
  -- TODO handle other cases here when the goal is a conjuction
  
step hypotheses goal@(Var name) =
  let pHyp = filter (\(Hypothesis _ expr) ->
                        contains expr goal) hypotheses in
      tryHypothesis hypotheses pHyp goal  
      --TODO deal with the rest of the cases

step hypotheses goal@(Implies p q) = do
  newHypName <- generateHypothesisName
  result <- generateProof1 (hypotheses++[Hypothesis newHypName p]) q
  case result of
    Nothing -> return Nothing
    Just (Proof newHyps _ newRules) -> return (Just (Proof newHyps goal ([ModusPonens newHypName]++newRules)))

step hypotheses goal@(Or p q) = do
  resultP <- tryOrIntroLeft hypotheses p q goal
  case resultP of
    Just proof -> do return (Just proof)
    Nothing -> do
      resultQ <- tryOrIntroRight hypotheses p q goal
      case resultQ of
        Just proof -> return (Just proof)
        Nothing -> do
          resultH <- tryHypothesis hypotheses hypotheses goal
          case resultH of
            Nothing -> return Nothing
            Just (Proof hypothesesH _ rulesH) -> do
              newResult <- generateProof1 hypothesesH goal -- TODO check this code
              case newResult of 
                Nothing -> return Nothing
                Just (Proof newHyp _ newRules) -> 
                  return (Just (Proof newHyp goal (rulesH++newRules)))
      

step _ _ = undefined

contains::Exp->Exp->Bool
contains a@(Var _) e = a == e
contains (And a b) e =  (contains a e) || (contains b e)
contains (Or a b) e =  (contains a e) || (contains b e)
contains (Implies a b) e = contains b e
  --(contains a e) || (contains b e)
contains (Not a) e = contains a e

isNotProcessedHyp :: [Hypothesis] -> Hypothesis -> Bool
isNotProcessedHyp hypList (Hypothesis _ (And p q)) =
  case (getHypothesisName hypList p, getHypothesisName hypList q) of
    (Nothing,_) -> True
    (_,Nothing) -> True
    (Just _, Just _) -> False

isNotProcessedHyp hypList (Hypothesis _ (Implies p q)) =
  case (getHypothesisName hypList p, getHypothesisName hypList q) of
    (Nothing,_) -> True
    (_,Nothing) -> True
    (Just _, Just _) -> False

isNotProcessedHyp hypList (Hypothesis _ (Not (Not p))) =
  case getHypothesisName hypList p of
    Nothing -> True
    _ -> False

isNotProcessedHyp hypList (Hypothesis _ (Or _ _)) = True -- TODO Think of this more
  

-- TODO finish testHyp


--------------------------------------------------------------------
-- end of proof generation
--------------------------------------------------------------------

testAndIntro :: IO()
testAndIntro =
  let p = Var "p" in
  let q = Var "q" in
    runTest [p,q] (And p q)

testAndIntroTwice :: IO()
testAndIntroTwice =
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
    runTest [p,q,r] (And (And p q) r)

testAndElim :: IO()
testAndElim =
  let p = Var "p" in
  let q = Var "q" in
    runTest [(And p q)] p

testImplElim :: IO()
testImplElim = 
  let p = Var "p" in
  let q = Var "q" in
    runTest [(Implies p q),p] q

testModusPonens :: IO()
testModusPonens = 
  let p = Var "p" in
  let q = Var "q" in
    runTest [q] (Implies p q)

testOrIntroLeft :: IO ()
testOrIntroLeft = 
  let p = Var "p" in
  let q = Var "q" in
    runTest [p] (Or p q)

testOrIntroRight :: IO ()
testOrIntroRight = 
  let p = Var "p" in
  let q = Var "q" in
    runTest [p] (Or p q)

test6 :: IO ()
test6 = 
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
    runTest [And (Or p q) r] (Or p q)
    
test1 :: IO ()
test1 = 
  -- p /\ q => r, q => p, q
  -- Goal: r
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let hypList = [
        Implies (And p q) r,
        Implies q p,
        q
        ] in
  let goal = r in
    runTest hypList goal

test2 :: IO()
test2 =
  -- (p->(q->r)) -> ( (p->q) -> (p->r) )
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let goal = (Implies
               (Implies p (Implies q r) ) -- (p->(q->r))
               (Implies (Implies p q) (Implies p r))) in -- ( (p->q) -> (p->r) )
    runTest [] goal

test3 :: IO()
test3 =
  --( (p->q) /\ (q->r) ) -> (p->r)
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let goal = Implies
        (And (Implies p q) (Implies q r))
        (Implies p r) in
    runTest [] goal


test4 :: IO()
test4 =
  --( (p->q) /\ (r->s) /\ (p \/ r) ) -> (q \/ s)
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let s = Var "s" in
  let goal = (Implies
               (And (Implies p q ) (And (Implies r s) (Or p r))) --( (p->q) /\ (r->s) /\ (p \/ r) )
               (Or q s)) --q \/ s
  in
    runTest [] goal

-- test5 :: IO()
-- test5 =
--   -- ( (p->q) /\ (r->s) /\ (~q \/ ~s) ) -> (~p \/ ~r)
--   let p = Var "p" in
--   let q = Var "q" in
--   let r = Var "r" in
--   let s = Var "s" in
--   let goal = (Implies
--                (And (Implies p q ) (And (Implies r s) (Or (Not q) (Not s)))) -- ( (p->q) /\ (r->s) /\ (~q \/ ~s) )
--                (Or (Not p) (Not r))) -- ~p \/ ~r
--   in
--   let rules = generateProof hypList goal  in
--     runTest [] goal rules
    

runTest:: [Exp] -> Exp -> IO()
runTest hypList goal = do
  putStrLn "============================================================================"
  let hypotheses = (buildHypotheses hypList) in do
    mapM_ (\h -> putStrLn (show h)) hypotheses
    putStrLn ("Goal:" ++ (show goal))
    case (evalState (generateProof hypotheses goal) (length hypList)) of
      Nothing -> putStrLn "Proof generation failed"
      Just rules  -> do
        result <- checkProof (Proof hypotheses goal rules)
        case result of
          Just (Proof hypList goal []) -> do
            putStrLn "CORRECT proof"
            mapM_ (\h -> putStrLn (show h)) hypList
          _ -> putStrLn "Proof FAILED"
            
main :: IO ()
main = do
  -- testAndIntro
  -- testAndIntroTwice
  -- testAndElim
  -- testImplElim
  -- testModusPonens
  -- test1
  -- test2
  -- testOrIntroLeft
  -- testOrIntroRight
  -- test6
  -- test3
  test4
  -- test5


             

