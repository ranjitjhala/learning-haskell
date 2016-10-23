import Data.List

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

generateProof :: [Hypothesis] -> Exp -> Maybe [Rule]
generateProof hypotheses goal =
  case generateProof1 hypotheses goal of
    Nothing -> Nothing
    Just (Proof _ _ rules) -> Just rules

generateProof1 :: [Hypothesis] -> Exp -> Maybe Proof
generateProof1 hypotheses goal =
  case find (\(Hypothesis name exp)-> exp==goal) hypotheses of
    Just _ -> Just (Proof hypotheses goal [])
    Nothing -> step hypotheses goal

    
step::[Hypothesis] -> Exp -> Maybe Proof
step hypotheses goal@(And p q) =
  -- check if last rule can be andIntro
  case generateProof1 hypotheses p of
    Nothing -> Nothing
    Just (Proof hyp1 _ rules1) ->
      case generateProof1 hyp1 q of
        Nothing -> Nothing
        Just (Proof hyp2 _ rules2) -> 
          -- add rule for AddIntro
          case (getHypothesisName hyp2 p,getHypothesisName hyp2 q) of
            (Just hypStr1, Just hypStr2) ->
              let conclStr = "H"++(show $ (length hyp2)) in
              let newHypList = hyp2 ++ [Hypothesis conclStr goal] in
              let newRules = rules1++rules2++[AndIntro hypStr1 hypStr2 conclStr] in
                Just (Proof newHypList goal newRules)
                      
            (_,_) -> Nothing
  -- TODO handle other cases here when the goal is a conjuction

step hypotheses goal@(Var name) =
  let pHyp = filter (\(Hypothesis _ expr) ->
                        contains expr goal) hypotheses in
    case find (\h -> isNotProcessedHyp hypotheses h) pHyp of
      Nothing -> Nothing
      Just (Hypothesis hName (And p q)) ->
        let pName =  "H"++(show $ (length hypotheses)) in
        let qName =  "H"++(show $ (length hypotheses) + 1) in
        -- create hyp p
        -- create hyp q
        -- add AndElimRight && AndElimLeft
        -- try again to generate proof with same goal
          case (generateProof1 (hypotheses++[(Hypothesis pName p),(Hypothesis qName q)]) goal) of
            Nothing -> Nothing
            Just (Proof newHyp _ newRules) -> Just (Proof newHyp goal
                                                    ([AndElimRight hName pName,
                                                      AndElimLeft hName qName]
                                                      ++newRules))
      Just (Hypothesis hName (Implies p q)) ->
        case (generateProof1 hypotheses p) of
          Nothing -> Nothing
          Just (Proof pHypList _ pRules) ->
            case (getHypothesisName pHypList p) of
              Nothing -> Nothing -- This should not happen TODO Add some error
              Just pName ->
                let qName =  "H"++(show $ (length pHypList)) in
                  case (generateProof1 (pHypList++[Hypothesis qName q]) goal) of
                    Nothing -> Nothing
                    Just (Proof qHypList _ qRules) ->
                      Just (Proof qHypList goal (pRules++[ImplElim pName hName qName]++qRules))
      --TODO deal with the rest of the cases

step hypotheses goal@(Implies p q) =
  let newHypName =  "H"++(show $ (length hypotheses)) in
    case (generateProof1 (hypotheses++[Hypothesis newHypName p]) q) of
      Nothing -> Nothing
      Just (Proof newHyps _ newRules) -> Just (Proof newHyps goal ([ModusPonens newHypName]++newRules))

step _ _ = undefined

contains::Exp->Exp->Bool
contains a@(Var _) e = a == e
contains (And a b) e =  (contains a e) || (contains b e)
contains (Or a b) e =  (contains a e) || (contains b e)
contains (Implies a b) e =  (contains a e) || (contains b e)
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
    case (generateProof hypotheses goal) of
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
  testAndIntro
  testAndIntroTwice
  testAndElim
  testImplElim
  testModusPonens
  test1
  test2
  -- test3
  -- test4
  -- test5


             

