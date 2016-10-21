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

test1 :: IO ()
test1 = 
  -- p /\ q => r, q => p, q
  -- Goal: r
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let hypList = [
        Hypothesis "H1" (Implies (And p q) r),
        Hypothesis "H2" (Implies q p),
        Hypothesis "H3" q
        ] in
  let rules = [
        ImplElim "H3" "H2" "H4",
          AndIntro "H4" "H3" "H5",
          ImplElim "H5" "H1" "H6"
        ]  in
    runTest hypList r rules

test2 :: IO()
test2 =
  -- (p->(q->r)) -> ( (p->q) -> (p->r) )
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let goal = (Implies
               (Implies p (Implies q r) ) -- (p->(q->r))
               (Implies (Implies p q) (Implies p r))) in -- ( (p->q) -> (p->r) )
  let rules = [
        ModusPonens "H1", -- (p->(q->r))
        ModusPonens "H2", -- p->q
        ModusPonens "H3", -- p
        ImplElim "H3" "H2" "H4", -- q
        ImplElim "H3" "H1" "H5", -- q -> r
        ImplElim "H4" "H5" "H6" ] in
    runTest [] goal rules

test3 :: IO()
test3 =
  -- ( (p->q) /\ (q->r) ) -> (p->r)
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let goal = Implies
        (And (Implies p q) (Implies q r))
        (Implies p r) in
  let rules = [
        ModusPonens "H1", --H1: (p->q) /\ (q->r); Goal p ->r
        ModusPonens "H2", --H2: p; Goal r
        AndElimLeft "H1" "H3", -- H3: q->r
        AndElimRight "H1" "H4", -- H4: p->q
        ImplElim "H2" "H4" "H5", -- H5: q
        ImplElim "H5" "H3" "H6" -- H6:r
              ] in
    runTest [] goal rules


test4 :: IO()
test4 =
  -- ( (p->q) /\ (r->s) /\ (p \/ r) ) -> (q \/ s)
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let s = Var "s" in
  let goal = (Implies
               (And (Implies p q ) (And (Implies r s) (Or p r))) -- ( (p->q) /\ (r->s) /\ (p \/ r) )
               (Or q s)) -- q \/ s
  in
  let rules = [
        ModusPonens "H1", -- H1: (p->q) /\ (r->s) /\ (p \/ r)  Goal: q \/ s
        AndElimLeft "H1" "H2", --H2:(r->s) /\ (p \/ r)
        AndElimRight "H1" "H3", --H3: p->q
        AndElimLeft "H2" "H4", --H4: p \/ r
        AndElimRight "H2" "H5", --H5:r->s
        SubProof (Implies p (Or q s)) -- 
          [
            ModusPonens "H7", -- H7:p
            ImplElim "H7" "H3" "H8", --H8:q
            OrIntroLeft "H8" s "H9" --H9:q\/s
          ] "H6", -- H6 p->(q\/s)
        SubProof (Implies r (Or q s))
          [
            ModusPonens "H10", --H10:r
            ImplElim "H10" "H5" "H11", --H11:s
            OrIntroRight "H11" q "H14" --H14: q\/s
          ] "H12", -- H12: r->(q\/s)
        OrElim "H4" "H6" "H12" "H13" --H13:q\/s
        ] in
    runTest [] goal rules

test5 :: IO()
test5 =
  -- ( (p->q) /\ (r->s) /\ (~q \/ ~s) ) -> (~p \/ ~r)
  let p = Var "p" in
  let q = Var "q" in
  let r = Var "r" in
  let s = Var "s" in
  let goal = (Implies
               (And (Implies p q ) (And (Implies r s) (Or (Not q) (Not s)))) -- ( (p->q) /\ (r->s) /\ (~q \/ ~s) )
               (Or (Not p) (Not r))) -- ~p \/ ~r
  in
  let rules = [
        ModusPonens "H1", -- H1:( (p->q) /\ (r->s) /\ (~q \/ ~s) ) Goal: ~p \/ ~r
        AndElimRight "H1" "H2", -- H2: p->q
        AndElimLeft "H1" "H3", -- H3: (r->s) /\ (~q \/ ~s)
        AndElimRight "H3" "H4", -- H4: r->s
        AndElimLeft "H3" "H5", -- H5: ~q \/ ~s
        SubProof (Implies (Not q) (Not p))
          [
            ModusPonens "H7", -- H7 ~q
            NegIntro "H7" "H2" "H8"
          ] "H6", -- H6: ~q -> ~p
        SubProof (Implies (Not s) (Not r))
          [
            ModusPonens "H9", -- H9 ~s
            NegIntro "H9" "H4" "H10"
          ] "H11", -- H11: ~s -> ~r
        SubProof (Implies (Not q) (Or (Not p) (Not r)))  
          [
            ModusPonens "H12", -- H12:~q
            ImplElim "H12" "H6" "H13", --H13: ~p
            OrIntroLeft "H13" (Not r) "H20" --H20:~p\/~r
          ] "H14", -- H14 ~q->(~p\/~r)
        SubProof (Implies (Not s) (Or (Not p) (Not r)))
          [
            ModusPonens "H15", --H15:~s
            ImplElim "H15" "H11" "H16", --H16:~r
            OrIntroRight "H16" (Not p) "H17" --H15: ~p\/~r
          ] "H18", -- H18: ~s->(~p\/~r)
        OrElim "H5" "H14" "H18" "H19" --H19:~p\/~r
        ] in
    runTest [] goal rules
    

runTest:: [Hypothesis] -> Exp -> [Rule] -> IO()
runTest hypList goal rules = do
  putStrLn "============================================================================"
  -- print hypothesis
  mapM_ (\h -> putStrLn (show h)) hypList
  putStrLn ("Goal:" ++ (show goal))
  result <- checkProof (Proof hypList goal rules)
  case result of
    Just (Proof hypList goal []) -> do
      putStrLn "CORRECT proof"
      mapM_ (\h -> putStrLn (show h)) hypList
    _ -> putStrLn "Proof FAILED"

          
main :: IO ()
main = do
  test1
  test2
  test3
  test4
  test5


             

