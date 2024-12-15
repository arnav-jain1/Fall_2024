{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables

data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
  TLoc :: TFBAE
  deriving (Show,Eq)

data FBAE where
  Num :: Int -> FBAE
  Plus :: FBAE -> FBAE -> FBAE
  Minus :: FBAE -> FBAE -> FBAE
  Mult :: FBAE -> FBAE -> FBAE
  Div :: FBAE -> FBAE -> FBAE
  Bind :: String -> FBAE -> FBAE -> FBAE
  Lambda :: String -> TFBAE -> FBAE -> FBAE
  App :: FBAE -> FBAE -> FBAE
  Id :: String -> FBAE
  Boolean :: Bool -> FBAE
  And :: FBAE -> FBAE -> FBAE
  Or :: FBAE -> FBAE -> FBAE
  Leq :: FBAE -> FBAE -> FBAE
  IsZero :: FBAE -> FBAE
  If :: FBAE -> FBAE -> FBAE -> FBAE
  Fix :: FBAE -> FBAE

  New :: FBAE -> FBAE
  Set :: FBAE -> FBAE -> FBAE
  Deref :: FBAE -> FBAE
  deriving (Show,Eq)

-- Value defintion for statically scoped eval

data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  Loc :: Int -> FBAEVal
  deriving (Show,Eq)

type Sto = Loc -> Maybe FBAEVal
type Loc = Int
type Store = (Loc, Sto)

derefSto s l = (s l)

initSto :: Sto
initSto _ = Nothing

setSto :: Sto -> Loc -> FBAEVal -> Sto
setSto s l v = \m -> if m==l then Just v else (s m)

initStore :: Store
initStore = (0, initSto)

derefStore :: Store -> Loc -> Maybe FBAEVal
derefStore (_, s) l = s l

setStore :: Store -> Loc -> FBAEVal -> Store
setStore (i, s) l v = (i, setSto s l v)

newStore :: Store -> Store
newStore (l, s) = (l+1, s)

type Retval = (FBAEVal, Store)

-- Substitution

subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = (Num x)
subst i v (Plus l r) = (Plus (subst i v l) (subst i v r))
subst i v (Minus l r) = (Minus (subst i v l) (subst i v r))
subst i v (Mult l r) = (Mult (subst i v l) (subst i v r))
subst i v (Div l r) = (Div (subst i v l) (subst i v r))
subst i v (Bind i' v' b') = if i==i'
                            then (Bind i' (subst i v v') b')
                            else (Bind i' (subst i v v') (subst i v b'))
subst i v (Lambda i' ty b') = if i==i'
                             then (Lambda i' ty b')
                             else (Lambda i' ty (subst i v b'))
subst i v (App f a) = (App (subst i v f) (subst i v a))
subst i v (Id i') = if i==i'
                    then v
                    else (Id i')
subst i v (Boolean b) = (Boolean b)
subst i v (And l r) = (And (subst i v l) (subst i v r))
subst i v (Or l r) = (Or (subst i v l) (subst i v r))
subst i v (Leq l r) = (Leq (subst i v l) (subst i v r))
subst i v (IsZero t) = (IsZero (subst i v t))
subst i v (If c l r) = (If (subst i v c) (subst i v l) (subst i v r))
subst i v (Fix t) = (Fix (subst i v t))




-- Enviornment for statically scoped eval

type Env = [(String,FBAEVal)]

-- Statically scoped eval
         
evalM :: Env -> Store -> FBAE -> Maybe Retval
evalM _ st (Num n) = if n >=0 then Just (NumV n, st) else Nothing
evalM _ st (Boolean n) = if (n==True) || (n==False) then Just (BooleanV n, st) else Nothing

evalM e st (Plus l r) = do { (NumV l', st') <- evalM e st l;
                             (NumV r', st'') <- evalM e st' r;
                             Just (NumV (l' + r'), st'')
                        }

evalM e st (Minus l r) = do { (NumV l', st') <- evalM e st l;
                              (NumV r', st'') <- evalM e st r;
                              if l' >= r' then Just (NumV (l' - r'), st'') else Nothing
                        }
evalM e st (Mult l r) = do { (NumV l', st') <- evalM e st l;
                             (NumV r', st'') <- evalM e st' r;
                             Just (NumV (l' * r'), st'')
                        }
evalM e st (Div l r) = do { (NumV l', st') <- evalM e st l;
                            (NumV r', st'') <- evalM e st' r;
                            if r' == 0 then Nothing else Just (NumV (l' `div` r'), st'')
                        }

evalM e st (Lambda i t s) = Just ((ClosureV i s e), st)

evalM e st (Bind i a s) = do { (va, st') <- evalM e st a;
                            evalM ((i,va):e) st' s
                         }
evalM e st (App f a) = do { ((ClosureV i s e'), st') <- evalM e st f;
                            (va, st'') <- evalM e st' a;
                            evalM ((i,va):e') st'' s
                       }


evalM e st (And l r) = do { (BooleanV l', st') <- evalM e st l;
                            (BooleanV r', st'') <- evalM e st' r;
                          Just (BooleanV (l' && r'), st'')
                        }
evalM e st (Or l r) = do { (BooleanV l', st') <- evalM e st l;
                           (BooleanV r', st'') <- evalM e st' r;
                           Just (BooleanV (l' || r'), st'')
                        }
evalM e st (Leq l r) = do { (NumV l', st')  <- evalM e st l;
                            (NumV r', st'') <- evalM e st' r;
                          Just (BooleanV (l' <= r'), st'')
                        }
evalM e st (IsZero n) = do { (NumV n', st') <- evalM e st n;
                         Just (BooleanV (n' == 0), st')
                       }

evalM e st (If o t f) = do { (BooleanV o', st') <- evalM e st o;
                             if o' then evalM e st' t else evalM e st' f;
                        }
evalM e st (Id s) = do {  (Loc v) <- lookup s e;
                          va <- (derefStore st v);
                          Just (va, st)
                       }

evalM e st (Fix f) = do { (ClosureV i va e', st') <- evalM e st f;
                       evalM e' st' (subst i (Fix (Lambda i TNum va)) va)
                     }
evalM e st (Set l v) = do { (va, st') <- evalM e st v;
                            (Loc l', st') <- evalM e st l;
                            Just (Loc l', (setStore st' l' va))
                          }

evalM e st (New v) = do { (va, (l, st')) <- evalM e st v;
                          Just ((Loc (l+1)), (setStore (newStore (l, st')) l va))
                        }
evalM e st (Deref l) = do { ((Loc l'), st') <- evalM e st l;
                            
                            v <- derefStore st l';
                            Just (v, st')
                          }


-- Helper function to run the evaluator
runEval :: FBAE -> Maybe FBAEVal
runEval expr = do { (a, _) <- evalM [] initStore expr;
                    Just a
                  }



-- Basic store allocation tests
test1 = runEval (New (Num 5))  -- Should create location 0 with value 5
test2 = runEval (Deref (New (Num 5)))  -- Should return value 5 from store

-- Basic arithmetic tests (with store allocation)
test3 = runEval (
  Bind "x" (New (Num 3)) (
    Bind "y" (New (Num 4)) (
      Plus (Deref (Id "x")) (Deref (Id "y"))
    )
  ))  -- Should return 7, with two locations allocated

test4 = runEval (
  Bind "x" (New (Num 10)) (
    Bind "y" (New (Num 3)) (
      Minus (Deref (Id "x")) (Deref (Id "y"))
    )
  ))  -- Should return 7, with two locations allocated

test5 = runEval (
  Bind "x" (New (Num 6)) (
    Bind "y" (New (Num 7)) (
      Mult (Deref (Id "x")) (Deref (Id "y"))
    )
  ))  -- Should return 42, with two locations allocated

-- Store manipulation tests
test6 = runEval (
  Bind "x" (New (Num 5)) (
    Bind "y" (New (Num 10)) (
      Set (Id "x") (Deref (Id "y"))  -- Set x to y's value
    )
  ))  -- Should update location of x to contain 10

-- Boolean and comparison tests
test7 = runEval (
  Bind "x" (New (Num 5)) (
    Bind "y" (New (Num 10)) (
      Leq (Deref (Id "x")) (Deref (Id "y"))
    )
  ))  -- Should return True, with two locations allocated

test8 = runEval (
  Bind "x" (New (Num 0)) (
    IsZero (Deref (Id "x"))
  ))  -- Should return True, with one location allocated

-- Conditional tests with store
test9 = runEval (
  Bind "x" (New (Num 0)) (
    If (IsZero (Deref (Id "x")))
      (New (Num 42))
      (New (Num 0))
  ))  -- Should allocate location for 0, then 42

-- Complex test with multiple store operations
test10 = runEval (
  Bind "x" (New (Num 5)) (
    Bind "y" (New (Num 10)) (
      If (Leq (Deref (Id "x")) (Deref (Id "y")))
        (Set (Id "x") (Plus (Deref (Id "x")) (Deref (Id "y"))))  -- Set x to x + y
        (Set (Id "x") (Minus (Deref (Id "x")) (Deref (Id "y"))))  -- Set x to x - y
    )
  ))  -- Should set x to 15

-- Factorial with store allocation
test11 = runEval (
  Bind "n" (New (Num 5)) (
    App (
      Fix (
        Lambda "f" (TNum :->: TNum) (
          Lambda "x" TNum (
            If (IsZero (Deref (Id "x")))
              (New (Num 1))
              (Bind "temp" (New (Num 1)) (
                Mult (Deref (Id "x")) 
                     (Deref (App (Id "f") (Minus (Deref (Id "x")) (Deref (New (Num 1))))))
              ))
          )
        )
      )
    ) (Id "n")
  ))  -- Should compute factorial of 5 (120), with multiple locations allocated

-- Error cases
test12 = runEval (
  Bind "x" (New (Num 5)) (
    Bind "y" (New (Num 0)) (
      Div (Deref (Id "x")) (Deref (Id "y"))  -- Division by zero
    )
  ))  -- Should return Nothing

test13 = runEval (
  Bind "x" (New (Num 3)) (
    Bind "y" (New (Num 5)) (
      Minus (Deref (Id "x")) (Deref (Id "y"))  -- Would result in negative number
    )
  ))  -- Should return Nothing

-- Test runner helper
runTests = do
  putStrLn "Running tests..."
  putStrLn $ "Test 1 (Basic store allocation): " ++ show test1
  putStrLn $ "Test 2 (Store dereference): " ++ show test2
  putStrLn $ "Test 3 (Addition with store): " ++ show test3
  putStrLn $ "Test 4 (Subtraction with store): " ++ show test4
  putStrLn $ "Test 5 (Multiplication with store): " ++ show test5
  putStrLn $ "Test 6 (Store update): " ++ show test6
  putStrLn $ "Test 7 (Comparison with store): " ++ show test7
  putStrLn $ "Test 8 (IsZero with store): " ++ show test8
  putStrLn $ "Test 9 (Conditional with store): " ++ show test9
  putStrLn $ "Test 10 (Complex store operations): " ++ show test10
  putStrLn $ "Test 11 (Factorial with store): " ++ show test11
  putStrLn $ "Test 12 (Division by zero error): " ++ show test12
  putStrLn $ "Test 13 (Negative result error): " ++ show test13

test_simple = runEval (Plus (New (Num 5)) (New (Num 6)))
