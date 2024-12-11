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
  LocV :: Int -> FBAEVal
  deriving (Show,Eq)

type Sto = Loc -> Maybe FBAEVal

type Loc = Int
type Store = (Loc, Sto)

derefSto s l = (s l)

initSto :: Sto
initSto _ = Nothing

setSto :: Sto -> Loc -> FBAEVal -> Sto
setSto s l v = \m -> if l==m then Just v else (s m)

initStore :: Store
initStore = (-1, initSto)

derefStore :: Store -> Loc -> Maybe FBAEVal
derefStore (i, s) l = derefSto s l

setStore :: Store -> Loc -> FBAEVal -> Store
setStore (i, s) l v = (i, setSto s l v)

newStore :: Store -> FBAEVal -> Store
newStore (i, s) v = (i+1, (setSto s i v))



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
         
evalM :: Env -> Store -> FBAE -> (Maybe FBAEVal)
evalM _ _ (Num n) = if n >=0 then Just (NumV n) else Nothing
evalM _ _ (Boolean n) = if (n==True) || (n==False) then Just (BooleanV n) else Nothing

evalM e st (Plus l r) = do { NumV l' <- evalM e st l;
                          NumV r' <- evalM e st r;
                          Just (NumV (l' + r'))
                        }

evalM e st (Minus l r) = do { NumV l' <- evalM e st l;
                          NumV r' <- evalM e st r;
                          if l' >= r' then Just (NumV (l' - r')) else Nothing
                        }
evalM e st (Mult l r) = do { NumV l' <- evalM e st l;
                          NumV r' <- evalM e st r;
                          Just (NumV (l' * r'))
                        }
evalM e st (Div l r) = do { NumV l' <- evalM e st l;
                         NumV r' <- evalM e st r;
                         if r' == 0 then Nothing else Just (NumV (l' `div` r'))
                        }

evalM e st (Lambda i t s) = Just (ClosureV i s e)
evalM e st (Bind i a s) = do { va <- evalM e st a;
                            evalM ((i,va):e) st s
                         }
evalM e st (App f a) = do { (ClosureV i s e') <- evalM e st f;
                         va <- evalM e st a;
                         evalM ((i,va):e') st s
                       }


evalM e st (And l r) = do { BooleanV l' <- evalM e st l;
                          BooleanV r' <- evalM e st r;
                          Just (BooleanV (l' && r'))
                        }
evalM e st (Or l r) = do { BooleanV l' <- evalM e st l;
                          BooleanV r' <- evalM e st r;
                          Just (BooleanV (l' || r'))
                        }
evalM e st (Leq l r) = do { NumV l' <- evalM e st l;
                          NumV r' <- evalM e st r;
                          Just (BooleanV (l' <= r'))
                        }
evalM e st (IsZero n) = do { NumV n' <- evalM e st n;
                         Just (BooleanV (n' == 0))
                       }
evalM e st (If o t f) = do { BooleanV o' <- evalM e st o;
                         if o' then evalM e st t else evalM e st f;
                        }
evalM e st (Id s) = lookup s e

evalM e st (Fix f) = do { (ClosureV i s e') <- evalM e st f;
                       evalM e' st (subst i (Fix (Lambda i TNum s)) s)
                     }

evalM e st (New v) = do { va <- evalM e st v;
                          newStore st va
                        }

-- TODO: fix


-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ (Num n) = Just TNum
typeofM _ (Boolean n) = if n==True || n==False then Just TBool else Nothing
typeofM e (Plus l r) = do { TNum <- typeofM e l;
                            TNum <- typeofM e r;
                            Just TNum
                          }

typeofM e (Minus l r) = do { TNum <- typeofM e l;
                             TNum <- typeofM e r;
                             Just TNum
                          }
typeofM e (Mult l r) = do { TNum <- typeofM e l;
                            TNum <- typeofM e r;
                            Just TNum
                          }

typeofM e (Div l r) = do { TNum <- typeofM e l;
                             TNum <- typeofM e r;
                             Just TNum
                          }
typeofM e (And l r) = do { TBool<- typeofM e l;
                             TBool <- typeofM e r;
                             Just TBool
                          }
typeofM e (Or l r) = do { TBool<- typeofM e l;
                             TBool <- typeofM e r;
                             Just TBool
                          }
typeofM e (Leq l r) = do { TNum <- typeofM e l;
                            TNum <- typeofM e r;
                            Just TBool
                          }
typeofM e (IsZero n) = do { TNum <- typeofM e n;
                            Just TBool
                          }
typeofM e (If c t f) = do { TBool <- typeofM e c;
                            t' <- typeofM e t;
                            f' <- typeofM e f;
                            if t' == f' then Just t' else Nothing 
                          }
typeofM e (Bind i a s) = do { va <- typeofM e a;
                              typeofM ((i, va):e) s
                            }
typeofM e (Id s) = lookup s e

typeofM e (Lambda i t s) = do { s' <- typeofM ((i, t):e) s;
                                Just (t:->:s')
                              }

typeofM e (App f a) = do { (t:->:s) <- typeofM e f;
                            a' <- typeofM e a;
                            if a' == t then Just s else Nothing
                         }
typeofM e (Fix f) = do { (t:->:s) <- typeofM e f;
                         Just s
                       }


-- TODO: fix


-- Interpreter
interp :: FBAE -> Maybe FBAEVal
interp a = do { _ <- typeofM [] a;
                evalM [] initStore a
              }

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.
testFix = Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3))

--
-- Helper function to run the evaluator
runEval :: FBAE -> Maybe FBAEVal
runEval expr = evalM [] initStore expr

-- Test 1: Basic use of `New` to create a new location in the store
test1 :: FBAE
test1 = New (Num 42)

-- Expected Output: A `LocV` value representing the allocated location.

-- Test 2: Using `Deref` to retrieve the value from a location
test2 :: FBAE
test2 = Deref (New (Num 99))

-- Expected Output: `NumV 99`

-- Test 3: Combination of `New`, `Deref`, and binding
test3 :: FBAE
test3 = Bind "x" (New (Num 5)) (Deref (Id "x"))

-- Expected Output: `NumV 5`

-- Test 4: `New` with arithmetic operations
test4 :: FBAE
test4 = Bind "x" (New (Num 8)) (Plus (Deref (Id "x")) (Num 2))

-- Expected Output: `NumV 10`

-- Test 5: `Deref` on an invalid location
test5 :: FBAE
test5 = Deref (Num 1) -- Using a non-location value with `Deref`

-- Expected Output: `Nothing` (invalid dereference)

-- Test 6: `New` with a boolean value
test6 :: FBAE
test6 = Deref (New (Boolean True))

-- Expected Output: `BooleanV True`

-- Test 7: Nested `New` and `Deref`
test7 :: FBAE
test7 = Bind "x" (New (Num 10))
          (Bind "y" (New (Deref (Id "x")))
            (Deref (Id "y")))

-- Expected Output: `NumV 10`

-- Test 8: Updating a value using `Set` and retrieving it with `Deref`
test8 :: FBAE
test8 = Bind "x" (New (Num 15))
          (Bind "y" (Set (Id "x") (Num 20))
            (Deref (Id "x")))

-- Expected Output: `NumV 20`

-- Test 9: Using `Set` to modify a value and `Deref` to confirm the change
test9 :: FBAE
test9 = Bind "x" (New (Num 7))
          (Bind "y" (Set (Id "x") (Plus (Deref (Id "x")) (Num 3)))
            (Deref (Id "x")))

-- Expected Output: `NumV 10`

-- Test 10: Sequential use of `New` to ensure unique locations
test10 :: FBAE
test10 = Bind "x" (New (Num 1))
            (Bind "y" (New (Num 2))
              (Plus (Deref (Id "x")) (Deref (Id "y"))))

-- Expected Output: `NumV 3`


main :: IO ()
main = do
  let tests = [test1, test2, test3, test4, test5, test6, test7, test8, test9, test10]
      results = map runEval tests
  mapM_ print results
