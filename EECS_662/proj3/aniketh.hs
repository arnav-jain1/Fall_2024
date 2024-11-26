
{-# LANGUAGE GADTs #-}

-- import Control.Monad

-- Calculator language extended with an environment to hold defined variables
data TFBAE where
  TNum :: TFBAE
  TBool :: TFBAE
  (:->:) :: TFBAE -> TFBAE -> TFBAE
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
  deriving (Show,Eq)

-- Value defintion for statically scoped eval
data FBAEVal where
  NumV :: Int -> FBAEVal
  BooleanV :: Bool -> FBAEVal
  ClosureV :: String -> FBAE -> Env -> FBAEVal
  deriving (Show,Eq)

-- Substitution
subst :: String -> FBAE -> FBAE -> FBAE
subst _ _ (Num x) = Num x
subst i v (Plus l r) = Plus (subst i v l) (subst i v r)
subst i v (Minus l r) = Minus (subst i v l) (subst i v r)
subst i v (Mult l r) = Mult (subst i v l) (subst i v r)
subst i v (Div l r) = Div (subst i v l) (subst i v r)
subst i v (Bind i' v' b') = if i==i'
                            then Bind i' (subst i v v') b'
                            else Bind i' (subst i v v') (subst i v b')
subst i v (Lambda i' ty b') = if i==i'
                             then Lambda i' ty b'
                             else Lambda i' ty (subst i v b')
subst i v (App f a) = App (subst i v f) (subst i v a)
subst i v (Id i') = if i==i'
                    then v
                    else Id i'
subst i v (Boolean b) = Boolean b
subst i v (And l r) = And (subst i v l) (subst i v r)
subst i v (Or l r) = Or (subst i v l) (subst i v r)
subst i v (Leq l r) = Leq (subst i v l) (subst i v r)
subst i v (IsZero t) = IsZero (subst i v t)
subst i v (If c l r) = If (subst i v c) (subst i v l) (subst i v r)
subst i v (Fix t) = Fix (subst i v t)

-- Enviornment for statically scoped eval
type Env = [(String,FBAEVal)]

-- Statically scoped eval
evalM :: Env -> FBAE -> Maybe FBAEVal
evalM e (Num x) = if x < 0 then Nothing else return (NumV x)
evalM e (Boolean b) = return (BooleanV b)
evalM e (Id x) = lookup x e
evalM e (Plus l r) = do { (NumV l') <- evalM e l;
                          (NumV r') <- evalM e r;
                          return (NumV (l' + r'))
                         }
evalM e (Minus l r) = do { (NumV l') <- evalM e l;
                           (NumV r') <- evalM e r;
                           if l' >= r' then return (NumV (l' - r')) else Nothing
                          }
evalM e (Mult l r) = do { (NumV l') <- evalM e l;
                          (NumV r') <- evalM e r;
                          return (NumV (l' * r'))
                         }
evalM e (Div l r) = do { (NumV l') <- evalM e l;
                         (NumV r') <- evalM e r;
                         if r' /= 0 then return (NumV (l' `div` r')) else Nothing
                        }

evalM e (And l r) = do { (BooleanV l') <- evalM e l;
                         (BooleanV r') <- evalM e r;
                         return (BooleanV (l' && r'))
                        }
evalM e (Or l r) = do { (BooleanV l') <- evalM e l;
                        (BooleanV r') <- evalM e r;
                        return (BooleanV (l' || r'))
                       }
evalM e (Leq l r) = do { (NumV l') <- evalM e l;
                         (NumV r') <- evalM e r;
                         return (BooleanV (l' <= r'))
                        }
evalM e (IsZero v) = do { (NumV v') <- evalM e v;
                          return (BooleanV (v' == 0))
                         }
evalM e (If c l r) = do { (BooleanV c') <- evalM e c;
                          if c' then evalM e l else evalM e r
                         }
evalM e (Lambda i _ s) = return (ClosureV i s e)
evalM e (App f a) = do { (ClosureV i s e') <- evalM e f;
                         a' <- evalM e a;
                         evalM ((i,a'):e') s
                        }
evalM e (Bind i v s) = do { v' <- evalM e v;
                            evalM ((i,v'):e) s
                           }
-- TODO: Idk what to do for the lambda type
evalM e (Fix f) = do { (ClosureV g b e') <- evalM e f;
                       evalM e' (subst g (Fix (Lambda g TNum b)) b)
                      }

-- Context for statically scoped type checking
type Cont = [(String,TFBAE)]

-- Type inference function
typeofM :: Cont -> FBAE -> Maybe TFBAE
typeofM c (Num _) = return TNum
typeofM c (Boolean _) = return TBool
typeofM c (Id x) = lookup x c
typeofM c (Plus l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum
                           }
typeofM c (Minus l r) = do { TNum <- typeofM c l;
                             TNum <- typeofM c r;
                             return TNum
                            }
typeofM c (Mult l r) = do { TNum <- typeofM c l;
                            TNum <- typeofM c r;
                            return TNum
                           }
typeofM c (Div l r) = do { TNum <- typeofM c l;
                           TNum <- typeofM c r;
                           return TNum
                          }
typeofM c (And l r) = do { TBool <- typeofM c l;
                           TBool <- typeofM c r;
                           return TBool
                          }
typeofM c (Or l r) = do { TBool <- typeofM c l;
                          TBool <- typeofM c r;
                          return TBool
                         }
typeofM c (Leq l r) = do { TNum <- typeofM c l;
                           TNum <- typeofM c r;
                           return TBool
                          }
typeofM c (IsZero l) = do { TNum <- typeofM c l;
                            return TBool
                           }
typeofM c (If t l r) = do { TBool <- typeofM c t;
                            tl <- typeofM c l;
                            tr <- typeofM c r;
                            if tl == tr then return tl else Nothing
                           }
typeofM c (Lambda i d s) = do { r <- typeofM ((i,d):c) s;
                                return (d :->: r)
                               }
typeofM c (App f a) = do { (d :->: r) <- typeofM c f;
                           ta <- typeofM c a;
                           if d == ta then return r else Nothing
                          }
typeofM c (Bind i v s) = do { tv <- typeofM c v;
                              typeofM ((i,tv):c) s
                             }
typeofM c (Fix f) = do { (d :->: r) <- typeofM c f;
                         return r
                        }

-- Interpreter
interp :: FBAE -> Maybe FBAEVal
interp a = do { _ <- typeofM [] a;
                evalM [] a
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

-- Tests for evaluation and interpretation
evalTests :: [(FBAE, Maybe FBAEVal)]
evalTests =
  [ -- Arithmetic operations
    (Plus (Num 3) (Num 5), Just (NumV 8)),
    (Minus (Num 10) (Num 5), Just (NumV 5)),
    (Minus (Num 3) (Num 5), Nothing), -- Edge case: subtraction with invalid condition
    (Mult (Num 4) (Num 2), Just (NumV 8)),
    (Div (Num 8) (Num 4), Just (NumV 2)),
    (Div (Num 8) (Num 0), Nothing), -- Edge case: division by zero

    -- Boolean operations
    (And (Boolean True) (Boolean False), Just (BooleanV False)),
    (Or (Boolean False) (Boolean True), Just (BooleanV True)),

    -- Relational operations
    (Leq (Num 4) (Num 4), Just (BooleanV True)),
    (Leq (Num 3) (Num 4), Just (BooleanV True)),
    (Leq (Num 5) (Num 4), Just (BooleanV False)),

    -- Zero check
    (IsZero (Num 0), Just (BooleanV True)),
    (IsZero (Num 3), Just (BooleanV False)),

    -- If-then-else
    (If (Boolean True) (Num 3) (Num 5), Just (NumV 3)),
    (If (Boolean False) (Num 3) (Num 5), Just (NumV 5)),

    -- Variable binding
    (Bind "x" (Num 3) (Id "x"), Just (NumV 3)),
    (Bind "x" (Num 3) (Plus (Id "x") (Num 5)), Just (NumV 8)),

    -- Lambda expressions
    (App (Lambda "x" TNum (Plus (Id "x") (Num 1))) (Num 5), Just (NumV 6)),
    (App (Lambda "x" TNum (Plus (Id "x") (Boolean True))) (Num 5), Nothing), -- Type mismatch

    -- Fixed-point operator
    (testFix, Just (NumV 6))
  ]

-- Tests for type checking
typeTests :: [(FBAE, Maybe TFBAE)]
typeTests =
  [ -- Arithmetic operations
    (Plus (Num 3) (Num 5), Just TNum),
    (Minus (Num 3) (Num 5), Just TNum),
    (Minus (Num 3) (Boolean True), Nothing), -- Edge case: type mismatch
    (Mult (Num 4) (Num 2), Just TNum),
    (Div (Num 8) (Num 4), Just TNum),
    (Div (Num 8) (Boolean False), Nothing), -- Type mismatch

    -- Boolean operations
    (And (Boolean True) (Boolean False), Just TBool),
    (Or (Boolean False) (Boolean True), Just TBool),
    (And (Num 3) (Boolean True), Nothing), -- Type mismatch

    -- Relational operations
    (Leq (Num 4) (Num 4), Just TBool),
    (Leq (Num 3) (Boolean True), Nothing), -- Type mismatch

    -- Zero check
    (IsZero (Num 0), Just TBool),
    (IsZero (Boolean True), Nothing), -- Type mismatch

    -- If-then-else
    (If (Boolean True) (Num 3) (Num 5), Just TNum),
    (If (Boolean True) (Num 3) (Boolean False), Nothing), -- Type mismatch
    (If (Num 3) (Num 3) (Num 5), Nothing), -- Type mismatch in condition

    -- Variable binding
    (Bind "x" (Num 3) (Id "x"), Just TNum),
    (Bind "x" (Num 3) (Plus (Id "x") (Num 5)), Just TNum),

    -- Lambda expressions
    (Lambda "x" TNum (Plus (Id "x") (Num 1)), Just (TNum :->: TNum)),
    (Lambda "x" TBool (Plus (Id "x") (Num 1)), Nothing), -- Type mismatch

    -- Application
    (App (Lambda "x" TNum (Plus (Id "x") (Num 1))) (Num 5), Just TNum),
    (App (Lambda "x" TNum (Plus (Id "x") (Num 1))) (Boolean True), Nothing), -- Type mismatch

    -- Fixed-point operator
    (testFix, Just TNum)
  ]

-- Assert the interpreted value is equal to the expected value
-- NOTE: I return calculated and expected value as well for diagnostics
assert :: Eq b => (a -> b) -> (a, b) -> (Bool, b, b)
assert fun (e, x) = (e'==x, e', x)
                      where e' = fun e

mapTests :: Eq b => (a -> b) -> [(a, b)] -> [(Bool,  b,  b)]
mapTests fun = map (assert fun)

test :: IO()
test = do
  putStr "evalM Tests: ";
  let results = mapTests (evalM []) evalTests;
  if all (\(x,_,_) -> x) results
    then putStrLn "Passed."
    else mapM_ print results

  putStr "typeofM Tests: ";
  let results = mapTests (typeofM []) typeTests;
  if all (\(x,_,_) -> x) results
    then putStrLn "Passed."
    else mapM_ print results

  putStr "interp Tests: ";
  let results = mapTests interp evalTests;
  if all (\(x,_,_) -> x) results
    then putStrLn "Passed."
    else mapM_ print results
