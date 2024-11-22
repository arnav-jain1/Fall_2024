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
         
evalM :: Env -> FBAE -> (Maybe FBAEVal)
evalM _ (Num n) = if n >=0 then Just (NumV n) else Nothing
evalM _ (Boolean n) = if (n==True) || (n==False) then Just (BooleanV n) else Nothing

evalM e (Plus l r) = do { NumV l' <- evalM e l;
                          NumV r' <- evalM e r;
                          Just (NumV (l' + r'))
                        }

evalM e (Minus l r) = do { NumV l' <- evalM e l;
                          NumV r' <- evalM e r;
                          if l' > r' then Just (NumV (l' - r')) else Nothing
                        }
evalM e (Mult l r) = do { NumV l' <- evalM e l;
                          NumV r' <- evalM e r;
                          Just (NumV (l' * r'))
                        }
evalM e (Div l r) = do { NumV l' <- evalM e l;
                          NumV r' <- evalM e r;
                          Just (NumV (l' `div` r'))
                        }

evalM e (Lambda i t s) = Just (ClosureV i s e)
evalM e (Bind i a s) = do { va <- evalM e a;
                            evalM ((i,va):e) s
                         }
evalM e (App f a) = do { (ClosureV i s e') <- evalM e f;
                         va <- evalM e a;
                         evalM ((i,va):e') s
                       }


evalM e (And l r) = do { BooleanV l' <- evalM e l;
                          BooleanV r' <- evalM e r;
                          Just (BooleanV (l' && r'))
                        }
evalM e (Or l r) = do { BooleanV l' <- evalM e l;
                          BooleanV r' <- evalM e r;
                          Just (BooleanV (l' || r'))
                        }
evalM e (Leq l r) = do { NumV l' <- evalM e l;
                          NumV r' <- evalM e r;
                          Just (BooleanV (l' <= r'))
                        }
evalM e (IsZero n) = do { NumV n' <- evalM e n;
                         Just (BooleanV (n' == 0))
                       }
evalM e (If o t f) = do { BooleanV o' <- evalM e o;
                         if o' then evalM e t else evalM e f;
                        }
evalM e (Id s) = lookup s e
-- TODO: fix


-- Type inference function

type Cont = [(String,TFBAE)]

typeofM :: Cont -> FBAE -> (Maybe TFBAE)
typeofM _ (Num n) = if n >=0 then Just TNum else Nothing
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
-- TODO: Lambda, app, id fix


-- Interpreter

interp :: FBAE -> (Maybe FBAEVal)
interp _ = Nothing

-- Factorial function for testing evalM and typeofM.  the type of test1 should
-- be TNum and the result of evaluating test1`should be (NumV 6).  Remember
-- that Just is used to return both the value and type.

test1 = (Bind "f" (Lambda "g" ((:->:) TNum TNum)
                    (Lambda "x" TNum (If (IsZero (Id "x")) (Num 1)
                                         (Mult (Id "x")
                                               (App (Id "g")
                                                    (Minus (Id "x")
                                                           (Num 1)))))))
         (App (Fix (Id "f")) (Num 3)))

