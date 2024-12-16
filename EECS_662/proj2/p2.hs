{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Imports for Monads

import Control.Monad

-- FAE AST and Type Definitions

data FAE where
  Num :: Int -> FAE
  Plus :: FAE -> FAE -> FAE
  Minus :: FAE -> FAE -> FAE
  Lambda :: String -> FAE -> FAE
  App :: FAE -> FAE -> FAE
  Id :: String -> FAE
  deriving (Show,Eq)

type Env = [(String,FAE)]

evalDynFAE :: Env -> FAE -> (Maybe FAE)
evalDynFAE _ (Num n) = if (n >= 0) then Just (Num n) else Nothing
evalDynFAE e (Plus l r) = do { Num l' <- evalDynFAE e l;
                               Num r' <- evalDynFAE e r;
                               Just (Num (l' + r'));
                             }
evalDynFAE e (Minus l r) = do { Num l' <- evalDynFAE e l;
                                Num r' <- evalDynFAE e r;
                                if (l' >= r') then Just (Num (l' - r')) else Nothing;
                              }
evalDynFAE e (Lambda i s) = Just (Lambda i s)
evalDynFAE e (Id s) = (lookup s e)
evalDynFAE e (App f a) = do { (Lambda i s) <- evalDynFAE e f;
                              va <- evalDynFAE e a;
                              evalDynFAE ((i, va):e) s
                            }



data FAEValue where
  NumV :: Int -> FAEValue
  ClosureV :: String -> FAE -> Env' -> FAEValue
  deriving (Show,Eq)
  
type Env' = [(String,FAEValue)]

evalStatFAE :: Env' -> FAE -> (Maybe FAEValue)
evalStatFAE _ (Num n) = if (n >= 0) then Just (NumV n) else Nothing
evalStatFAE e (Plus l r) = do { NumV l' <- evalStatFAE e l;
                                NumV r' <- evalStatFAE e r;
                                Just (NumV (l' + r'));
                             }
evalStatFAE e (Minus l r) = do { NumV l' <- evalStatFAE e l;
                                 NumV r' <- evalStatFAE e r;
                                 if (l' >= r') then Just (NumV (l' - r')) else Nothing;
                              }
evalStatFAE e (Lambda i s) = Just (ClosureV i s e)
evalStatFAE e (Id s) = (lookup s e)
evalStatFAE e (App f a) = do { (ClosureV i s e') <- evalStatFAE e f;
                              va <- evalStatFAE e a;
                              evalStatFAE ((i, va):e') s
                            }





-- FBAE AST and Type Definitions

data FBAE where
  NumD :: Int -> FBAE
  PlusD :: FBAE -> FBAE -> FBAE
  MinusD :: FBAE -> FBAE -> FBAE
  LambdaD :: String -> FBAE -> FBAE
  AppD :: FBAE -> FBAE -> FBAE
  BindD :: String -> FBAE -> FBAE -> FBAE
  IdD :: String -> FBAE
  deriving (Show,Eq)

elabFBAE :: FBAE -> FAE
elabFBAE (NumD v) = (Num v)
elabFBAE (PlusD l r) = (Plus (elabFBAE l) (elabFBAE r))
elabFBAE (MinusD l r) = (Minus (elabFBAE l) (elabFBAE r))
elabFBAE (LambdaD i s) = (Lambda i (elabFBAE s))
elabFBAE (IdD s) = (Id s)
elabFBAE (AppD f a) = App (elabFBAE f) (elabFBAE a)
elabFBAE (BindD i a s) = App (Lambda i (elabFBAE s)) (elabFBAE a)

evalFBAE :: Env' -> FBAE -> (Maybe FAEValue)
evalFBAE _ f = evalStatFAE [] (elabFBAE f);

-- evalFBAE _ (BindD (IdD "x") (NumD 4) (BindD (IdD "i") (LambdaD (IdD "n") (Plus (Id "n") (IdD "x"))) (BindD (IdD "x") (AppD (IdD "i") (IdD "x")) (AppD (IdD "i") (IdD "x")))))

-- FBAEC AST and Type Definitions

--data FBAEC where
  --NumE :: Int -> FBAEC
  --PlusE :: FBAEC -> FBAEC -> FBAEC
  --MinusE :: FBAEC -> FBAEC -> FBAEC
  --TrueE :: FBAEC
  --FalseE :: FBAEC
  --AndE :: FBAEC -> FBAEC -> FBAEC
  --OrE :: FBAEC -> FBAEC -> FBAEC
  --NotE :: FBAEC -> FBAEC
  --IfE :: FBAEC -> FBAEC -> FBAEC -> FBAEC
  --LambdaE :: String -> FBAEC -> FBAEC
  --AppE :: FBAEC -> FBAEC -> FBAEC BindE :: String -> FBAEC -> FBAEC -> FBAEC
  --IdE :: String -> FBAEC
  --deriving (Show,Eq)

--elabFBAEC :: FBAEC -> FAE
--elabFBAEC _ = (Num (-1))

--evalFBAEC :: Env' -> FBAEC -> Maybe FAEValue
--evalFBAEC _ _ = Nothing
