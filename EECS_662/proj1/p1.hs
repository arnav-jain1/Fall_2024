{-# LANGUAGE GADTs, FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- AST Definition

data TABE where
  TNum :: TABE
  TBool :: TABE
  deriving (Show,Eq)

data ABE where
  Num :: Int -> ABE
  Plus :: ABE -> ABE -> ABE
  Minus :: ABE -> ABE -> ABE
  Mult :: ABE -> ABE -> ABE
  Div :: ABE -> ABE -> ABE
  Boolean :: Bool -> ABE
  And :: ABE -> ABE -> ABE
  Leq :: ABE -> ABE -> ABE
  IsZero :: ABE -> ABE
  If :: ABE -> ABE -> ABE -> ABE
  deriving (Show,Eq)

-- Evaluation Functions


evalM :: ABE -> Maybe ABE
evalM (Num n) = if n >= 0 then Just (Num n) else Nothing
evalM (Boolean b) = if not b || b then Just (Boolean b) else Nothing
evalM (Plus l r) = do { Num l' <- evalM l;
                        Num r' <- evalM r;
                        Just ( Num (  l' + r')) 
                      }
evalM (Minus l r) = do { Num l' <- evalM l;
                        Num r' <- evalM r;
                        if l' >= r' then Just ( Num (  l' + r')) else Nothing
                      }
evalM (Mult l r) = do { Num l' <- evalM l;
                        Num r' <- evalM r;
                        Just ( Num (  l' * r')) 
                      }

evalM (Div l r) = do { Num l' <- evalM l;
                       Num r' <- evalM r;
                       if r' == 0 then Nothing else Just ( Num (  l' `div` r')) 
                      }
evalM (And l r) = do { Boolean l <- evalM l; 
                       Boolean r <- evalM r;
                       Just ( Boolean (l && r))
                       }
evalM (Leq l r) = do { Boolean l <- evalM l; 
                       Boolean r <- evalM r;
                       Just ( Boolean (l <= r))
                       }
evalM (IsZero x) = do { Num x' <- evalM x;
                        if x' == 0 then Just (Boolean True) else Just (Boolean False)
                      }


-- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM _ = Nothing

-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM _ = Nothing

-- Optimizer

optimize :: ABE -> ABE
optimize e = e

evalOptM :: ABE -> Maybe ABE
evalOptM _ = Nothing
