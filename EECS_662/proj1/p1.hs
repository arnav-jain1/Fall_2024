{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant ==" #-}
{-# HLINT ignore "Redundant bracket" #-}

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
                        if l' >= r' then Just ( Num (  l' - r')) else Nothing
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
evalM (Leq l r) = do { Num l <- evalM l; 
                       Num r <- evalM r;
                       Just ( Boolean (l <= r))
                       }
evalM (IsZero x) = do { Num x' <- evalM x;
                        if x' == 0 then Just (Boolean True) else Just (Boolean False)
                      }
evalM (If s l r) = do { Boolean s' <- evalM s;
                        if s' then evalM l else evalM r
                       }


-- Replace this with your interpreter

-- Type Derivation Function

typeofM :: ABE -> Maybe TABE
typeofM (Num a) = if a >= 0 then Just TNum else Nothing
typeofM (Boolean b) = if b == True || b == False then Just TBool else Nothing
typeofM (Plus l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TNum && r' == TNum then Just TNum else Nothing 
                        }
typeofM (Minus l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TNum && r' == TNum then Just TNum else Nothing 
                         }

typeofM (Mult l r) = do { TNum <- typeofM l;
                          TNum <- typeofM r;
                          Just TNum
                         }
typeofM (Div l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TNum && r' == TNum then Just TNum else Nothing 
                       }
typeofM (And l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TBool && r' == TBool then Just TBool else Nothing 
                         }
typeofM (Leq l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TNum && r' == TNum then Just TBool else Nothing 
                       }
typeofM (IsZero x) = do { x' <- typeofM x;
                          if x' == TNum then Just TNum else Nothing 
                       }
typeofM (If s l r) = do { TBool <- typeofM s;
                          l' <- typeofM l;
                          r' <- typeofM r;
                        if r' == l' then Just r' else Nothing
                       }
-- Combined interpreter

evalTypeM :: ABE -> Maybe ABE
evalTypeM a = do { a' <- typeofM a;
                   evalM a
                 }

-- Optimizer

optimize :: ABE -> ABE
optimize (Num n) = Num n
optimize (Boolean b) = Boolean b
optimize (Plus l r) 
  | optimize (l) == Num 0 = optimize r
  | optimize (r) == Num 0 = optimize l
  | otherwise = Plus (optimize l) (optimize r)
optimize (Minus l r) 
  | optimize (l) == Num 0 = optimize r
  | optimize (r) == Num 0 = optimize l
  | otherwise = Minus (optimize l) (optimize r)
optimize (Mult l r) = (Mult (optimize l) (optimize r))
optimize (Div l r) = (Div (optimize l) (optimize r))
optimize (And l r) = And (optimize l) (optimize r)
optimize (Leq l r) = Leq (optimize l) (optimize r)
optimize (IsZero x) = IsZero (optimize x)
optimize (If s l r) 
  | optimize (s) == Boolean True = optimize l
  | optimize (s) == Boolean False = optimize r

evalOptM :: ABE -> Maybe ABE
evalOptM s = evalM ( optimize s)
-- Test cases for ABE Interpreter

main :: IO ()
main = do
    putStrLn "Running tests for ABE Interpreter..."
    
    
    -- Regular tests
    testEvalOptim (Plus (Num 1) (Num 2)) (Just (Num 3))
    testEvalOptim (Minus (Num 5) (Num 2)) (Just (Num 3))
    testEvalOptim (Mult (Num 3) (Num 4)) (Just (Num 12))
    testEvalOptim (Div (Num 10) (Num 2)) (Just (Num 5))
    testEvalOptim (Div (Num 10) (Num 0)) Nothing -- Dive by 0
    testEvalOptim (And (Boolean True) (Boolean False)) (Just (Boolean False))
    testEvalOptim (Leq (Num 1) (Num 2)) (Just (Boolean True))
    testEvalOptim (IsZero (Num 0)) (Just (Boolean True))
    testEvalOptim (If (Boolean True) (Num 1) (Num 0)) (Just (Num 1))

    -- Edge cases for arithmetic operations
    testEvalOptim (Minus (Num 2) (Num 3)) Nothing  -- negative result
    testEvalOptim (Div (Num 5) (Num 2)) (Just (Num 2))  -- int division

    -- Edge cases for boolean operations
    testEvalOptim (And (Boolean True) (Num 1)) Nothing  -- Type mismatch
    testEvalOptim (Leq (Boolean False) (Boolean True)) Nothing  -- Type mismatch

    -- Edge cases for IsZero
    testEvalOptim (IsZero (Boolean True)) Nothing  -- Type mismatch
    testEvalOptim (IsZero (Num (-1))) Nothing  -- Negative number

    
    -- Same but for type 
    testEvalType (Plus (Num 1) (Num 2)) (Just (Num 3))
    testEvalType (Minus (Num 5) (Num 2)) (Just (Num 3))
    testEvalType (Mult (Num 3) (Num 4)) (Just (Num 12))
    testEvalType (Div (Num 10) (Num 2)) (Just (Num 5))
    testEvalType (Div (Num 10) (Num 0)) Nothing -- div by 0
    testEvalType (And (Boolean True) (Boolean False)) (Just (Boolean False))
    testEvalType (Leq (Num 1) (Num 2)) (Just (Boolean True))
    testEvalType (IsZero (Num 0)) (Just (Boolean True))
    testEvalType (If (Boolean True) (Num 1) (Num 0)) (Just (Num 1))

    -- Edge cases for arithmetic operations
    testEvalType (Minus (Num 2) (Num 3)) Nothing  -- negative result
    testEvalType (Div (Num 5) (Num 2)) (Just (Num 2))  -- int div

    -- Edge cases for boolean operations
    testEvalType (And (Boolean True) (Num 1)) Nothing  -- Type mismatch
    testEvalType (Leq (Boolean False) (Boolean True)) Nothing  -- Type mismatch

    -- Edge cases for IsZero
    testEvalType (IsZero (Boolean True)) Nothing  -- Type mismatch
    testEvalType (IsZero (Num (-1))) Nothing  -- Negative number

    -- Mismatching if 
    testEvalType (If (Boolean True) (Num 1) (Boolean False)) (Nothing)  -- Different types in branches

    -- Type checking tests
    testTypeOf (Num 5) (Just TNum)
    testTypeOf (Boolean True) (Just TBool)
    testTypeOf (Plus (Num 1) (Num 2)) (Just TNum)
    testTypeOf (And (Boolean True) (Boolean False)) (Just TBool)
    testTypeOf (Leq (Num 1) (Num 2)) (Just TBool)
    testTypeOf (If (Boolean True) (Num 1) (Boolean False)) Nothing
    testTypeOf (Plus (Num 1) (Boolean True)) Nothing  -- Type mismatch
    testTypeOf (If (Num 0) (Num 1) (Num 2)) Nothing  -- Non-boolean condition
    testTypeOf (IsZero (Boolean False)) Nothing  -- Type mismatch
    
    -- Optimization tests
    testOptimize (Plus (Num 0) (Num 5)) (Num 5)
    testOptimize (Minus (Num 5) (Num 0)) (Num 5)
    testOptimize (If (Boolean True) (Num 1) (Num 0)) (Num 1)
    testOptimize (Plus (Num 0) (Plus (Num 0) (Num 5))) (Num 5)  -- Nested zero addition
    testOptimize (Minus (Minus (Num 10) (Num 0)) (Num 0)) (Num 10)  -- Nested zero subtraction
    testOptimize (If (Boolean True) (Plus (Num 1) (Num 2)) (Mult (Num 3) (Num 4))) (Plus (Num 1) (Num 2))  -- Optimization in If branches
    putStrLn "All tests completed."


testEvalOptim :: ABE -> Maybe ABE -> IO ()
testEvalOptim input expected = do
    let result = evalOptM input
    if result == expected
        then putStrLn $ "PASS: EvalOptim " ++ show input
        else putStrLn $ "FAIL: EvalOptim " ++ show input ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result

testEvalType :: ABE -> Maybe ABE -> IO ()
testEvalType input expected = do
    let result = evalTypeM input
    if result == expected
        then putStrLn $ "PASS: EvalType " ++ show input
        else putStrLn $ "FAIL: EvalType " ++ show input ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result

testTypeOf :: ABE -> Maybe TABE -> IO ()
testTypeOf input expected = do
    let result = typeofM input
    if result == expected
        then putStrLn $ "PASS: TypeOf " ++ show input
        else putStrLn $ "FAIL: TypeOf " ++ show input ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result

testOptimize :: ABE -> ABE -> IO ()
testOptimize input expected = do
    let result = optimize input
    if result == expected
        then putStrLn $ "PASS: Optimize " ++ show input
        else putStrLn $ "FAIL: Optimize " ++ show input ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result


