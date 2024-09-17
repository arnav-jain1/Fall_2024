{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- Imports for Parsec
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Data.Bits (Bits(xor))

--
-- Simple caculator over naturals with no identifiers
--
-- Author: Perry Alexander
-- Date: Tue Jan 23 17:54:44 CST 2018
--
-- Source files for the Arithmetic Expressions (AE) language from PLIH
--

-- AST Definition

data AE where
  Nat :: Int -> AE
  Plus :: AE -> AE -> AE
  Minus :: AE -> AE -> AE
  Mult :: AE -> AE -> AE
  Div :: AE -> AE -> AE
  If0 :: AE -> AE -> AE -> AE
  deriving (Show,Eq)

-- AE Parser (Requires ParserUtils and Parsec included above)

languageDef =
  javaStyle { identStart = letter
            , identLetter = alphaNum
            , reservedNames = [ "if0"
                              , "then"
                              , "else"
                              ]
            , reservedOpNames = [ "+","-","*","/"]
            }
  
lexer = makeTokenParser languageDef

inFix o c a = (Infix (reservedOp lexer o >> return c) a)
preFix o c = (Prefix (reservedOp lexer o >> return c))
postFix o c = (Postfix (reservedOp lexer o >> return c))

parseString p str =
  case parse p "" str of
    Left e -> error $ show e
    Right r -> r

expr :: Parser AE
expr = buildExpressionParser operators term

operators = [
  [ inFix "*" Mult AssocLeft
    , inFix "/" Div AssocLeft ]
  , [ inFix "+" Plus AssocLeft
  , inFix "-" Minus AssocLeft ]
  ]
  
numExpr :: Parser AE
numExpr = do i <- integer lexer
             return (Nat (fromInteger i))

ifExpr :: Parser AE
ifExpr  = do reserved lexer "if0"
             c <- expr
             reserved lexer "then"
             t <- expr
             reserved lexer "else"
             e <- expr
             return (If0 c t e)
                     

term = parens lexer expr
       <|> numExpr
       <|> ifExpr

-- Parser invocation
-- Call parseAE to parse a string into the AE data structure.

parseAE = parseString expr

-- Evaluation Functions
-- Replace the bodies of these functions with your implementations for
-- Exercises 1-4.  Feel free to add utility functions or testing functions as
-- you see fit, but do not change the function signatures.  Note that only
-- Exercise 4 requires you to integrate the parser above.

evalAE :: AE -> Int
evalAE (Nat a) = a
evalAE (Plus l r) 
    | evalAE(l) < 0 = error "!"
    | evalAE(r) < 0  = error "!"
    | otherwise = (evalAE(l) + evalAE(r))
evalAE (Minus l r)
    | evalAE(l) < 0 = error("!")
    | evalAE(r) < 0  = error("!")
    | evalAE(l) < evalAE(r) = error("!")
    | otherwise = (evalAE(l) - evalAE(r))
evalAE (Mult l r) 
    | evalAE(l) < 0 = error "!"
    | evalAE(r) < 0  = error "!"
    | otherwise = (evalAE(l) * evalAE(r))
evalAE (Div l r) 
    | evalAE(l) < 0 = error "!"
    | evalAE(r) <= 0  = error "!"
    | otherwise = (evalAE(l) `div` evalAE(r))
evalAE (If0 condition l r) 
    | evalAE(condition) == 0 = evalAE(l)
    | otherwise = evalAE(r)
evalAEMaybe :: AE -> Maybe Int
evalAEMaybe (Nat a) = Just a
evalAEMaybe (Plus l r) = case (evalAEMaybe l) of
                            (Nothing) -> Nothing
                            (Just l') -> case (evalAEMaybe r) of
                                           (Nothing) -> Nothing
                                           (Just r') -> if l' >= 0 && r' >= 0 then Just (r' + l') else Nothing
evalAEMaybe (Minus l r) = case (evalAEMaybe l) of
                            (Nothing) -> Nothing
                            (Just l') -> case (evalAEMaybe r) of
                                           (Nothing) -> Nothing
                                           (Just r') -> if l' >= 0 && r' >= 0 && l' >= r' then Just (l' - r') else Nothing
evalAEMaybe (Mult l r) = case (evalAEMaybe l) of
                            (Nothing) -> Nothing
                            (Just l') -> case (evalAEMaybe r) of
                                           (Nothing) -> Nothing
                                           (Just r') -> if l' >= 0 && r' >= 0 then Just (r' * l') else Nothing
evalAEMaybe (Div l r) = case (evalAEMaybe l) of 
                          (Nothing) -> Nothing
                          (Just l') -> case (evalAEMaybe r) of
                                         (Nothing) -> Nothing
                                         (Just r') -> if l' > 0 && r' >= 0 then Just (r' `div` l') else Nothing

evalAEMaybe (If0 condition l r)  = case (evalAEMaybe condition) of
                                     (Nothing) -> Nothing
                                     (Just condition') -> if condition' == 0 then evalAEMaybe l else evalAEMaybe r
evalM :: AE -> Maybe Int
evalM (Nat x) = if x >= 0 then Just x else Nothing
evalM (Plus l r) = do { x <- evalM l;
                       y <- evalM r;
                       Just (x + y) }
evalM (Minus l r) = do { x <- evalM l;
                         y <- evalM r;
                         if (x >= y) then Just (x-y) else Nothing }
evalM (Mult l r) = do { x <- evalM l;
                        y <- evalM r;
                        Just (x * y)
                      }
evalM (Div l r) = do { x <- evalM l;
                       y <- evalM r;
                       if (y == 0) then Nothing else Just (x `div` y)
                     }
evalM (If0 cond l r) = do {z <- evalM cond;
                          if (z == 0) then evalM l else evalM r}

interpAE :: String -> Maybe Int
interpAE input = evalM (parseAE input)


-- Regular operations
testCase1 = interpAE "1 + 2"  
testCase2 = interpAE "10 - 5"  
testCase3 = interpAE "3 * 4"  
testCase4 = interpAE "20 / 4"  
testCase5 = interpAE "2 + 3 * 4"  
testCase6 = interpAE "0 + 0"  
testCase7 = interpAE "1000000 * 1000000"  
testCase8 = interpAE "5 - 5"  
testCase9 = interpAE "10 / 1"  
testCase10 = interpAE "(2 + 3) * 4"  
testCase11 = interpAE "2 * (10 - 3)"  

-- If0 expressions
testCase12 = interpAE "if0 0 then 1 else 2"  
testCase13 = interpAE "if0 1 then 10 else 20"  
testCase14 = interpAE "if0 (5 - 5) then 100 else 200"  

-- Edge cases
testCase15 = interpAE "3 / 2"  

-- Nothing cases
testCase16 = interpAE "5 - 10"  
testCase17 = interpAE "10 / -2"  
testCase18 = interpAE "-5 + 10"  
testCase19 = interpAE "10 / 0"  

-- Run all test cases
main = do
    putStrLn ( "Test Case 1: " ++ show testCase1)
    putStrLn ( "Test Case 2: " ++ show testCase2)
    putStrLn ( "Test Case 3: " ++ show testCase3)
    putStrLn ( "Test Case 4: " ++ show testCase4)
    putStrLn ( "Test Case 5: " ++ show testCase5)
    putStrLn ( "Test Case 6: " ++ show testCase6)
    putStrLn ( "Test Case 7: " ++ show testCase7)
    putStrLn ( "Test Case 8: " ++ show testCase8)
    putStrLn ( "Test Case 9: " ++ show testCase9)
    putStrLn ( "Test Case 10: " ++ show testCase10)
    putStrLn ( "Test Case 11: " ++ show testCase11)
    putStrLn ( "Test Case 12: " ++ show testCase12)
    putStrLn ( "Test Case 13: " ++ show testCase13)
    putStrLn ( "Test Case 14: " ++ show testCase14)
    putStrLn ( "Test Case 15: " ++ show testCase15)
    putStrLn ( "Test Case 16: " ++ show testCase16)
    putStrLn ( "Test Case 17: " ++ show testCase17)
    putStrLn ("Test Case 18: " ++ show testCase18)
    putStrLn ("Test Case 19: " ++ show testCase19)

