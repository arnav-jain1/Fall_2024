{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

-- Imports for Parsec

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

-- AST Pretty Printer

pprint :: ABE -> String
pprint (Num n) = show n
pprint (Boolean b) = show b
pprint (Plus n m) = "(" ++ pprint n ++ " + " ++ pprint m ++ ")"
pprint (Minus n m) = "(" ++ pprint n ++ " - " ++ pprint m ++ ")"
pprint (Mult n m) = "(" ++ pprint n ++ " * " ++ pprint m ++ ")"
pprint (Div n m) = "(" ++ pprint n ++ " / " ++ pprint m ++ ")"
pprint (And n m) = "(" ++ pprint n ++ " && " ++ pprint m ++ ")"
pprint (Leq n m) = "(" ++ pprint n ++ " <= " ++ pprint m ++ ")"
pprint (IsZero m) = "(isZero " ++ pprint m ++ ")"
pprint (If c n m) = "(if " ++ pprint c ++ " then " ++ pprint n ++ " else " ++ pprint m ++ ")"


-- Parser (Requires ParserUtils and Parsec)

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

expr :: Parser ABE
expr = buildExpressionParser opTable term

opTable = [ [ inFix "*" Plus AssocLeft
            , inFix "/" Minus AssocLeft ]
          , [ inFix "+" Plus AssocLeft
            , inFix "-" Minus AssocLeft ]
          , [ inFix "<=" Leq AssocLeft
            , preFix "isZero" IsZero ]
          , [ inFix "&&" And AssocLeft ]
          ]

numExpr :: Parser ABE
numExpr = do i <- integer lexer
             return (Num (fromInteger i))

trueExpr :: Parser ABE
trueExpr = do i <- reserved lexer "true"
              return (Boolean True)

falseExpr :: Parser ABE
falseExpr = do i <- reserved lexer "false"
               return (Boolean False)

ifExpr :: Parser ABE
ifExpr = do reserved lexer "if"
            c <- expr
            reserved lexer "then"
            t <- expr
            reserved lexer "else"
            e <- expr
            return (If c t e)

term = parens lexer expr
       <|> numExpr
       <|> trueExpr
       <|> falseExpr
       <|> ifExpr

-- Parser invocation

parseABE = parseString expr

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
typeofM (Num a) = if a > 0 then Just TNum else Nothing
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
                          if l' == TBool && r' == TBool then Just TNum else Nothing 
                         }
typeofM (Leq l r) = do { l' <- typeofM l;
                          r' <- typeofM r;
                          if l' == TNum && r' == TNum then Just TNum else Nothing 
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
    
    -- Parsing tests
    testParse "1 + 2" (Plus (Num 1) (Num 2))
    testParse "3 * (4 - 2)" (Mult (Num 3) (Minus (Num 4) (Num 2)))
    testParse "if true then 1 else 0" (If (Boolean True) (Num 1) (Num 0))
    testParse "isZero(0)" (IsZero (Num 0))
    testParse "1 <= 2 && true" (And (Leq (Num 1) (Num 2)) (Boolean True))
    
    -- Evaluation tests
    testEval (Plus (Num 1) (Num 2)) (Just (Num 3))
    testEval (Minus (Num 5) (Num 2)) (Just (Num 3))
    testEval (Mult (Num 3) (Num 4)) (Just (Num 12))
    testEval (Div (Num 10) (Num 2)) (Just (Num 5))
    testEval (Div (Num 10) (Num 0)) Nothing
    testEval (And (Boolean True) (Boolean False)) (Just (Boolean False))
    testEval (Leq (Num 1) (Num 2)) (Just (Boolean True))
    testEval (IsZero (Num 0)) (Just (Boolean True))
    testEval (If (Boolean True) (Num 1) (Num 0)) (Just (Num 1))
    
    -- Type checking tests
    testTypeOf (Num 5) (Just TNum)
    testTypeOf (Boolean True) (Just TBool)
    testTypeOf (Plus (Num 1) (Num 2)) (Just TNum)
    testTypeOf (And (Boolean True) (Boolean False)) (Just TBool)
    testTypeOf (Leq (Num 1) (Num 2)) (Just TBool)
    testTypeOf (If (Boolean True) (Num 1) (Boolean False)) Nothing
    
    -- Optimization tests
    testOptimize (Plus (Num 0) (Num 5)) (Num 5)
    testOptimize (Minus (Num 5) (Num 0)) (Num 5)
    testOptimize (If (Boolean True) (Num 1) (Num 0)) (Num 1)
    
    putStrLn "All tests completed."

testParse :: String -> ABE -> IO ()
testParse input expected = do
    let result = parseABE input
    if result == expected
        then putStrLn $ "PASS: Parse '" ++ input ++ "'"
        else putStrLn $ "FAIL: Parse '" ++ input ++ "'\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result

testEval :: ABE -> Maybe ABE -> IO ()
testEval input expected = do
    let result = evalM input
    if result == expected
        then putStrLn $ "PASS: Eval " ++ show input
        else putStrLn $ "FAIL: Eval " ++ show input ++ "\n  Expected: " ++ show expected ++ "\n  Got: " ++ show result

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
