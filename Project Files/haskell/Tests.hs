{-# OPTIONS_GHC -Wall #-}
-- Unit tests: runghc Tests.hs
module Main (main) where

import Control.Monad (forM_, unless)
import System.Exit (exitFailure)

import Expr
import Samples

data Outcome = Outcome
  { testName :: String
  , passed   :: Bool
  , expected :: String
  , actual   :: String
  }

check :: (Eq a, Show a) => String -> a -> a -> Outcome
check name want got = Outcome name (want == got) (show want) (show got)

env :: Env
env = sampleEnv

-- Part B

sampleTests :: [Outcome]
sampleTests =
  [ check (sampleName s) (sampleExpected s) (eval sampleEnv (sampleExpr s))
  | s <- samples ]

evalTests :: [Outcome]
evalTests =
  [ check "literal"                  (Right 4.5) (eval [] (Lit 4.5))
  , check "bound variable"           (Right 2)   (eval env (Var "y"))
  , check "unbound variable"         (Left "undefined variable: x") (eval [] (Var "x"))
  , check "addition"                 (Right 7)   (eval env (Add (Var "x") (Var "y")))
  , check "subtraction"              (Right 3)   (eval env (Sub (Var "x") (Var "y")))
  , check "multiplication"           (Right 10)  (eval env (Mul (Var "x") (Var "y")))
  , check "division"                 (Right 0.4) (eval env (Div (Var "y") (Var "x")))
  , check "nested: 2 + 3 * 4"        (Right 14)
      (eval [] (Add (Lit 2) (Mul (Lit 3) (Lit 4))))
  , check "zero divided by a number" (Right 0)   (eval env (Div (Var "z") (Var "x")))
  , check "division by literal zero" (Left "division by zero: 1 / 0")
      (eval [] (Div (Lit 1) (Lit 0)))
  , check "error deep in the tree"   (Left "division by zero: 1 / z")
      (eval env (Mul (Lit 2) (Add (Lit 1) (Div (Lit 1) (Var "z")))))
  , check "leftmost error is reported" (Left "undefined variable: p")
      (eval env (Add (Var "p") (Var "q")))
  , check "let reads the outer env"  (Right 10)
      (eval env (Let "b" (Var "x") (Mul (Var "b") (Var "y"))))
  , check "let shadows only inside its body" (Right 105)
      (eval env (Add (Let "x" (Lit 100) (Var "x")) (Var "x")))
  , check "let binding does not leak out" (Left "undefined variable: a")
      (eval env (Add (Let "a" (Lit 1) (Var "a")) (Var "a")))
  , check "error in let body"        (Left "undefined variable: w")
      (eval env (Let "a" (Lit 1) (Var "w")))
  , check "if takes the else branch" (Right 2)
      (eval env (If (BLit False) (Lit 1) (Lit 2)))
  , check "unchosen else branch never runs" (Right 1)
      (eval env (If (BLit True) (Lit 1) (Var "w")))
  , check "unchosen then branch never runs" (Right 7)
      (eval env (If (BLit False) (Div (Lit 1) (Lit 0)) (Lit 7)))
  , check "error in the condition"   (Left "undefined variable: w")
      (eval env (If (Less (Var "w") (Lit 1)) (Lit 1) (Lit 2)))
  ]

evalBTests :: [Outcome]
evalBTests =
  [ check "less than (true)"         (Right True)  (evalB env (Less (Var "y") (Var "x")))
  , check "less than (false)"        (Right False) (evalB env (Less (Var "x") (Var "y")))
  , check "equality"                 (Right True)  (evalB env (Equal (Var "z") (Lit 0)))
  , check "not"                      (Right False) (evalB env (Not (BLit True)))
  , check "and short-circuits"       (Right False)
      (evalB env (And (BLit False) (Less (Var "w") (Lit 1))))
  , check "or short-circuits"        (Right True)
      (evalB env (Or (BLit True) (Less (Var "w") (Lit 1))))
  , check "and evaluates its right side when needed" (Left "undefined variable: w")
      (evalB env (And (BLit True) (Less (Var "w") (Lit 1))))
  ]

-- Part C

x, y, a :: Expr
x = Var "x"
y = Var "y"
a = Var "a"

simplifyTests :: [Outcome]
simplifyTests =
  [ check "x + 0  ->  x"             x (simplify (Add x (Lit 0)))
  , check "0 + x  ->  x"             x (simplify (Add (Lit 0) x))
  , check "x - 0  ->  x"             x (simplify (Sub x (Lit 0)))
  , check "x - x  ->  0"             (Lit 0) (simplify (Sub (Add x y) (Add x y)))
  , check "x * 1  ->  x"             x (simplify (Mul x (Lit 1)))
  , check "1 * x  ->  x"             x (simplify (Mul (Lit 1) x))
  , check "x * 0  ->  0"             (Lit 0) (simplify (Mul x (Lit 0)))
  , check "0 * x  ->  0"             (Lit 0) (simplify (Mul (Lit 0) x))
  , check "x / 1  ->  x"             x (simplify (Div x (Lit 1)))
  , check "constant folding"         (Lit 14) (simplify (Add (Lit 2) (Mul (Lit 3) (Lit 4))))
  , check "n / 0 is left for eval to report" (Div (Lit 1) (Lit 0))
      (simplify (Div (Lit 1) (Lit 0)))
  , check "bottom-up: (x * 1) + 0  ->  x" x (simplify (Add (Mul x (Lit 1)) (Lit 0)))
  , check "rewrites inside let"      (Let "a" x (Mul a y))
      (simplify (Let "a" (Add x (Lit 0)) (Mul (Mul a (Lit 1)) y)))
  , check "unused let is removed"    x (simplify (Let "a" (Lit 9) x))
  , check "known condition picks a branch" x
      (simplify (If (Less (Lit 1) (Lit 2)) x y))
  , check "double negation"          (Less x (Lit 1))
      (simplifyB (Not (Not (Less x (Lit 1)))))
  , check "and with false"           (BLit False)
      (simplifyB (And (BLit False) (Less (Var "w") (Lit 1))))
  , check "nothing to simplify"      (Add x y) (simplify (Add x y))
  , check "every sample keeps its value after simplify"
      (map sampleExpected samples)
      (map (eval sampleEnv . simplify . sampleExpr) samples)
  , check "x * 0 hides an error: before" (Left "undefined variable: w")
      (eval env (Mul (Var "w") (Lit 0)))
  , check "x * 0 hides an error: after"  (Right 0)
      (eval env (simplify (Mul (Var "w") (Lit 0))))
  ]

hofTests :: [Outcome]
hofTests =
  [ check "evalAll = map (eval env)" [Right 1, Left "undefined variable: w"]
      (evalAll env [Lit 1, Var "w"])
  , check "evalBatch keeps successes" [16, 2.5, 11, 11, 2.5, 0]
      (evalBatch sampleEnv (map sampleExpr samples))
  , check "batchReport counts (succeeded, failed)" (6, 3)
      (let r = batchReport sampleEnv (map sampleExpr samples)
       in (succeeded r, failed r))
  , check "batchReport collects the errors"
      [ "division by zero: x / (y - 2)"
      , "undefined variable: w"
      , "division by zero: 1 / z" ]
      (failures (batchReport sampleEnv (map sampleExpr samples)))
  , check "batchReport of an empty batch" (BatchReport 0 0 [] [])
      (batchReport env [])
  , check "freeVars skips let-bound names" ["x", "y"]
      (freeVars (Let "a" x (Add a y)))
  , check "freeVars has no duplicates" ["x"] (freeVars (Mul x (Add x x)))
  , check "undefinedVars finds w only" ["w"]
      (undefinedVars env (Add (Var "w") (Let "a" (Lit 1) a)))
  , check "undefinedVars on a closed expression" []
      (undefinedVars env (Mul (Add x (Lit 3)) y))
  ]

prettyTests :: [Outcome]
prettyTests =
  [ check "brackets only where needed" "(x + 3) * y"
      (pretty (Mul (Add (Var "x") (Lit 3)) (Var "y")))
  , check "left-associative minus"   "a - b - c"
      (pretty (Sub (Sub (Var "a") (Var "b")) (Var "c")))
  , check "right-nested minus"       "a - (b - c)"
      (pretty (Sub (Var "a") (Sub (Var "b") (Var "c"))))
  , check "fractions and negatives"  "2.5 + (-3)"
      (pretty (Add (Lit 2.5) (Lit (-3))))
  , check "let inside an operator"   "1 + (let a = 2 in a)"
      (pretty (Add (Lit 1) (Let "a" (Lit 2) (Var "a"))))
  , check "conditions"               "if x < 10 && not (y == 0) then x / y else 0"
      (pretty (If (And (Less (Var "x") (Lit 10)) (Not (Equal (Var "y") (Lit 0))))
                  (Div (Var "x") (Var "y")) (Lit 0)))
  ]

groups :: [(String, [Outcome])]
groups =
  [ ("Part B: sample evaluations (as in the report)", sampleTests)
  , ("Part B: eval",                                  evalTests)
  , ("Part B: evalB",                                 evalBTests)
  , ("Part C: simplify",                              simplifyTests)
  , ("Part C: higher-order functions",                hofTests)
  , ("Pretty printing",                               prettyTests)
  ]

main :: IO ()
main = do
  forM_ groups $ \(title, outcomes) -> do
    putStrLn ("\n== " ++ title ++ " ==")
    mapM_ report outcomes
  let everything = concatMap snd groups
      failing    = filter (not . passed) everything
  putStrLn ("\n" ++ show (length everything - length failing) ++ "/"
            ++ show (length everything) ++ " tests passed.")
  unless (null failing) exitFailure
  where
    report o
      | passed o  = putStrLn ("  PASS  " ++ testName o)
      | otherwise = do
          putStrLn ("  FAIL  " ++ testName o)
          putStrLn ("          expected: " ++ expected o)
          putStrLn ("          actual:   " ++ actual o)
