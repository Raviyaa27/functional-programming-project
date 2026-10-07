{-# OPTIONS_GHC -Wall #-}
-- |
-- Module      : Main (demo)
-- Description : Demonstrates every part of the interpreter.
--
-- Run with:   runghc Main.hs
module Main (main) where

import Control.Monad (forM_, zipWithM_)
import Data.List (intercalate)

import Expr
import Samples

main :: IO ()
main = do
  putStrLn "EC8206 Functional Programming - Expression Interpreter Demo"
  putStrLn ("Environment: " ++ showEnv sampleEnv)

  heading "Part B: sample evaluations (expected vs actual)"
  zipWithM_ showSample [1 :: Int ..] samples

  heading "Part C: simplify (before  ->  after)"
  forM_ simplifyExamples $ \e ->
    putStrLn ("  " ++ pad 34 (pretty e) ++ "->  " ++ pretty (simplify e))
  putStrLn "  note: 1 / 0 is left for eval to report; w * 0 -> 0 also drops the"
  putStrLn "        'undefined variable' error, so simplify preserves successful"
  putStrLn "        results only (see Props.hs and the report, Part E)."

  heading "Part C: batch evaluation with map (eval env)"
  let batch = map sampleExpr samples
  zipWithM_ (\i r -> putStrLn ("  [" ++ show i ++ "] " ++ showResult r))
            [1 :: Int ..] (evalAll sampleEnv batch)
  let report = batchReport sampleEnv batch
  putStrLn ("  evalBatch   : " ++ showValues (evalBatch sampleEnv batch))
  putStrLn ("  batchReport : " ++ show (succeeded report) ++ " succeeded, "
            ++ show (failed report) ++ " failed")
  forM_ (failures report) $ \err -> putStrLn ("                - " ++ err)

  heading "Part C: static check with undefinedVars (no evaluation needed)"
  forM_ samples $ \s ->
    case undefinedVars sampleEnv (sampleExpr s) of
      []      -> pure ()
      missing -> putStrLn ("  " ++ pretty (sampleExpr s) ++ "  uses unbound "
                           ++ intercalate ", " missing)
  putStrLn "  (every other sample only uses bound variables)"

-- | Print one sample as expression / expected / actual / verdict.
showSample :: Int -> Sample -> IO ()
showSample i s = do
  let got     = eval sampleEnv (sampleExpr s)
      verdict = if got == sampleExpected s then "PASS" else "FAIL"
  putStrLn ("  [" ++ show i ++ "] " ++ sampleName s)
  putStrLn ("      expression : " ++ pretty (sampleExpr s))
  putStrLn ("      expected   : " ++ showResult (sampleExpected s))
  putStrLn ("      actual     : " ++ pad 40 (showResult got) ++ verdict)

simplifyExamples :: [Expr]
simplifyExamples =
  [ Add (Mul x (Lit 1)) (Lit 0)
  , Let "a" (Add x (Lit 0)) (Mul (Mul (Var "a") (Lit 1)) y)
  , Add (Mul (Lit 2) (Lit 3)) (Mul x (Lit 0))
  , Sub (Add x y) (Add x y)
  , If (Less (Lit 1) (Lit 2)) (Div x (Lit 1)) y
  , Let "unused" (Lit 9) (Add x y)
  , Div (Lit 1) (Lit 0)
  , Mul (Var "w") (Lit 0)
  ]
  where
    x = Var "x"
    y = Var "y"

------------------------------------------------------------------------------
-- Formatting helpers
------------------------------------------------------------------------------

heading :: String -> IO ()
heading title = putStrLn ("\n== " ++ title ++ " ==")

showResult :: Either String Double -> String
showResult (Right v)  = pretty (Lit v)
showResult (Left err) = "error: " ++ err

showValues :: [Double] -> String
showValues vs = "[" ++ intercalate ", " (map (pretty . Lit) vs) ++ "]"

showEnv :: Env -> String
showEnv env = intercalate ", " [name ++ " = " ++ pretty (Lit v) | (name, v) <- env]

pad :: Int -> String -> String
pad n s = s ++ replicate (n - length s) ' '
