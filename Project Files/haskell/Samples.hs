{-# OPTIONS_GHC -Wall #-}
-- |
-- Module      : Samples
-- Description : The sample evaluations shown in the report.
--
-- Each sample pairs an expression with its expected result, worked out by
-- hand. "Main" prints them as an expected-vs-actual table and "Tests" checks
-- them automatically.
module Samples
  ( Sample (..)
  , sampleEnv
  , samples
  ) where

import Expr

-- | One sample evaluation: a short description, the expression and the
-- result we expect 'eval' to produce.
data Sample = Sample
  { sampleName     :: String
  , sampleExpr     :: Expr
  , sampleExpected :: Either String Double
  }

-- | The environment shared by all samples: x = 5, y = 2, z = 0.
sampleEnv :: Env
sampleEnv = [("x", 5), ("y", 2), ("z", 0)]

samples :: [Sample]
samples =
  [ Sample "precedence"
      -- (x + 3) * y  =  (5 + 3) * 2
      (Mul (Add (Var "x") (Lit 3)) (Var "y"))
      (Right 16)
  , Sample "division"
      -- x / y  =  5 / 2
      (Div (Var "x") (Var "y"))
      (Right 2.5)
  , Sample "let binding"
      -- let a = x * 2 in a + 1  =  10 + 1
      (Let "a" (Mul (Var "x") (Lit 2)) (Add (Var "a") (Lit 1)))
      (Right 11)
  , Sample "let shadowing"
      -- let x = 1 in (let x = 10 in x) + x  =  10 + 1
      (Let "x" (Lit 1) (Add (Let "x" (Lit 10) (Var "x")) (Var "x")))
      (Right 11)
  , Sample "if / boolean ops"
      -- if x < 10 && not (y == 0) then x / y else 0  =  5 / 2
      (If (And (Less (Var "x") (Lit 10)) (Not (Equal (Var "y") (Lit 0))))
          (Div (Var "x") (Var "y"))
          (Lit 0))
      (Right 2.5)
  , Sample "guarded division"
      -- if z == 0 then 0 else x / z  =  0  (the x / z branch never runs)
      (If (Equal (Var "z") (Lit 0)) (Lit 0) (Div (Var "x") (Var "z")))
      (Right 0)
  , Sample "division by zero"
      -- x / (y - 2)  where y - 2 = 0
      (Div (Var "x") (Sub (Var "y") (Lit 2)))
      (Left "division by zero: x / (y - 2)")
  , Sample "undefined variable"
      -- x + w  where w is not bound
      (Add (Var "x") (Var "w"))
      (Left "undefined variable: w")
  , Sample "error inside let"
      -- let a = 1 / z in a + 1  where z = 0
      (Let "a" (Div (Lit 1) (Var "z")) (Add (Var "a") (Lit 1)))
      (Left "division by zero: 1 / z")
  ]
