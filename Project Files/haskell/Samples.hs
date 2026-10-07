{-# OPTIONS_GHC -Wall #-}
-- Sample expressions with their expected results (worked out by hand).
module Samples
  ( Sample (..)
  , sampleEnv
  , samples
  ) where

import Expr

data Sample = Sample
  { sampleName     :: String
  , sampleExpr     :: Expr
  , sampleExpected :: Either String Double
  }

sampleEnv :: Env
sampleEnv = [("x", 5), ("y", 2), ("z", 0)]

samples :: [Sample]
samples =
  [ Sample "precedence"
      (Mul (Add (Var "x") (Lit 3)) (Var "y"))
      (Right 16)
  , Sample "division"
      (Div (Var "x") (Var "y"))
      (Right 2.5)
  , Sample "let binding"
      (Let "a" (Mul (Var "x") (Lit 2)) (Add (Var "a") (Lit 1)))
      (Right 11)
  , Sample "let shadowing"
      (Let "x" (Lit 1) (Add (Let "x" (Lit 10) (Var "x")) (Var "x")))
      (Right 11)
  , Sample "if / boolean ops"
      (If (And (Less (Var "x") (Lit 10)) (Not (Equal (Var "y") (Lit 0))))
          (Div (Var "x") (Var "y"))
          (Lit 0))
      (Right 2.5)
  , Sample "guarded division"
      (If (Equal (Var "z") (Lit 0)) (Lit 0) (Div (Var "x") (Var "z")))
      (Right 0)
  , Sample "division by zero"
      (Div (Var "x") (Sub (Var "y") (Lit 2)))
      (Left "division by zero: x / (y - 2)")
  , Sample "undefined variable"
      (Add (Var "x") (Var "w"))
      (Left "undefined variable: w")
  , Sample "error inside let"
      (Let "a" (Div (Lit 1) (Var "z")) (Add (Var "a") (Lit 1)))
      (Left "division by zero: 1 / z")
  ]
