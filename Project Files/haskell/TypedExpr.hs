{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall #-}
-- |
-- Module      : TypedExpr
-- Description : Extension: the same language as a GADT.
--
-- 'Expr.Expr' and 'Expr.BExpr' use two separate types to keep numbers and
-- conditions apart. A GADT (generalised algebraic data type) takes the idea
-- further: the index @a@ in @TExpr a@ records the type of value an
-- expression produces, so one type describes both kinds, and GHC still
-- rejects ill-typed expressions at compile time, for example
--
-- > TIf (TNum 1) (TNum 2) (TNum 3)   -- TNum 1 is a TExpr Double, not TExpr Bool
-- > TAdd (TBool True) (TNum 1)       -- TBool True is a TExpr Bool
--
-- The assignment does not require this module; the report mentions it in
-- Part E as the natural next step.
module TypedExpr
  ( TExpr (..)
  , evalT
  ) where

-- | A typed expression that produces a value of type @a@.
data TExpr a where
  TNum   :: Double -> TExpr Double
  TBool  :: Bool -> TExpr Bool
  TVar   :: String -> TExpr Double
  TAdd   :: TExpr Double -> TExpr Double -> TExpr Double
  TSub   :: TExpr Double -> TExpr Double -> TExpr Double
  TMul   :: TExpr Double -> TExpr Double -> TExpr Double
  TDiv   :: TExpr Double -> TExpr Double -> TExpr Double
  TLess  :: TExpr Double -> TExpr Double -> TExpr Bool
  TEqual :: TExpr Double -> TExpr Double -> TExpr Bool
  TAnd   :: TExpr Bool -> TExpr Bool -> TExpr Bool
  TNot   :: TExpr Bool -> TExpr Bool
  TLet   :: String -> TExpr Double -> TExpr a -> TExpr a
  TIf    :: TExpr Bool -> TExpr a -> TExpr a -> TExpr a

-- | One evaluator for every result type: a @TExpr Double@ evaluates to a
-- 'Double' and a @TExpr Bool@ to a 'Bool'. Only the data-dependent errors
-- (division by zero, undefined variable) are left for run time.
evalT :: [(String, Double)] -> TExpr a -> Either String a
evalT _   (TNum n)       = Right n
evalT _   (TBool b)      = Right b
evalT env (TVar x)       = maybe (Left ("undefined variable: " ++ x)) Right
                                 (lookup x env)
evalT env (TAdd a b)     = (+) <$> evalT env a <*> evalT env b
evalT env (TSub a b)     = (-) <$> evalT env a <*> evalT env b
evalT env (TMul a b)     = (*) <$> evalT env a <*> evalT env b
evalT env (TDiv a b)     = do
  x <- evalT env a
  y <- evalT env b
  if y == 0 then Left "division by zero" else Right (x / y)
evalT env (TLess a b)    = (<)  <$> evalT env a <*> evalT env b
evalT env (TEqual a b)   = (==) <$> evalT env a <*> evalT env b
evalT env (TAnd p q)     = do
  l <- evalT env p
  if l then evalT env q else Right False
evalT env (TNot p)       = not <$> evalT env p
evalT env (TLet x e body) = do
  v <- evalT env e
  evalT ((x, v) : env) body
evalT env (TIf c t e)    = do
  b <- evalT env c
  if b then evalT env t else evalT env e
