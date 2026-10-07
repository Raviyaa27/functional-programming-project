{-# OPTIONS_GHC -Wall #-}
-- EC8206 Functional Programming project
-- Achintha H.G.R. (EG/2021/4384)
module Expr
  ( Expr (..)
  , BExpr (..)
  , Env
  , eval
  , evalB
  , simplify
  , simplifyB
  , evalAll
  , evalBatch
  , BatchReport (..)
  , batchReport
  , freeVars
  , undefinedVars
  , pretty
  , prettyB
  ) where

import Data.Either (rights)
import Data.List (nub)

-- Part A

data Expr
  = Lit Double
  | Var String
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Let String Expr Expr
  | If BExpr Expr Expr
  deriving (Eq, Show)

data BExpr
  = BLit Bool
  | Less Expr Expr
  | Equal Expr Expr
  | And BExpr BExpr
  | Or BExpr BExpr
  | Not BExpr
  deriving (Eq, Show)

type Env = [(String, Double)]

-- Part B

eval :: Env -> Expr -> Either String Double
eval _   (Lit n)        = Right n
eval env (Var x)        = maybe (Left ("undefined variable: " ++ x)) Right
                                (lookup x env)
eval env (Add a b)      = (+) <$> eval env a <*> eval env b
eval env (Sub a b)      = (-) <$> eval env a <*> eval env b
eval env (Mul a b)      = (*) <$> eval env a <*> eval env b
eval env (Div a b)      = do
  x <- eval env a
  y <- eval env b
  if y == 0
    then Left ("division by zero: " ++ pretty (Div a b))
    else Right (x / y)
eval env (Let x e body) = do
  v <- eval env e
  eval ((x, v) : env) body
eval env (If c t e)     = do
  b <- evalB env c
  if b then eval env t else eval env e

evalB :: Env -> BExpr -> Either String Bool
evalB _   (BLit b)    = Right b
evalB env (Less a b)  = (<)  <$> eval env a <*> eval env b
evalB env (Equal a b) = (==) <$> eval env a <*> eval env b
evalB env (And p q)   = do
  l <- evalB env p
  if l then evalB env q else Right False
evalB env (Or p q)    = do
  l <- evalB env p
  if l then Right True else evalB env q
evalB env (Not p)     = not <$> evalB env p

-- Part C

-- x * 0, x - x and an unused let drop a sub-expression, and any error in it.
simplify :: Expr -> Expr
simplify = rewrite . descend
  where
    descend :: Expr -> Expr
    descend e@(Lit _)      = e
    descend e@(Var _)      = e
    descend (Add a b)      = Add (simplify a) (simplify b)
    descend (Sub a b)      = Sub (simplify a) (simplify b)
    descend (Mul a b)      = Mul (simplify a) (simplify b)
    descend (Div a b)      = Div (simplify a) (simplify b)
    descend (Let x e body) = Let x (simplify e) (simplify body)
    descend (If c t e)     = If (simplifyB c) (simplify t) (simplify e)

    rewrite :: Expr -> Expr
    rewrite (Add (Lit a) (Lit b)) = Lit (a + b)
    rewrite (Sub (Lit a) (Lit b)) = Lit (a - b)
    rewrite (Mul (Lit a) (Lit b)) = Lit (a * b)
    rewrite (Div (Lit a) (Lit b))
      | b /= 0                    = Lit (a / b)
    rewrite (Add e (Lit 0))       = e
    rewrite (Add (Lit 0) e)       = e
    rewrite (Sub e (Lit 0))       = e
    rewrite (Sub a b)
      | a == b                    = Lit 0
    rewrite (Mul e (Lit 1))       = e
    rewrite (Mul (Lit 1) e)       = e
    rewrite (Mul _ (Lit 0))       = Lit 0
    rewrite (Mul (Lit 0) _)       = Lit 0
    rewrite (Div e (Lit 1))       = e
    rewrite (If (BLit True) t _)  = t
    rewrite (If (BLit False) _ e) = e
    rewrite (Let x _ body)
      | x `notElem` freeVars body = body
    rewrite e                     = e

simplifyB :: BExpr -> BExpr
simplifyB = rewriteB . descendB
  where
    descendB :: BExpr -> BExpr
    descendB c@(BLit _)  = c
    descendB (Less a b)  = Less (simplify a) (simplify b)
    descendB (Equal a b) = Equal (simplify a) (simplify b)
    descendB (And p q)   = And (simplifyB p) (simplifyB q)
    descendB (Or p q)    = Or (simplifyB p) (simplifyB q)
    descendB (Not p)     = Not (simplifyB p)

    rewriteB :: BExpr -> BExpr
    rewriteB (Less (Lit a) (Lit b))  = BLit (a < b)
    rewriteB (Equal (Lit a) (Lit b)) = BLit (a == b)
    rewriteB (And (BLit True) q)     = q
    rewriteB (And (BLit False) _)    = BLit False
    rewriteB (Or (BLit True) _)      = BLit True
    rewriteB (Or (BLit False) q)     = q
    rewriteB (Not (BLit b))          = BLit (not b)
    rewriteB (Not (Not p))           = p
    rewriteB c                       = c

-- eval env is a partial application of eval
evalAll :: Env -> [Expr] -> [Either String Double]
evalAll env = map (eval env)

evalBatch :: Env -> [Expr] -> [Double]
evalBatch env = rights . evalAll env

data BatchReport = BatchReport
  { succeeded :: Int
  , failed    :: Int
  , values    :: [Double]
  , failures  :: [String]
  } deriving (Eq, Show)

batchReport :: Env -> [Expr] -> BatchReport
batchReport env = foldr tally (BatchReport 0 0 [] []) . evalAll env
  where
    tally :: Either String Double -> BatchReport -> BatchReport
    tally (Right v)  r = r { succeeded = succeeded r + 1
                           , values    = v : values r }
    tally (Left err) r = r { failed    = failed r + 1
                           , failures  = err : failures r }

freeVars :: Expr -> [String]
freeVars = nub . go
  where
    go :: Expr -> [String]
    go (Lit _)        = []
    go (Var x)        = [x]
    go (Add a b)      = go a ++ go b
    go (Sub a b)      = go a ++ go b
    go (Mul a b)      = go a ++ go b
    go (Div a b)      = go a ++ go b
    go (Let x e body) = go e ++ filter (/= x) (go body)
    go (If c t e)     = goB c ++ go t ++ go e

    goB :: BExpr -> [String]
    goB (BLit _)    = []
    goB (Less a b)  = go a ++ go b
    goB (Equal a b) = go a ++ go b
    goB (And p q)   = goB p ++ goB q
    goB (Or p q)    = goB p ++ goB q
    goB (Not p)     = goB p

undefinedVars :: Env -> Expr -> [String]
undefinedVars env = filter (`notElem` bound) . freeVars
  where
    bound = map fst env

-- Pretty printing

pretty :: Expr -> String
pretty = prettyPrec 0

prettyB :: BExpr -> String
prettyB = prettyPrecB 0

prettyPrec :: Int -> Expr -> String
prettyPrec p (Lit n)
  | n < 0                   = parensIf (p > 0) (showNumber n)
  | otherwise               = showNumber n
prettyPrec _ (Var x)        = x
prettyPrec p (Add a b)      = parensIf (p > 6) (prettyPrec 6 a ++ " + " ++ prettyPrec 7 b)
prettyPrec p (Sub a b)      = parensIf (p > 6) (prettyPrec 6 a ++ " - " ++ prettyPrec 7 b)
prettyPrec p (Mul a b)      = parensIf (p > 7) (prettyPrec 7 a ++ " * " ++ prettyPrec 8 b)
prettyPrec p (Div a b)      = parensIf (p > 7) (prettyPrec 7 a ++ " / " ++ prettyPrec 8 b)
prettyPrec p (Let x e body) = parensIf (p > 0)
  ("let " ++ x ++ " = " ++ prettyPrec 0 e ++ " in " ++ prettyPrec 0 body)
prettyPrec p (If c t e)     = parensIf (p > 0)
  ("if " ++ prettyPrecB 0 c ++ " then " ++ prettyPrec 0 t ++ " else " ++ prettyPrec 0 e)

prettyPrecB :: Int -> BExpr -> String
prettyPrecB _ (BLit b)    = if b then "true" else "false"
prettyPrecB p (Less a b)  = parensIf (p > 4) (prettyPrec 5 a ++ " < "  ++ prettyPrec 5 b)
prettyPrecB p (Equal a b) = parensIf (p > 4) (prettyPrec 5 a ++ " == " ++ prettyPrec 5 b)
prettyPrecB p (And l r)   = parensIf (p > 3) (prettyPrecB 3 l ++ " && " ++ prettyPrecB 4 r)
prettyPrecB p (Or l r)    = parensIf (p > 2) (prettyPrecB 2 l ++ " || " ++ prettyPrecB 3 r)
prettyPrecB p (Not b)     = parensIf (p > 8) ("not " ++ prettyPrecB 9 b)

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

showNumber :: Double -> String
showNumber n
  | isWhole   = show (round n :: Integer)
  | otherwise = show n
  where
    isWhole = not (isInfinite n) && not (isNaN n)
              && abs n < 1e15 && n == fromInteger (round n)
