{-# OPTIONS_GHC -Wall #-}
-- |
-- Module      : Expr
-- Description : A small, type-safe arithmetic expression interpreter.
-- Author      : Achintha H.G.R. (EG/2021/4384)
--
-- EC8206 Functional Programming - Project Assignment.
--
--   * Part A  (language design) : 'Expr', 'BExpr', 'Env'
--   * Part B  (evaluation)      : 'eval', 'evalB'
--   * Pretty printing           : 'pretty', 'prettyB'
module Expr
  ( -- * Part A: language design
    Expr (..)
  , BExpr (..)
  , Env
    -- * Part B: evaluation
  , eval
  , evalB
    -- * Pretty printing
  , pretty
  , prettyB
  ) where

------------------------------------------------------------------------------
-- Part A: language design
------------------------------------------------------------------------------

-- | Arithmetic expressions. Every constructor is a different "shape" of
-- expression, so 'Expr' is a sum type: a value is exactly one of these
-- alternatives, and each alternative carries exactly the data it needs.
data Expr
  = Lit Double             -- ^ numeric literal, e.g. @3.5@
  | Var String             -- ^ variable reference, looked up in the 'Env'
  | Add Expr Expr          -- ^ @e1 + e2@
  | Sub Expr Expr          -- ^ @e1 - e2@
  | Mul Expr Expr          -- ^ @e1 * e2@
  | Div Expr Expr          -- ^ @e1 / e2@ (fails if the divisor is zero)
  | Let String Expr Expr   -- ^ @let x = e1 in e2@ (local, lexically scoped)
  | If BExpr Expr Expr     -- ^ @if b then e1 else e2@
  deriving (Eq, Show)

-- | Boolean conditions, used only as the first field of 'If'. Keeping them
-- in a separate type means a number can never be used where a condition is
-- expected, and a condition can never be used as a number.
data BExpr
  = BLit Bool              -- ^ @true@ / @false@
  | Less Expr Expr         -- ^ @e1 < e2@
  | Equal Expr Expr        -- ^ @e1 == e2@
  | And BExpr BExpr        -- ^ @b1 && b2@ (short-circuits)
  | Or BExpr BExpr         -- ^ @b1 || b2@ (short-circuits)
  | Not BExpr              -- ^ @not b@
  deriving (Eq, Show)

-- | Variable bindings. When a name occurs more than once, the first
-- (innermost) binding wins, which is exactly what 'lookup' does.
type Env = [(String, Double)]

------------------------------------------------------------------------------
-- Part B: evaluation
------------------------------------------------------------------------------

-- | Evaluate an expression in an environment.
--
-- Failure is an ordinary return value ('Left' with a message), never a
-- runtime exception. The 'Either' monad / applicative threads it through:
-- as soon as one sub-expression fails, the whole evaluation stops and that
-- 'Left' becomes the result.
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
  eval ((x, v) : env) body    -- a NEW list: the caller's env is untouched
eval env (If c t e)     = do
  b <- evalB env c
  if b then eval env t else eval env e   -- only the chosen branch runs

-- | Evaluate a condition. Mutually recursive with 'eval', because
-- comparisons contain arithmetic expressions.
evalB :: Env -> BExpr -> Either String Bool
evalB _   (BLit b)    = Right b
evalB env (Less a b)  = (<)  <$> eval env a <*> eval env b
evalB env (Equal a b) = (==) <$> eval env a <*> eval env b
evalB env (And p q)   = do
  l <- evalB env p
  if l then evalB env q else Right False    -- short-circuit, like (&&)
evalB env (Or p q)    = do
  l <- evalB env p
  if l then Right True else evalB env q     -- short-circuit, like (||)
evalB env (Not p)     = not <$> evalB env p

------------------------------------------------------------------------------
-- Pretty printing (used in error messages and in the demo output)
------------------------------------------------------------------------------

-- | Render an expression in ordinary infix notation, adding only the
-- parentheses that operator precedence requires.
pretty :: Expr -> String
pretty = prettyPrec 0

-- | Render a condition in infix notation.
prettyB :: BExpr -> String
prettyB = prettyPrecB 0

-- Precedences: if/let 0, (||) 2, (&&) 3, comparisons 4, (+ -) 6, (* /) 7.
-- All binary operators are left-associative, so the right operand is
-- printed one level tighter than the left one.
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

-- | Whole numbers are shown without a trailing ".0".
showNumber :: Double -> String
showNumber n
  | isWhole   = show (round n :: Integer)
  | otherwise = show n
  where
    isWhole = not (isInfinite n) && not (isNaN n)
              && abs n < 1e15 && n == fromInteger (round n)
