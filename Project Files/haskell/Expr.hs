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
--   * Part C  (higher-order)    : 'simplify', 'simplifyB', 'evalAll',
--                                 'evalBatch', 'batchReport', 'freeVars',
--                                 'undefinedVars'
--   * Pretty printing           : 'pretty', 'prettyB'
module Expr
  ( -- * Part A: language design
    Expr (..)
  , BExpr (..)
  , Env
    -- * Part B: evaluation
  , eval
  , evalB
    -- * Part C: simplification and higher-order functions
  , simplify
  , simplifyB
  , evalAll
  , evalBatch
  , BatchReport (..)
  , batchReport
  , freeVars
  , undefinedVars
    -- * Pretty printing
  , pretty
  , prettyB
  ) where

import Data.Either (rights)
import Data.List (nub)

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
-- Part C: simplification
------------------------------------------------------------------------------

-- | Rewrite an expression using algebraic identities.
--
-- The children are simplified first (bottom-up), then one rule is tried at
-- the root. Every rule returns either an already-simplified sub-expression
-- or a literal, so a single pass is enough: @simplify (simplify e)@ equals
-- @simplify e@ (checked by @prop_idempotent@ in "Props").
--
-- Rules that throw a sub-expression away (@x * 0@, @x - x@ and an unused
-- @let@) also throw away any error hidden inside it. So 'simplify' keeps
-- every /successful/ result the same, but can turn a failing expression
-- into one that succeeds (see @prop_naiveEquivalence@ in "Props").
simplify :: Expr -> Expr
simplify = rewrite . descend
  where
    -- Step 1: simplify every child, keeping the node itself.
    descend :: Expr -> Expr
    descend e@(Lit _)      = e
    descend e@(Var _)      = e
    descend (Add a b)      = Add (simplify a) (simplify b)
    descend (Sub a b)      = Sub (simplify a) (simplify b)
    descend (Mul a b)      = Mul (simplify a) (simplify b)
    descend (Div a b)      = Div (simplify a) (simplify b)
    descend (Let x e body) = Let x (simplify e) (simplify body)
    descend (If c t e)     = If (simplifyB c) (simplify t) (simplify e)

    -- Step 2: apply the first identity that matches at the root.
    rewrite :: Expr -> Expr
    rewrite (Add (Lit a) (Lit b)) = Lit (a + b)          -- constant folding
    rewrite (Sub (Lit a) (Lit b)) = Lit (a - b)
    rewrite (Mul (Lit a) (Lit b)) = Lit (a * b)
    rewrite (Div (Lit a) (Lit b))
      | b /= 0                    = Lit (a / b)          -- never fold n / 0
    rewrite (Add e (Lit 0))       = e                    -- x + 0  =  x
    rewrite (Add (Lit 0) e)       = e                    -- 0 + x  =  x
    rewrite (Sub e (Lit 0))       = e                    -- x - 0  =  x
    rewrite (Sub a b)
      | a == b                    = Lit 0                -- x - x  =  0
    rewrite (Mul e (Lit 1))       = e                    -- x * 1  =  x
    rewrite (Mul (Lit 1) e)       = e                    -- 1 * x  =  x
    rewrite (Mul _ (Lit 0))       = Lit 0                -- x * 0  =  0
    rewrite (Mul (Lit 0) _)       = Lit 0                -- 0 * x  =  0
    rewrite (Div e (Lit 1))       = e                    -- x / 1  =  x
    rewrite (If (BLit True) t _)  = t                    -- known condition
    rewrite (If (BLit False) _ e) = e
    rewrite (Let x _ body)
      | x `notElem` freeVars body = body                 -- unused binding
    rewrite e                     = e

-- | Simplify a condition. These rules mirror the short-circuit behaviour
-- of 'evalB', so they never hide an error that evaluation would report.
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

------------------------------------------------------------------------------
-- Part C: higher-order functions over lists of expressions
------------------------------------------------------------------------------

-- | Evaluate many expressions against one shared environment.
--
-- Currying: @eval :: Env -> Expr -> Either String Double@ takes its
-- arguments one at a time, so the partial application @eval env@ is itself
-- a function @Expr -> Either String Double@, which 'map' applies to every
-- expression in the list.
evalAll :: Env -> [Expr] -> [Either String Double]
evalAll env = map (eval env)

-- | Keep only the results that succeeded (the scaffold's @evalBatch@).
evalBatch :: Env -> [Expr] -> [Double]
evalBatch env = rights . evalAll env

-- | Summary of evaluating a batch of expressions.
data BatchReport = BatchReport
  { succeeded :: Int        -- ^ how many expressions evaluated successfully
  , failed    :: Int        -- ^ how many produced an error
  , values    :: [Double]   -- ^ the successful results, in input order
  , failures  :: [String]   -- ^ the error messages, in input order
  } deriving (Eq, Show)

-- | Evaluate a batch and count successes versus failures in one 'foldr'.
batchReport :: Env -> [Expr] -> BatchReport
batchReport env = foldr tally (BatchReport 0 0 [] []) . evalAll env
  where
    tally :: Either String Double -> BatchReport -> BatchReport
    tally (Right v)  r = r { succeeded = succeeded r + 1
                           , values    = v : values r }
    tally (Left err) r = r { failed    = failed r + 1
                           , failures  = err : failures r }

-- | The variables an expression reads from its environment (those not
-- bound by an enclosing @let@), without duplicates.
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
    go (Let x e body) = go e ++ filter (/= x) (go body)   -- x is bound in body
    go (If c t e)     = goB c ++ go t ++ go e

    goB :: BExpr -> [String]
    goB (BLit _)    = []
    goB (Less a b)  = go a ++ go b
    goB (Equal a b) = go a ++ go b
    goB (And p q)   = goB p ++ goB q
    goB (Or p q)    = goB p ++ goB q
    goB (Not p)     = goB p

-- | A static check, done without evaluating anything: the free variables
-- of an expression that the environment does not bind. When the result is
-- empty, 'eval' can never fail with "undefined variable"
-- (@prop_staticCheckSound@ in "Props").
undefinedVars :: Env -> Expr -> [String]
undefinedVars env = filter (`notElem` bound) . freeVars
  where
    bound = map fst env

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
