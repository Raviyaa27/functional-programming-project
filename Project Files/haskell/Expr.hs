{-# OPTIONS_GHC -Wall #-}
-- |
-- Module      : Expr
-- Description : A small, type-safe arithmetic expression interpreter.
-- Author      : Achintha H.G.R. (EG/2021/4384)
--
-- EC8206 Functional Programming - Project Assignment.
--
--   * Part A  (language design) : 'Expr', 'BExpr', 'Env'
module Expr
  ( -- * Part A: language design
    Expr (..)
  , BExpr (..)
  , Env
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
