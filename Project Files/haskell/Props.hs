{-# OPTIONS_GHC -Wall -Wno-orphans #-}
-- |
-- Module      : Main (property tests)
-- Description : QuickCheck properties for eval, simplify and the batch functions.
--
-- Run with:   cabal test props
--
-- Unit tests check chosen examples; these properties are checked on
-- thousands of randomly generated expressions. This is only possible
-- because 'eval' and 'simplify' are pure: the same input always gives the
-- same output, so a property can be tested by just calling the functions.
module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf, stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck
import qualified Test.QuickCheck.Random as Random

import Expr

------------------------------------------------------------------------------
-- Random expressions
------------------------------------------------------------------------------

-- | Generated expressions use these names. The test environment binds x, y
-- and z (with z = 0), but never w, so both kinds of error occur. x and y are
-- picked more often so that plenty of expressions also succeed.
varPool :: [String]
varPool = ["x", "y", "z", "w"]

genVar :: Gen Expr
genVar = Var <$> frequency [(3, pure "x"), (3, pure "y"), (1, pure "z"), (1, pure "w")]

testEnv :: Env
testEnv = [("x", 5), ("y", 2), ("z", 0)]

-- | Small whole numbers keep every intermediate result finite, so floating
-- point overflow (Infinity, NaN) cannot blur the comparisons below.
genLit :: Gen Expr
genLit = Lit . fromIntegral <$> choose (-3, 3 :: Int)

-- | An expression tree whose depth is bounded by the size parameter.
genExpr :: Int -> Gen Expr
genExpr 0 = oneof [genLit, genVar]
genExpr n = frequency
  [ (3, genExpr 0)
  , (2, Add <$> sub <*> sub)
  , (2, Sub <$> sub <*> sub)
  , (2, Mul <$> sub <*> sub)
  , (2, Div <$> sub <*> sub)
  , (1, Let <$> elements varPool <*> sub <*> sub)
  , (1, If  <$> genBExpr (n `div` 2) <*> sub <*> sub)
  ]
  where
    sub = genExpr (n `div` 2)

genBExpr :: Int -> Gen BExpr
genBExpr 0 = BLit <$> arbitrary
genBExpr n = frequency
  [ (1, BLit <$> arbitrary)
  , (2, Less  <$> sub <*> sub)
  , (2, Equal <$> sub <*> sub)
  , (1, And <$> bsub <*> bsub)
  , (1, Or  <$> bsub <*> bsub)
  , (1, Not <$> bsub)
  ]
  where
    sub  = genExpr (n `div` 2)
    bsub = genBExpr (n `div` 2)

instance Arbitrary Expr where
  arbitrary = sized genExpr
  shrink    = shrinkExpr

-- | Smaller versions of an expression, so that QuickCheck can report the
-- simplest counterexample it finds.
shrinkExpr :: Expr -> [Expr]
shrinkExpr (Lit n)        = [Lit 0 | n /= 0]
shrinkExpr (Var _)        = []
shrinkExpr (Add a b)      = shrinkBin Add a b
shrinkExpr (Sub a b)      = shrinkBin Sub a b
shrinkExpr (Mul a b)      = shrinkBin Mul a b
shrinkExpr (Div a b)      = shrinkBin Div a b
shrinkExpr (Let x e body) =
  [e, body] ++ [Let x e' body | e' <- shrinkExpr e]
            ++ [Let x e body' | body' <- shrinkExpr body]
shrinkExpr (If c t e)     =
  [t, e] ++ [If c' t e | c' <- shrinkBExpr c]
         ++ [If c t' e | t' <- shrinkExpr t]
         ++ [If c t e' | e' <- shrinkExpr e]

shrinkBin :: (Expr -> Expr -> Expr) -> Expr -> Expr -> [Expr]
shrinkBin node a b =
  [a, b] ++ [node a' b | a' <- shrinkExpr a] ++ [node a b' | b' <- shrinkExpr b]

shrinkBExpr :: BExpr -> [BExpr]
shrinkBExpr (BLit _)    = []
shrinkBExpr (Less a b)  = [BLit True, BLit False] ++ [Less a' b | a' <- shrinkExpr a]
                                                  ++ [Less a b' | b' <- shrinkExpr b]
shrinkBExpr (Equal a b) = [BLit True, BLit False] ++ [Equal a' b | a' <- shrinkExpr a]
                                                  ++ [Equal a b' | b' <- shrinkExpr b]
shrinkBExpr (And p q)   = [p, q] ++ [And p' q | p' <- shrinkBExpr p] ++ [And p q' | q' <- shrinkBExpr q]
shrinkBExpr (Or p q)    = [p, q] ++ [Or p' q | p' <- shrinkBExpr p] ++ [Or p q' | q' <- shrinkBExpr q]
shrinkBExpr (Not p)     = p : [Not p' | p' <- shrinkBExpr p]

-- | Number of nodes in an expression tree.
size :: Expr -> Int
size (Lit _)        = 1
size (Var _)        = 1
size (Add a b)      = 1 + size a + size b
size (Sub a b)      = 1 + size a + size b
size (Mul a b)      = 1 + size a + size b
size (Div a b)      = 1 + size a + size b
size (Let _ e body) = 1 + size e + size body
size (If c t e)     = 1 + sizeB c + size t + size e

sizeB :: BExpr -> Int
sizeB (BLit _)    = 1
sizeB (Less a b)  = 1 + size a + size b
sizeB (Equal a b) = 1 + size a + size b
sizeB (And p q)   = 1 + sizeB p + sizeB q
sizeB (Or p q)    = 1 + sizeB p + sizeB q
sizeB (Not p)     = 1 + sizeB p

------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

-- | simplify never changes the value of an expression that evaluates
-- successfully.
prop_preservesSuccess :: Expr -> Property
prop_preservesSuccess e =
  case eval testEnv e of
    Left _  -> label "original fails (nothing to compare)" True
    Right v -> label "original succeeds" (eval testEnv (simplify e) === Right v)

-- | The stronger claim "simplify never changes whether evaluation succeeds"
-- is FALSE: x * 0 = 0, x - x = 0 and removing an unused let all throw away
-- a sub-expression together with any error inside it. 'expectFailure' makes
-- QuickCheck search for a counterexample and pass only if it finds one.
-- (Error messages are ignored here, since they quote the expression.)
prop_naiveEquivalence :: Property
prop_naiveEquivalence = expectFailure $ \e ->
  succeeds (eval testEnv (simplify e)) === succeeds (eval testEnv e)
  where
    succeeds :: Either String Double -> Maybe Double
    succeeds = either (const Nothing) Just

-- | Bottom-up rewriting reaches a normal form in a single pass.
prop_idempotent :: Expr -> Property
prop_idempotent e = simplify (simplify e) === simplify e

-- | simplify never makes an expression bigger.
prop_neverGrows :: Expr -> Bool
prop_neverGrows e = size (simplify e) <= size e

-- | If the static check finds no unbound variables, eval never fails with
-- "undefined variable".
prop_staticCheckSound :: Expr -> Property
prop_staticCheckSound e =
  null (undefinedVars testEnv e) ==>
    case eval testEnv e of
      Left err -> counterexample err (not ("undefined variable" `isPrefixOf` err))
      Right _  -> property True

-- | Whenever eval reports an undefined variable, the static check had
-- already found that variable.
prop_staticCheckComplete :: Expr -> Property
prop_staticCheckComplete e =
  case eval testEnv e of
    Left err | Just v <- stripPrefix "undefined variable: " err
            -> property (v `elem` undefinedVars testEnv e)
    _       -> property True

-- | batchReport accounts for every expression in the batch exactly once.
prop_batchAddsUp :: [Expr] -> Property
prop_batchAddsUp es =
  let r = batchReport testEnv es
  in conjoin
       [ succeeded r + failed r === length es
       , values r               === evalBatch testEnv es
       , length (failures r)    === failed r
       ]

-- | The innermost let binding wins.
prop_letShadows :: Double -> Double -> Property
prop_letShadows a b =
  eval testEnv (Let "x" (Lit a) (Let "x" (Lit b) (Var "x"))) === Right b

------------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------------

main :: IO ()
main = do
  results <- sequence
    [ run "simplify preserves every successful result" prop_preservesSuccess
    , run "simplify can hide an error (counterexample expected)" prop_naiveEquivalence
    , run "simplify is idempotent"                     prop_idempotent
    , run "simplify never grows an expression"         prop_neverGrows
    , run "undefinedVars is sound"                     prop_staticCheckSound
    , run "undefinedVars is complete"                  prop_staticCheckComplete
    , run "batchReport adds up"                        prop_batchAddsUp
    , run "let shadowing"                              prop_letShadows
    ]
  unless (all isSuccess results) exitFailure
  where
    -- A fixed seed makes every run (and the output quoted in the report)
    -- reproducible.
    run :: Testable p => String -> p -> IO Result
    run name p = do
      putStrLn ("\n== " ++ name ++ " ==")
      quickCheckWithResult
        stdArgs { maxSuccess = 1000, replay = Just (Random.mkQCGen 2026, 0) } p
