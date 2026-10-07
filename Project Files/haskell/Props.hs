{-# OPTIONS_GHC -Wall -Wno-orphans #-}
-- QuickCheck properties: cabal test props
module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf, stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck
import qualified Test.QuickCheck.Random as Random

import Expr

-- w is never bound in testEnv
varPool :: [String]
varPool = ["x", "y", "z", "w"]

genVar :: Gen Expr
genVar = Var <$> frequency [(3, pure "x"), (3, pure "y"), (1, pure "z"), (1, pure "w")]

testEnv :: Env
testEnv = [("x", 5), ("y", 2), ("z", 0)]

-- small whole numbers keep results finite (no Infinity or NaN)
genLit :: Gen Expr
genLit = Lit . fromIntegral <$> choose (-3, 3 :: Int)

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

prop_preservesSuccess :: Expr -> Property
prop_preservesSuccess e =
  case eval testEnv e of
    Left _  -> label "original fails (nothing to compare)" True
    Right v -> label "original succeeds" (eval testEnv (simplify e) === Right v)

-- expected to fail: simplify can hide an error (e.g. w - w becomes 0)
prop_naiveEquivalence :: Property
prop_naiveEquivalence = expectFailure $ \e ->
  succeeds (eval testEnv (simplify e)) === succeeds (eval testEnv e)
  where
    succeeds :: Either String Double -> Maybe Double
    succeeds = either (const Nothing) Just

prop_idempotent :: Expr -> Property
prop_idempotent e = simplify (simplify e) === simplify e

prop_neverGrows :: Expr -> Bool
prop_neverGrows e = size (simplify e) <= size e

prop_staticCheckSound :: Expr -> Property
prop_staticCheckSound e =
  null (undefinedVars testEnv e) ==>
    case eval testEnv e of
      Left err -> counterexample err (not ("undefined variable" `isPrefixOf` err))
      Right _  -> property True

prop_staticCheckComplete :: Expr -> Property
prop_staticCheckComplete e =
  case eval testEnv e of
    Left err | Just v <- stripPrefix "undefined variable: " err
            -> property (v `elem` undefinedVars testEnv e)
    _       -> property True

prop_batchAddsUp :: [Expr] -> Property
prop_batchAddsUp es =
  let r = batchReport testEnv es
  in conjoin
       [ succeeded r + failed r === length es
       , values r               === evalBatch testEnv es
       , length (failures r)    === failed r
       ]

prop_letShadows :: Double -> Double -> Property
prop_letShadows a b =
  eval testEnv (Let "x" (Lit a) (Let "x" (Lit b) (Var "x"))) === Right b

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
    -- fixed seed so the results are reproducible
    run :: Testable p => String -> p -> IO Result
    run name p = do
      putStrLn ("\n== " ++ name ++ " ==")
      quickCheckWithResult
        stdArgs { maxSuccess = 1000, replay = Just (Random.mkQCGen 2026, 0) } p
