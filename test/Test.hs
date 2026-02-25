{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Merge
import GHC.Generics (Generic)
import Test.QuickCheck hiding (Success)

data Pair = Pair Int Int
  deriving (Eq, Show, Generic)

instance Arbitrary Pair where
  arbitrary = Pair <$> arbitrary <*> arbitrary

main :: IO ()
main = do
  -- required: agreement is identity
  check "required agree"
    \(a :: Int) -> runMerge (required @() id) a a == Success a

  -- required: disagreement is failure
  check "required disagree"
    \(a :: Int) b -> a /= b ==> runMerge (required @() id) a b === Error ()

  -- optional: Nothing is the unit
  check "optional unit left"
    \(b :: Maybe Int) -> runMerge (optional @() id) Nothing b == Success b
  check "optional unit right"
    \(a :: Maybe Int) -> runMerge (optional @() id) a Nothing == Success a

  -- optional: agreement on Just
  check "optional agree"
    \(a :: Int) -> runMerge (optional @() id) (Just a) (Just a) == Success (Just a)

  -- optional: disagreement on Just
  check "optional disagree"
    \(a :: Int) b -> a /= b ==> runMerge (optional @() id) (Just a) (Just b) === Error ()

  -- combine: always succeeds with the semigroup
  check "combine"
    \(a :: [Int]) b -> runMerge (combine @(Validation ()) id) a b == Success (a <> b)

  -- <*> accumulates errors from both sides
  check "error accumulation"
    \(a :: Int) b (c :: Int) d -> (a /= b && c /= d) ==>
      let m = (,) <$> required fst .? ["x"] <*> required snd .? ["y"]
      in runMerge m (a, c) (b, d) === Error ["x", "y"]

  -- genericMerge agrees with manual applicative
  check "generic = manual"
    \p1 p2 ->
      let manual = Pair <$> required (\(Pair x _) -> x)
                        <*> required (\(Pair _ y) -> y)
      in runMerge (genericMerge @()) p1 p2 == runMerge manual p1 (p2 :: Pair)

  -- hoist preserves semantics
  check "hoist"
    \(a :: Int) b ->
      let mm = Merge (\x y -> if x == y then Just x else Nothing)
          vm = hoist (\case Just x -> Success x; Nothing -> Error ()) mm
      in runMerge vm a b == if a == b then Success a else Error ()

  -- effect polymorphism: combine works in Maybe
  check "combine @Maybe"
    \(a :: [Int]) b -> runMerge (combine @Maybe id) a b == Just (a <> b)

  -- effect polymorphism: combine works in []
  check "combine @[]"
    \(a :: Sum Int) b -> runMerge (combine @[] id) a b == [a <> b]

check :: Testable prop => String -> prop -> IO ()
check name prop = do
  putStr (name ++ ": ")
  quickCheck prop
