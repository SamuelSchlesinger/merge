{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (mapM_)
import System.Exit (exitFailure, exitSuccess)
import Data.Merge

requires :: String -> [Bool] -> IO ()
requires msg = mapM_ (uncurry go) . zip [1..] where
  go n x
    | x = pure ()
    | otherwise = putStrLn (msg <> ": " <> show n) >> exitFailure

main :: IO ()
main = do
  let
    merge = (,,)
      <$> optional (\(x,_,_) -> x) .? ["fst"]
      <*> required (\(_,x,_) -> x) .? ["snd"]
      <*> combine (\(_,_,x) -> x)  .? ["thd"]
    merge' = combine Max .? ["max"]
    merge'' = combine Last .? ["last"]
  requires "merge"
    [ runMerge merge (Just 10, 1, []) (Nothing, 1, [1]) == Success (Just 10, 1, [1]) 
    , runMerge merge (Nothing, 1, [2]) (Nothing, 1, [3]) == Success (Nothing, 1, [2, 3])
    , runMerge merge (Nothing, 1, [1, 2]) (Nothing, 2, [3, 4]) == Error ["snd"]
    , runMerge merge (Just 10, 1, [7]) (Just 11, 1, []) == Error ["fst"]
    , runMerge merge (Just 10, 1, []) (Just 11, 2, []) == Error ["fst", "snd"]
    , runMerge merge' 5 10 == Success (Max 10)
    , runMerge merge'' True False == Success (Last False)
    , (((,) <$> Error "Hello" <*> Success 10) :: Validation String (Bool, Int)) == Error "Hello"
    , (((,) <$> Success True <*> Success (10 :: Int)) :: Validation String (Bool, Int)) == Success (True, 10)
    ]
