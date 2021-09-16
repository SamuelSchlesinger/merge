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
  let merge = (,) <$> optional fst <*> required snd
  requires "merge"
    [ runMerge merge (Just 10, 1) (Nothing, 1) == Just (Just 10, 1) 
    , runMerge merge (Nothing, 1) (Nothing, 1) == Just (Nothing, 1)
    , runMerge merge (Nothing, 1) (Nothing, 2) == Nothing
    , runMerge merge (Just 10, 1) (Just 11, 1) == Nothing
    , runMerge merge (Just 10, 1) (Just 11, 2) == Nothing
    ]
