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
  let merge = (,,) <$> optional (\(x,_,_) -> x) <*> required (\(_,x,_) -> x) <*> combine (\(_,_,x) -> x)
  let merge' = combine Max
  let merge'' = combine Last
  requires "merge"
    [ runMerge merge (Just 10, 1, []) (Nothing, 1, [1]) == Just (Just 10, 1, [1]) 
    , runMerge merge (Nothing, 1, [2]) (Nothing, 1, [3]) == Just (Nothing, 1, [2, 3])
    , runMerge merge (Nothing, 1, [1, 2]) (Nothing, 2, [3, 4]) == Nothing
    , runMerge merge (Just 10, 1, [7]) (Just 11, 1, []) == Nothing
    , runMerge merge (Just 10, 1, []) (Just 11, 2, []) == Nothing
    , runMerge merge' 5 10 == Just (Max 10)
    , runMerge merge'' True False == Just (Last False)
    ]
