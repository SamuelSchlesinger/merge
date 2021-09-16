{- |
Name: Data.Merge
Description: To describe merging of data types.
License: MIT
Copyright: Samuel Schlesinger 2021 (c)
-}
{-# LANGUAGE BlockArguments #-}
module Data.Merge where

import Control.Monad (join)
import Control.Applicative (Alternative (..))
import Data.Profunctor (Profunctor (..))

-- | Describes the merging of two values of the same type
-- into some other type. Represented as a 'Maybe' valued
-- function, one can also think of this as a predicate
-- showing which pairs of values can be merged in this way.
--
-- > data Example = Whatever { a :: Int, b :: Maybe Bool }
-- > mergeExamples :: Merge Example Example
-- > mergeExamples = Example <$> required a <*> optional b
newtype Merge x a = Merge { runMerge :: x -> x -> Maybe a }

-- | The most general combinator for constructing 'Merge's.
merge :: (x -> x -> Maybe a) -> Merge x a
merge = Merge

instance Profunctor Merge where
  dimap l r (Merge f) = Merge \x x' -> r <$> f (l x) (l x')

instance Functor (Merge x) where
  fmap = rmap

instance Applicative (Merge x) where
  pure x = Merge (\_ _ -> Just x)
  fa <*> a = Merge \x x' -> runMerge fa x x' <*> runMerge a x x'

instance Alternative (Merge x) where
  empty = Merge \_ _ -> Nothing
  Merge f <|> Merge g = Merge \x x' -> f x x' <|> g x x'

instance Monad (Merge x) where
  a >>= f = Merge \x x' -> join $ fmap (\b -> runMerge b x x') $ fmap f $ runMerge a x x'

instance Semigroup a => Semigroup (Merge x a) where
  a <> b = Merge \x x' -> runMerge a x x' <> runMerge b x x'

instance Semigroup a => Monoid (Merge x a) where
  mempty = Merge \_ _ -> mempty  

-- | Meant to be used to merge optional fields in a record.
optional :: Eq a => (x -> Maybe a) -> Merge x (Maybe a)
optional f = Merge (\x x' -> go (f x) (f x'))  where
  go (Just x) (Just x')
    | x == x' = Just (Just x)
    | otherwise = Nothing
  go Nothing (Just x) = Just (Just x)
  go (Just x) Nothing = Just (Just x)
  go Nothing Nothing = Just Nothing

-- | Meant to be used to merge required fields in a record.
required :: Eq a => (x -> a) -> Merge x a
required f = Merge (\x x' -> go (f x) (f x'))  where
  go x x'
    | x == x' = Just x
    | otherwise = Nothing
