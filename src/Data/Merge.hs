{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{- |
Name: Data.Merge
Description: To describe merging of data types.
License: MIT
Copyright: Samuel Schlesinger 2021 (c)
-}
{-# LANGUAGE BlockArguments #-}
module Data.Merge
  ( 
    -- * A Validation Applicative
    Validation(..)
  , validation
    -- * The Merge type
  , Merge (Merge, runMerge)
    -- * Construction
  , (.?)
  , merge
  , optional
  , required
  , combine
  , combineWith
  , combineGen
  , combineGenWith
  , Alternative(..)
  , Applicative(..)
    -- * Modification
  , flattenValidation
  , Profunctor(..)
    -- * Useful Semigroups
  , Optional(..)
  , Required(..)
  , Last (..)
  , First (..)
  , Product (..)
  , Sum (..)
  , Dual (..)
  , Max (..)
  , Min (..)
  ) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Control.Monad (join)
import Data.Coerce (Coercible, coerce)
import Control.Applicative (Alternative (..))
import Data.Profunctor (Profunctor (..))
import Data.Bifunctor (Bifunctor(..))
import Data.Semigroup (Last (..), First (..), Product (..), Sum (..), Dual (..), Max (..), Min (..))

-- | Like 'Either', but with an 'Applicative' instance which
-- accumulates errors using their 'Semigroup' operation.
data Validation e a =
    Error e
  | Success a
  deriving (Functor, Eq, Ord, Show, Read, Generic, Typeable)

validation :: (e -> r) -> (a -> r) -> Validation e a -> r
validation f g = \case
  Error e -> f e
  Success a -> g a

instance Bifunctor Validation where
  bimap f g (Error e) = Error (f e)
  bimap f g (Success a) = Success (g a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Error e   <*> Error e'  = Error (e <> e')
  Error e   <*> _         = Error e
  _         <*> Error e'  = Error e'

instance Monoid e => Alternative (Validation e) where
  empty = Error mempty
  Success a <|> x = Success a
  Error e <|> x = x

-- | Describes the merging of two values of the same type
-- into some other type. Represented as a 'Maybe' valued
-- function, one can also think of this as a predicate
-- showing which pairs of values can be merged in this way.
--
-- > data Example = Whatever { a :: Int, b :: Maybe Bool }
-- > mergeExamples :: Merge Example Example
-- > mergeExamples = Example <$> required a <*> optional b
newtype Merge e x a = Merge { runMerge :: x -> x -> Validation e a }

-- | Appends some errors. Useful for the combinators provided by this library,
-- which use 'mempty' to provide the default error type.
(.?) :: Semigroup e => Merge e x a -> e -> Merge e x a
m .? e = Merge \x x' -> bimap (e <>) id (runMerge m x x')

infixl 6 .?

-- | Flattens a 'Maybe' layer inside of a 'Merge'
flattenValidation :: Merge e x (Validation e a) -> Merge e x a
flattenValidation (Merge f) = Merge \x x' ->
  case f x x' of
    Error e -> Error e
    Success (Error e) -> Error e
    Success (Success a) -> Success a

-- | The most general combinator for constructing 'Merge's.
merge :: (x -> x -> Validation e a) -> Merge e x a
merge = Merge

instance Profunctor (Merge e) where
  dimap l r (Merge f) = Merge \x x' -> r <$> f (l x) (l x')

instance Functor (Merge e x) where
  fmap = rmap

instance Semigroup e => Applicative (Merge e x) where
  pure x = Merge (\_ _ -> Success x)
  fa <*> a = Merge \x x' -> runMerge fa x x' <*> runMerge a x x'

instance Monoid e => Alternative (Merge e x) where
  empty = Merge \_ _ -> empty
  Merge f <|> Merge g = Merge \x x' -> f x x' <|> g x x'

instance (Semigroup e, Semigroup a) => Semigroup (Merge e x a) where
  a <> b = Merge \x x' -> (<>) <$> runMerge a x x' <*> runMerge b x x'

instance (Monoid e, Semigroup a) => Monoid (Merge e x a) where
  mempty = Merge \_ _ -> Error mempty  

-- | Meant to be used to merge optional fields in a record.
optional :: (Monoid e, Eq a) => (x -> Maybe a) -> Merge e x (Maybe a)
optional = combineGen (maybe (Optional (Success Nothing)) (Optional . Success . Just)) unOptional

-- | Meant to be used to merge required fields in a record.
required :: forall e a x. (Monoid e, Eq a) => (x -> a) -> Merge e x a
required = combineGen (Required . Success) unRequired

-- | Associatively combine original fields of the record.
combine :: forall e a x. Semigroup a => (x -> a) -> Merge e x a
combine = combineWith (<>)

-- | Combine original fields of the record with the given function.
combineWith :: forall e a x. (a -> a -> a) -> (x -> a) -> Merge e x a
combineWith c f = Merge (\x x' -> go (f x) (f x')) where
  go x x' = Success (x `c` x')

-- | Sometimes, one can describe a merge strategy via a binary operator. 'Optional'
-- and 'Required' describe 'optional' and 'required', respectively, in this way.
combineGenWith :: forall e a x s. (s -> s -> s) -> (a -> s) -> (s -> Validation e a) -> (x -> a) -> Merge e x a
combineGenWith c g l f = flattenValidation $ fmap l $ combineWith c (g . f)

-- | 'combineGen' specialized to 'Semigroup' operations.
combineGen :: forall e a x s. Semigroup s => (a -> s) -> (s -> Validation e a) -> (x -> a) -> Merge e x a
combineGen = combineGenWith (<>)

-- | This type's 'Semigroup' instance encodes the simple,
-- discrete lattice generated by any given set, excluding the
-- bottom.
newtype Required e a = Required { unRequired :: Validation e a }
  deriving (Eq, Show, Read, Ord, Generic, Typeable)

instance (Monoid e, Eq a) => Semigroup (Required e a) where
  Required (Success a) <> Required (Success a')
    | a == a' = Required (Success a)
    | otherwise = Required (Error mempty)
  Required (Error e) <> Required (Success _) = Required (Error e)
  Required (Success _) <> Required (Error e') = Required (Error e')
  Required (Error e) <> Required (Error e') = Required (Error (e <> e'))

-- | This type's 'Semigroup' instance encodes the simple,
-- deiscrete lattice generated by any given set.
newtype Optional e a = Optional { unOptional :: Validation e (Maybe a) }
  deriving (Eq, Show, Read, Ord, Generic, Typeable)

instance (Monoid e, Eq a) => Semigroup (Optional e a) where
  Optional (Success (Just a)) <> Optional (Success (Just a'))
    | a == a' = Optional (Success (Just a))
    | otherwise = Optional (Error mempty)
  Optional (Success Nothing) <> x = x
  x <> Optional (Success Nothing) = x
  Optional (Error e) <> Optional (Error e') = Optional (Error (e <> e'))
  x <> Optional (Error e) = Optional (Error e)
  Optional (Error e) <> x = Optional (Error e)

instance (Monoid e, Eq a) => Monoid (Optional e a) where
  mempty = Optional (Success Nothing)
