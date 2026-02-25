{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |
-- A library for merging data from multiple sources, parameterized
-- by an effect. Use 'Validation' for error-accumulating merges,
-- 'Either' for fail-fast, @[]@ for nondeterministic, etc.
module Data.Merge
  ( -- * Validation
    Validation(..)
  , validation
    -- * Merge
  , Merge(..)
    -- * Construction
  , combine
  , combineWith
  , required
  , optional
  , (.?)
  , (.!)
    -- * Modification
  , flatten
  , flattenValidation
  , hoist
    -- * Generic Deriving
  , GMerge(..)
  , genericMerge
    -- * Carrier Semigroups
  , Optional(..)
  , Required(..)
    -- * Re-exports
  , Alternative(..)
  , Applicative(..)
  , Profunctor(..)
  , Last (..)
  , First (..)
  , Product (..)
  , Sum (..)
  , Dual (..)
  , Max (..)
  , Min (..)
  ) where

import GHC.Generics (Generic, Rep, from, to, U1(..), K1(..), M1(..), (:*:)(..), (:+:)(..))
import Data.Typeable (Typeable)
import Control.Applicative (Alternative (..))
import Data.Profunctor (Profunctor (..))
import Data.Bifunctor (Bifunctor(..))
import Data.Semigroup (Last (..), First (..), Product (..), Sum (..), Dual (..), Max (..), Min (..))

-- ================================================================
-- Validation
-- ================================================================

-- | Like 'Either', but accumulates errors via 'Semigroup'.
data Validation e a =
    Error e
  | Success a
  deriving (Functor, Eq, Ord, Show, Read, Generic, Typeable)

validation :: (e -> r) -> (a -> r) -> Validation e a -> r
validation f g = \case
  Error e -> f e
  Success a -> g a

instance Bifunctor Validation where
  bimap f _ (Error e) = Error (f e)
  bimap _ g (Success a) = Success (g a)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Success f <*> Success x = Success (f x)
  Error e   <*> Error e'  = Error (e <> e')
  Error e   <*> _         = Error e
  _         <*> Error e'  = Error e'

instance Monoid e => Alternative (Validation e) where
  empty = Error mempty
  Success a <|> _ = Success a
  Error _   <|> x = x

-- ================================================================
-- Merge
-- ================================================================

-- | Merge two values of type @x@ into an @a@ within effect @f@.
newtype Merge f x a = Merge { runMerge :: x -> x -> f a }

instance Functor f => Profunctor (Merge f) where
  dimap l r (Merge f) = Merge \x x' -> r <$> f (l x) (l x')

instance Functor f => Functor (Merge f x) where
  fmap g (Merge f) = Merge \x x' -> g <$> f x x'

instance Applicative f => Applicative (Merge f x) where
  pure x = Merge (\_ _ -> pure x)
  Merge fa <*> Merge a = Merge \x x' -> fa x x' <*> a x x'

instance Alternative f => Alternative (Merge f x) where
  empty = Merge \_ _ -> empty
  Merge f <|> Merge g = Merge \x x' -> f x x' <|> g x x'

instance (Applicative f, Semigroup a) => Semigroup (Merge f x a) where
  a <> b = Merge \x x' -> (<>) <$> runMerge a x x' <*> runMerge b x x'

instance (Alternative f, Semigroup a) => Monoid (Merge f x a) where
  mempty = Merge \_ _ -> empty

-- ================================================================
-- Construction
-- ================================================================

-- | Combine using a 'Semigroup'.
combine :: (Applicative f, Semigroup a) => (x -> a) -> Merge f x a
combine = combineWith (<>)

-- | Combine using an explicit function.
combineWith :: Applicative f => (a -> a -> a) -> (x -> a) -> Merge f x a
combineWith c f = Merge \x x' -> pure (f x `c` f x')

-- | Values must be equal or the merge fails.
required :: forall e a x. (Monoid e, Eq a) => (x -> a) -> Merge (Validation e) x a
required f = Merge \x x' ->
  let a = f x; b = f x'
  in if a == b then Success a else Error mempty

-- | 'Nothing' yields to the other side; two 'Just's must agree.
optional :: (Monoid e, Eq a) => (x -> Maybe a) -> Merge (Validation e) x (Maybe a)
optional f = Merge \x x' -> case (f x, f x') of
  (Nothing, b)       -> Success b
  (a, Nothing)       -> Success a
  (Just a, Just b)
    | a == b         -> Success (Just a)
    | otherwise      -> Error mempty

-- | Annotate errors.
(.?) :: Semigroup e => Merge (Validation e) x a -> e -> Merge (Validation e) x a
m .? e = Merge \x x' -> bimap (e <>) id (runMerge m x x')

-- | Transform the error type.
(.!) :: Merge (Validation e) x a -> (e -> e') -> Merge (Validation e') x a
m .! f = Merge \x x' -> bimap f id (runMerge m x x')

infixl 6 .?

-- ================================================================
-- Modification
-- ================================================================

-- | Flatten a nested effect via 'Monad'.
flatten :: Monad f => Merge f x (f a) -> Merge f x a
flatten (Merge f) = Merge \x x' -> f x x' >>= id

-- | Flatten a nested 'Validation'. Unlike 'flatten', this does not
-- require 'Monad', since 'Validation' cannot have a lawful one.
flattenValidation :: Merge (Validation e) x (Validation e a) -> Merge (Validation e) x a
flattenValidation (Merge f) = Merge \x x' ->
  case f x x' of
    Error e            -> Error e
    Success (Error e)  -> Error e
    Success (Success a) -> Success a

-- | Change the effect via a natural transformation.
hoist :: (forall b. f b -> g b) -> Merge f x a -> Merge g x a
hoist nat (Merge f) = Merge \x x' -> nat (f x x')

-- ================================================================
-- Generic Deriving
-- ================================================================

-- | Derive a merge where all fields use 'required' semantics.
-- Different constructors cannot be merged.
--
-- @
-- data Config = Config { port :: Int, name :: String }
--   deriving Generic
--
-- mergeConfigs :: Merge (Validation [String]) Config Config
-- mergeConfigs = genericMerge
-- @
genericMerge :: forall e a. (Generic a, GMerge e (Rep a)) => Merge (Validation e) a a
genericMerge = dimap from to gmerge

-- | Generic merge over representation types.
class GMerge e f where
  gmerge :: Merge (Validation e) (f p) (f p)

instance Semigroup e => GMerge e U1 where
  gmerge = pure U1

instance (Semigroup e, GMerge e f, GMerge e g) => GMerge e (f :*: g) where
  gmerge = (:*:) <$> lmap (\(l :*: _) -> l) gmerge
                  <*> lmap (\(_ :*: r) -> r) gmerge

instance (Monoid e, GMerge e f, GMerge e g) => GMerge e (f :+: g) where
  gmerge = Merge \x y -> case (x, y) of
    (L1 a, L1 b) -> L1 <$> runMerge gmerge a b
    (R1 a, R1 b) -> R1 <$> runMerge gmerge a b
    _             -> Error mempty

instance (Monoid e, Eq c) => GMerge e (K1 i c) where
  gmerge = K1 <$> required (\ (K1 x) -> x)

instance GMerge e f => GMerge e (M1 i t f) where
  gmerge = M1 <$> lmap (\ (M1 x) -> x) gmerge

-- ================================================================
-- Carrier Semigroups
-- ================================================================

-- | Discrete lattice with bottom ('Nothing').
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
  Optional (Error e) <> _ = Optional (Error e)

instance (Monoid e, Eq a) => Monoid (Optional e a) where
  mempty = Optional (Success Nothing)

-- | Discrete lattice without bottom (values must agree).
newtype Required e a = Required { unRequired :: Validation e a }
  deriving (Eq, Show, Read, Ord, Generic, Typeable)

instance (Monoid e, Eq a) => Semigroup (Required e a) where
  Required (Success a) <> Required (Success a')
    | a == a' = Required (Success a)
    | otherwise = Required (Error mempty)
  Required (Error e) <> Required (Success _) = Required (Error e)
  Required (Success _) <> Required (Error e') = Required (Error e')
  Required (Error e) <> Required (Error e') = Required (Error (e <> e'))
