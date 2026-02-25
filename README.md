# Merge

A library for consistently merging information from multiple sources.

## The Problem

Often, one finds themselves having multiple sources of knowledge
for some piece of data, and having to merge these together. Perhaps
we have a type representing partial information about a digital friend.

```haskell
data Friend = Friend
  { name :: Maybe Text
  , email :: Maybe Text
  , age :: Max Int
  , pubKey :: PublicKey
  }
```

If we learn some information about a friend from someone, and some
from someone else, we'll want to merge that information to have a
more complete picture. That said, it might not succeed, as we may
have inconsistent information like two different names or different
public keys.

## Applicative Merging

```haskell
mergeFriends :: Merge [String] Friend Friend
mergeFriends =
  Friend
    <$> optional name   .? ["name"]
    <*> optional email  .? ["email"]
    <*> combine age     .? ["age"]
    <*> required pubKey .? ["pubKey"]

f :: Friend -> Friend -> Validation [String] Friend
f = runMerge mergeFriends
```

The `Validation` type accumulates errors for every field that fails
to merge, rather than stopping at the first failure.

## Generic Deriving

For simple cases where all fields use `required` semantics (values
must agree), you can derive a merge automatically:

```haskell
data Config = Config { port :: Int, host :: String }
  deriving Generic

mergeConfigs :: Merge [String] Config Config
mergeConfigs = genericMerge
```

## Effect Parameterization

`Merge f x a` is parameterized by an effect `f`, allowing different
merge behaviors:

```haskell
-- Error-accumulating
configMerge :: Merge (Validation [String]) Config Config

-- Fail-fast
failFastMerge :: Merge (Either String) Config Config

-- Nondeterministic (all possible merges)
allMerges :: Merge [] Config Config

-- Convert between effects with hoist
hoist toValidation :: Merge Maybe x a -> Merge (Validation ()) x a
```

## Combinators

| Combinator | Behavior |
|---|---|
| `required` | Values must be equal |
| `optional` | `Nothing` yields to the other; two `Just`s must agree |
| `combine` | Merge via `Semigroup` |
| `combineWith` | Merge via explicit function |
| `.?` | Annotate errors |
| `genericMerge` | Derive from `Generic` (all fields `required`) |
