# Merge

```haskell
data User = User
  { name :: Maybe Text
  , pubKey :: PublicKey
  }

mergeUsers :: Merge User User
mergeUsers =
  User
    <$> optional name
    <*> required pubKey

f :: User -> User -> Maybe User
f x y = runMerge mergeUsers x y
```
