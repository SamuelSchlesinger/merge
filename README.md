# Merge

Often, one finds themselves having multiple sources of knowledge
for some piece of data, and having to merge these together. Perhaps
we have a type representing partial information about a digital friend.

```haskell
data Friend = Friend
  { name :: Maybe Text
  , email :: Maybe Text
  , age :: Int
  , pubKey :: PublicKey
  }
```

If we learn some information about a friend from someone, and some
from someone else, we'll want to merge that information to have a
more complete picture. That said, it might not succeed, as we may
have inconsistent information like two different names or different
public keys. We'll want a function of type:

```haskell
f :: Friend -> Friend -> Maybe Friend
```

That's the pattern that this library encapsulates!

```
mergeFriends :: Merge Friend Friend
mergeFriends =
  User
    <$> optional name
    <*> optional email
    <*> combine Max
    <*> required pubKey

f :: Friend -> Friend -> Maybe Friend
f x y = runMerge mergeFriends x y
```
