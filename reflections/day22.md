Today's challenge, I think, shows a lot of advantages in the ways that Haskell
approaches mathematical abstractions :)

Unlike the other reflections, today I'm not going to explain "how I do it", as
much as "how I came up with the answer" --- and hopefully try to show how
Haskell's framing of mathematical abstractions like groups help *guide* us to
the answer.

Reading the problem, the initial thought is that we have what is essentially a
composition of [permutations][] -- the mathematical word for "shuffle",
basically.

[permutations]: https://en.wikipedia.org/wiki/Permutation

One of the most famous properties of permutations is that they are a "group",
which means they can be composed (associatively), have an identity, and can be
inverted.  This means that if you have two permutations, you can "squish" them
to create a new permutation, and work with that *new* permutation the same way.
I've talked about [using group theory][group theory] principles to help guide
us towards solutions and optimizations in Advent of Code challenges in the
past.

The *first* big advantage here is that we can treat our transformations *as
data*, and not as functions.  And that if we have two transformations, we can
always create a new one (just a normal data type value) that represents the
composition of the two original ones.

[group theory]: https://blog.jle.im/entry/alchemical-groups.html

Knowing permutations are a group, it means that once we settle on our
representation of them, `P`, we can write an instance of `P` for `Semigroup`,
`Monoid`, and `Group`, common abstractions in Haskell that many types are
already instances of.  Abstractions like `Semigroup` and `Monoid` are pretty
much an everyday thing in Haskell, so this fits in quite nicely.  `Group` comes
from the *[groups][]* package, which also provides some nice applications of
group theory.

[groups]: https://hackage.haskell.org/package/groups

```haskell
class Semigroup p where
    -- | permutation composition: compose two permutations to yield a new one
    (<>) :: p -> p -> p

    -- | extreeeemely efficient way of composing a permutation with itself
    -- multiple times, thanks to group theory
    stimes :: Int -> p -> p

class Monoid p where
    -- | the identity permutation, where p <> mempty = p
    mempty :: p

class Group p where
    -- | invert a permutation. so p <> invert p = mempty
    invert :: p -> p
```

Just *knowing* that permutations form a group naturally guides us to these
abstractions --- we already know what *interface* our type will have, even
before we write any code.  We know that no matter *what* our implementation of
permutation will be, we will have `(<>)`, `stimes`, `mempty`, `invert`
available to us to use.  So, let's do just that!  We'll use a stub data type
`Perm` to represent our permutation and "pretend" we have that interface on it.
We'll write our function first and then fill in the interface later!

```haskell
-- | Represents a permutation of cards
data Perm

-- | Given a permutation, find the place where a given index ends up
traceIndex :: Perm -> Int -> Int

-- | Given a permutation, find the place where 2019 ends up
part1 :: [Perm] -> Int
part1 perms = traceIndex bigPerm 2019
  where
    bigPerm = mconcat perms
```

And that's it!


