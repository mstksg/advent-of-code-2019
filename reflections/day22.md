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
representation of them, `Perm`, we can write an instance of `Perm` for
`Semigroup`, `Monoid`, and `Group`, common abstractions in Haskell that many
types are already instances of.  Abstractions like `Semigroup` and `Monoid` are
pretty much an everyday thing in Haskell, so this fits in quite nicely.
`Group` comes from the *[groups][]* package, which also provides some nice
applications of group theory.

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
-- | Represents a permutation of n cards
data Perm n

-- | Given a permutation list, find the place where a given index ends up.
(@$) :: Perm n -> Finite n -> Finite n

-- | Parse a string line into the permutation it represents
parsePerm :: String -> Perm n

-- | Given a permutation list, find the place where 2019 ends up
part1 :: [Perm 10007] -> Finite 10007
part1 perms = bigPerm @$ 2019
  where
    bigPerm = mconcat perms
```

And...that's it!  For the actual "logic" of our part 1!

Here, I'm using `Finite n` from the great *[finite-typelits][]* library, where
`Finite 100` represents "an index between 0 and 99", etc.  It's just exactly
the right "shape" to represent the index of a deck of cards.  *finite-typelits*
wasn't designed with group theory in mind, but it's still a great tool here ---
which is a testament to how flexible these abstractions can actually be :)

[finite-typelits]: https://hackage.haskell.org/package/finite-typelits

We can plan out our part 2 as well:

```haskell
-- | Given a permutation list, find the index that will end up at 2020
part2 :: [Perm 119315717514047] -> Finite 119315717514047
part2 perms = invert biiigPerm @$ 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm
```

Part 2, I think, is where the group theory really shines.

1.  We take advantage of `stimes`, which uses repeated squaring.  That means
    that to compute `stimes 8 x`, instead of using `x <> x <> x <> x <> x <> x <> x <> x`, it does `let x2 = x <> x; x4 = x2 <> x2 in x4 <> x4`,
    essentially cutting down the number of multiplications exponentially.  This
    means that to compute `stimes 101741582076661`, we only need to do about 47
    multiplications (log base 2), and not 101741582076661.

    This is only possible because we know that permutation composition is
    associative, so it doesn't matter how we associate our parentheses.  It is
    only "safe" to use repeated squaring if you *know* that your operation is
    associative.  Having a semigroup abstraction *in the first place* guides us
    to this efficient solution --- in a way that is pre-built just for us!
    This is made all the more powerful because *semigroup* is a ubiquitous
    abstraction in Haskell, so we "think about" it all the time.

2.  Remember how `p @$ 2019` gives us the index that `2019` is sent to?  Well,
    we want something else in this case.  We basically want the index that
    *will be sent to* `2020`.  So, we want to *reverse the function*.  Luckily,
    since our function is just a permutation, it is easy to reverse this: just
    `invert` the permutation!

    The idea that we can simply invert a permutation instead of having to write
    a whole new permutation representation just to do "backwards indexing" is
    something that we are *guided to*, just by recognizing that permutations
    form a group.

Now, time to actually write our permutation representation -- the definition of
`Perm`.  A good first guess might be to write our permutation as an actual
function.  Then, we can just use function composition as our permutation
composition.


```haskell
data Perm n = Perm (Finite n -> Finite n)

(@$) :: Perm n -> Finite n -> Finite n
Perm f @$ x  = f x

parsePerm :: KnownNat n => String -> Perm n
parsePerm str = case words str of
    "cut":n:_           -> Perm $ \i -> i - modulo (read n)
    "deal":"into":_     -> Perm $ \i -> maxBound - i
    "deal":"with":_:n:_ -> Perm $ \i -> i * modulo (read n)

instance Semigroup (Perm n) where
    Perm f <> Perm g = Perm (f . g)
instance Monoid (Perm n) where
    mempty = Perm id
instance Group (Perm n) where
    invert (Perm f) = ?????
```

Note that `Finite n`'s `Num` instance is inherently modular arithmetic, so
things like `negate` and multiplication will "do the right thing". We use
`modulo`:

```haskell
modulo :: KnownNat n => Integer -> Finite n
```

which "reads" an `Integer` into a `Finite n`, making sure to wrap it in a
cyclic way if it is negative or too high.

This way works... but we see that there isn't any nice way to write `invert` for
this.  Also, `stimes` doesn't help us *too* much here, because repeated
squaring of function composition is...still a lot of function compositions in
the end.  So, back to the drawing board.

If we look carefully at `parsePerm`, we might start to see a pattern in all of
our permutations.  In fact, they all seem to follow the same form:

```haskell
"cut":n:_           -> Perm $ \i -> i - modulo (read n)
"deal":"into":_     -> Perm $ \i -> negate i + maxBound
"deal":"with":_:n:_ -> Perm $ \i -> i * modulo (read n)
```

They all seem to be some "scaling" and "adding" of `i`.  If we align things up,
this becomes a little more clear:

```haskell
"cut":n:_           -> Perm $ \i ->                1 * i - modulo (read n)
"deal":"into":_     -> Perm $ \i ->               -1 * i + maxBound
"deal":"with":_:n:_ -> Perm $ \i ->  modulo (read n) * i
```

Each of these seems to be some sort of scaling-and-adding of `i`...also known
as an [Affine Transformation][affine], but modulo some cyclic rotation.

[affine]: https://en.wikipedia.org/wiki/Affine_transformation

Well...affine transformations on cyclic indices are a subset of permutations in
general.  More importantly, we know (after some googling) that they are also
*closed with respect to composition and inversion* ... which means that they
are, themselves, a group!  Maybe we can represent this as our permutation type:

```haskell
data Affine n = Aff { aScale :: Finite n
                    , aShift :: Finite n
                    }

(@$) :: KnownNat n => Affine n -> Finite n -> Finite n
Aff a b @$ x = a * x + b

parseAffine :: KnownNat n => String -> Affine n
parseAffine str = case words str of
    "cut":n:_           -> Aff                1  (-modulo (read n))
    "deal":"into":_     -> Aff        (negate 1)          maxBound
    "deal":"with":_:n:_ -> Aff (modulo (read n))                 0
```

So far so good :)  Now to think of what our composition actions are.
Composing `a' x + b'` after `a x + b` is `a' (a x + b) + b'`, which is `a' a x +
a' b + b'`:

```haskell
instance KnownNat n => Semigroup (Affine n) where
    Aff a' b' <> Aff a b = Aff (a' * a) (a' * b + b')
```

The identity permutation just leaves x alone, `1 x + 0`:

```haskell
instance Monoid (Affine n) where
    mempty = Aff 1 0
```

*Inverting* something means that we want `invert p <> p == mempty`.  So that
means we want

```haskell
invert (Aff a b) <> Aff a b = Aff 1 0
       Aff a' b' <> Aff a b = Aff 1 0
 Aff (a' * a) (a' * b + b') = Aff 1 0
```

Which means we need `a' * a = 1`, and `a' * b + b' = 0`.  A quick google shows
us that if we want `a' * a = 1`, then we need `a' = a ^ (n - 2)`, or `a ^
(maxBound - 1)`.  The second case is a little simpler: we see that `b' = -(a' *
b)`

```haskell
instance KnownNat n => Group (Affine n) where
    invert (Aff a b) = Aff a' b'
      where
        a' = a ^ (maxBound @(Finite n) - 1)
        b' = negate $ a' * b
```

And...we're done!  This actually is pretty efficient with repeated squaring
because we are just squaring numbers.

Well, this feels a little anticlimactic, doesn't it?  Just to close us out,
I'll re-paste the code we planned before, now with the context that we have
implemented the appropriate permutation types.  We get the `[Affine n]`s by
using `parseAffine` on the `lines` of our input group (remembering to `reverse`
because that's how compositions work by convention).

```haskell
-- | Given a permutation list, find the place where 2019 ends up
part1 :: [Affine 10007] -> Finite 10007
part1 perms = bigPerm @$ 2019
  where
    bigPerm = mconcat perms

-- | Given a permutation list, find the index that will end up at 2020
part2 :: [Affine 119315717514047] -> Finite 119315717514047
part2 perms = invert biiigPerm @$ 2020
  where
    bigPerm   = mconcat perms
    biiigPerm = stimes 101741582076661 bigPerm
```

As expected, Haskell performs these ~47 multiplication steps pretty quickly,
and part 2 is only about 3 times slower than part 1 (~50μs vs. ~20μs).

Hopefully this is an illustrative story about taking advantage of how Haskell
frames abstractions (as typeclasses) to *guide* us to an answer that might not
have been obvious in the first place!
