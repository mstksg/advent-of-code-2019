This one is pretty fun in Haskell because you get to use a trick that everyone
loves but nobody gets to use often enough --- [recursive knot tying][knot]!
Basically it's an idiomatic way to do [dynamic programming][dp] in Haskell by
taking advantage of lazy data structures ([this blog post][jelvis] is my
favorite explanation of it).

[knot]: https://wiki.haskell.org/Tying_the_Knot
[dp]: https://en.wikipedia.org/wiki/Dynamic_programming
[jelvis]: http://jelv.is/blog/Lazy-Dynamic-Programming/

The general idea is: let's say we had a map of children to parents, `Map String
String`.  To get the count of all indirect orbits, we can get a `Map String
Int`, a map of children to the number of parents and indirect parents above
them, and get the sum of those.

But how do we compute that?

Here, I'm going to show the "finale" first, and explain the way to get there:

```haskell
type Parent = String
type Child  = String

parents :: Map Child Parent

parentsCount     :: Map Child Int
parentsCount     = parents <&> \p -> case M.lookup p parentsCount of
    Nothing -> 1
    Just n  -> n + 1

parentsOfParents :: Map Child [Parent]
parentsOfParents = parents <&> \p -> case M.lookup p parentsOfParents of
    Nothing -> []
    Just ps -> p:ps
```

Fun, right?  And satisfyingly symmetrical.  That's more or less it!

So, how do we get there?

Let's call the child-parent map and the parent counts map as:

```haskell
type Parent = String
type Child  = String

parents      :: Map Child Parent
parentsCount :: Map Child Int
```


We see that the two have the same keys, so we can "map" a function over the
`parents` map to get `parentsCount`:

```haskell
parentsCount :: Map Child Int
parentsCount = fmap countTheParents parents

countTheParents :: Parent -> Int
countTheParents p = -- ?
```

So how do we `countTheParents`?  Well, we can look the parent up in
`parentsCount`, add one to the answer.  That's because if the parent has `n`
indirect parents, then the child has `n + 1` indirect parents:

```haskell
parentsCount :: Map Child Int
parentsCount = fmap countTheParents parents

countTheParents :: Parent -> Int
countTheParents p = case M.lookup p parentsCount of
    Nothing -> 1        -- count is 1
    Just n  -> n + 1    -- count is 1 + number of parents of parents
```

And that's it!


```haskell
part1 :: Int
part1 = sum parentsCount
````

The interesting thing here is that the leaves of `parentsCount` are lazily
evaluated --- so they can recursively refer to each other!

We can do `part2` in the same way, basically: we can build a list of parents of
parents of parents `"YOU"`, and then a list of parents of parents of parents of
`"SAN"`, and count the number of items that are unique to each.

```haskell
parentsOfParents :: Map Child [Parent]
parentsOfParents = fmap getPP parents

getPP :: Parent -> [Parent]
getPP p = case M.lookup p parentsOfParents of
    Nothing -> []       -- no parents
    Just pp -> p : pp   -- parent consed to parents of parents
```

Note that we actually could have defined `parentsCount` this way too:

```haskell
-- we could have done this
parentsCount :: Map Child Int
parentsCount = fmap length parentsOfParents
```

(But this is worse than the way we did it originally.  Do you see why?)


But anyway, for part 2, we will get the parents of parents of `"YOU"` and the
parents of parents of `"SAN"` and count the items that are unique to each:


```haskell
import qualified Data.Set as S

part2 :: Int
part2 = S.size onlyYou + S.size onlySan
  where
    Just you = M.lookup "YOU" parentsOfParents
    Just san = M.lookup "SAN" parentsOfParents
    onlyYou  = you S.\\ san     -- remove all items in `san` from `you`
    onlySan  = san S.\\ you     -- remove all items in `you` from `san`
```

Note that because the leaves in a `Map` are lazy, this will only actually
construct a list `[Parent]` for the keys that you look up --- parents lists for
keys you don't care about are never assembled.

The nice thing about recursive knot tying is that it gives a very concise and
readable way of saying "what you want":

```haskell
parentsCount :: Map Child Int
parentsCount = fmap countTheParents parents

countTheParents :: Parent -> Int
countTheParents p = case M.lookup p parentsCount of
    Nothing -> 1
    Just n  -> n + 1
```

This code is pretty easy to walk through, and logic of getting the parent count
(`countTheParents`) can be easily read as English: "If you get nothing when
you look up the parent in the parents count, then you only have one parent.
If you *do* get something, then it's one plus that something".

The recursive way here makes it much more readable in a "denotative" sense: you
say what it *is*, and the program/compiler figures out the rest for you.
Because of this, knot tying is often cited as one of the flashy "tech demos" of
denotative programming.  You might have seen someone write `fibs = 1 : 1 :
zipWith (+) fibs (tail fibs)` --- that's the same thing going on here.

And, with a lazy language like Haskell, it means that the leaves remain
unevaluated until we need them.  This will explode in your face in other
languages: if you evaluate all of the leaves "in order", then the first item
will depend on another unevaluated item, which might cause an error in other
languages.

It's always fun when a puzzle demonstrates so well a trick that is essential in
every Haskeller's tool belt :)
