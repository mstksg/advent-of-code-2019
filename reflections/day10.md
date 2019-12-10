Ah, a 2D lattice map problem -- a staple of Advent of Code, and a favorite to
many (including me!)

The first thing to do is get our map into a format we can use.  Using `V2 Int`
to represent a 2d point (because of its useful instances like `Num` and
`Applicative`), we want to get things into a `Set` of all asteroids.  This is
common enough that I have a pre-made utility function to handle this, but for
demonstration's sake we can implement it like:

```haskell
import qualified Data.Set as S

type Point = V2 Int

asteroidSet :: String -> Set Point
asteroidSet = ifoldMap (\y -> ifoldMap (\x -> crunch (V2 x y)))
            . lines
  where
    crunch p '#' = S.singleton p
    crunch _ _   = S.empty
```

Here I'm using the very handy `ifoldMap :: Monoid m => (Int -> a -> m) -> [a]`
from *[Control.Lens.Indexed][cli]*, which is a very useful function that I hope
will some day make it to *base*.  It's like `foldMap` with also the indices
available.

[cli]: https://www.stackage.org/haddock/lts-14.17/lens-4.17.1/Control-Lens-Indexed.html#v:ifoldMap

Anyway, how do we check if an asteroid is obscured?  There are probably many
good methods, but for me I found all the points in a straight line between two
asteroids, and checked if any of those items are in the asteroid field. (I did
attempt also to get the set of all unique angles, but that method ended up
being 10x slower for some reason? also using floating point equality makes me
feel queasy to my core)

```haskell
lineTo :: Point -> Point -> [Point]
lineTo p0 p1 = [ p0 + t *^ step | t <- [1 .. gcf  - 1] ]
  where
    d@(V2 dx dy) = p1 - p0
    gcf          = gcd dx dy
    step         = (`div` gcf) <$> d
```

Hopefully this shows at least is a good demonstration of why I like `V2 Int` as
`Point` so much.  We take advantages of its instances a lot, including:

*   Using the `Num` instance to compute the deltas, `V2 dx dy = p1 - p0`
*   Using the `Functor` instance to compute the step, `(`div` gcf) <$> d`
*   The handy scalar multiplication function `c *^ v`

I love `V2` :D

Anyway, the main crux of this algorithm is the list comprehension, which
computes the "steps" between the start and finish.

We can now check all the viewable points.

```haskell
viewableIn
    :: Set Point    -- ^ asteroid field
    -> Point        -- ^ vantage point
    -> Set Point    -- ^ all viewable points
viewableIn asteroids p = S.filter good (toList asteroids)
  where
    good q = p /= q
          && all (`S.notMember` asteroids) (lineTo p q)
```

Now we can do part 1:

```haskell
part1 :: Set Point -> Int
part1 asteroids = S.findMax $
    S.map (S.length . viewableIn asteroids) asteroids
```

For part 2, we are going to structure our program as an `unfoldr`.  Unfoldr
generates items while keeping some internal state.  We'll use the "currently
aimed at asteroid" and "asteroids left" as our state, and emit newly eliminated
asteroids.  Then we can simply get the 200th item in the resulting list:

```haskell
part2 :: Set Point -> Point
part2 asteroids =
    unfoldr (shootFrom station) (Nothing, asteroids) !! 199
  where
    station = maximumBy (comparing (S.size . viewableIn asteroids))
                asteroids
```

So we have `shootFrom` as our iterating function. Our "state" will be `Maybe
Point` (the asteroid our blaster is aimed at) and `Set Point`, the asteroid
field remaining.  We'll return `Nothing` when we run out of asteroids to
eliminate.

To implement `shootFrom`, it's useful to be able to sort all viewable asteroids
by the angle they make.  To do that, I made a function `angleFrom` which
computes the angle between two points, clockwise from vertical.  I use `atan2`
with some algebraic finessing to make sure north is the *minimal* amount, and
the direction moves appropriately (we flip its arguments and remember to invert
the `y` axis).

```haskell
angleTo :: Point -> Point -> Double
angleTo p0 p1 = atan2 (-fromIntegral dx) (fromIntegral dy)
  where
    V2 dx dy = p1 - p0
```

We now have all the parts to write `shootFrom`:

```haskell
shootFrom
    :: Point                                    -- ^ station
    -> (Maybe Point, Set Point)                 -- ^ current aim and remaining asteroids
    -> Maybe (Point, Maybe Point, Set Point))   -- ^ blasted asteroid, new aim, leftover field
shootFrom station (aim, asteroids) = guard (not (S.null asteroids)) $>
    case aim of
      Nothing ->
        let targ:next:_ = targetList
        in  (targ, (Just next, S.delete targ asteroids))
      Just a ->
        let targ:next:_ = dropWhile (/= a) targetList
        in  (targ, (Just next, S.delete targ asteroids))
  where
    targetList = cycle
               . sortOn (angleTo station)
               . toList
               $ viewableIn asteroids station
```

Our `targetList` is all of the remaining asteroids that are viewable from our
station, sorted by their angle from the station (0 being north, going
clockwise).  We `cycle :: [a] -> [a]` it, which loops it on itself forever, so
that the "next target" will always be the item *after* the current target.  It
turns `[a,b,c]` into `[a,b,c,a,b,c,a,b,c...]`, so if we want to ask "what
target comes after `c`?", we can see that `a` is after `c` in the cycled
version.

First, we use `guard` to return `Nothing` immediately if there are no asteroids
left.  But if there are asteroids left, we then check what we are aiming at. If
we aren't aiming at anything, just find the first item in the target list and
blast at that.  Otherwise, eat up the target list until we find the item we are
aiming at, and blast at that.  In both cases, the item after our target will be
the new item we are aiming at.

We just then need to make sure we delete our target in the new `Set Point`, to
remove it from the pool.

This one was a nice mix of math, geometry, spatial awareness, and a sense of
iterative algorithms (like `shootFrom`) -- for me, all of the best parts of an
Advent of Code challenge :)
