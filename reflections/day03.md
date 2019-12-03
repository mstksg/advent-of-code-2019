As another data processing one, I feel like this might be another win for
Haskell as well :)  I got stuck in a silly bug for my part 1, but for part 2 my
leaderboard position jumped up considerably in comparison --- my suspicion is
that the new twist made it difficult for imperative coders, but the twist was
naturally handled in the Haskell case.

First off, I'm going to parse the path not as a series of directions and
numbers, but rather as a list of each individual step to take.  This was
similar to my approach for [2016 Day 1][y16d1].  I'm using my favorite type for
describing points, [V2][v2], because it has a really useful `Num` instance to
support addition of points.

[y17d1]: https://adventofcode.com/2016/day/1
[v2]: https://hackage.haskell.org/package/linear/docs/Linear-V2.html

```haskell
import           Data.List.Split
import           Linear.V2

parsePath :: String -> [V2 Int]
parsePath = concatMap parsePoint . splitOn ","
  where
    parsePoint (d:ns) = replicate (read ns) $ case d of
      'U' -> V2   0    1
      'R' -> V2   1    0
      'D' -> V2   0  (-1)
      'L' -> V2 (-1)   0
    parsePoint _      = []
```

Now, our list of points is simply a cumulative sum, which comes from our best
friend `scanl'`.  We use `scanl'` to get the running sum of all the direction
pieces, drop the first point (at `V2 0 0`), and get the set of all points.

```haskell
visited :: [V2 Int] -> Set (V2 Int)
visited = S.fromList . drop 1 . scanl' (+) (V2 0 0)
```

Now Part 1 is:

```haskell
part1 :: String -> Int
part1 str = minimum (S.map mannDist (S.intersection xs ys))
  where
    [xs, ys] = map (visited . parsePath) (lines str)
    mannDist (V2 x y) = abs x + abs y
```

Once we get the intersection (the set of points that are
visited by both), we can map the `mannDist` over each intersection and find the
minimum.

Part 2 adds an "extra twist", in that now we also want to keep track of the
time it takes to reach each point.  This requires only a small tweak to
`visited`:

```haskell
visited2 :: [V2 Int] -> Map (V2 Int) Int
visited2 = M.fromListWith min        -- turn it into a map, keeping first seen
         . drop 1                    -- drop the first item
         . flip zip [0..]            -- list of (sum, time taken)
         . scanl' (+) (V2 0 0)       -- running sum
```

We pair each item in the running sum with the time taken, and so get a map of
points seen to time taken to get to that point.  We make sure to use
`M.fromListWith min` so that we keep the *lowest* time at each point.

Part 2 is very similar, then:

```haskell
part2 :: String -> Int
part2 str = minimum (M.intersectionWith (+) xs ys)
  where
    [xs, ys] = map (visited2 . parsePath) (lines str)
```

Using `M.intersectionWith (+)` instead of `S.intersection`, because we want the
map that has the same keys in both paths, while adding together the times at
each key.

Note that we can actually solve `part1` using `visited2` instead of
`visited`...because we can "forget" the values in a `Map (V2 Int) Int` by using
`M.keysSet :: Map k a -> Set k`.
