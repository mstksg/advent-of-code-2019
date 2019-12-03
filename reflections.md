Reflections
===========

Table of Contents
-----------------

* [Day 1](#day-1)
* [Day 2](#day-2)


Day 1
------

*[Prompt][d01p]* / *[Code][d01g]* / *[Rendered][d01h]*

[d01p]: https://adventofcode.com/2019/day/1
[d01g]: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Challenge/Day01.hs
[d01h]: https://mstksg.github.io/advent-of-code-2019/src/AOC.Challenge.Day01.html


Haskell has a history of making Day 1's seem trivial :)  In this case it's a
straightforward map:

```haskell
fuel :: Int -> Int
fuel = subtract 2 . (`div` 3)

part1 :: [Int] -> Int
part1 = sum . map fuel

part2 :: [Int] -> Int
part2 = sum . map (sum . drop 1 . takeWhile (>= 0) . iterate fuel)
```

These can be parsed with `map read . lines`!

I accidentally forgot the `drop 1` the first time I submitted, so I hit the
cooldown.  Teaches me to remember to test all my answers next time :)


### Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 242.6 μs   (239.5 μs .. 246.9 μs)
                     0.987 R²   (0.969 R² .. 0.999 R²)
mean                 249.2 μs   (240.2 μs .. 267.0 μs)
std dev              40.28 μs   (11.10 μs .. 64.54 μs)
variance introduced by outliers: 91% (severely inflated)

>> Day 01b
benchmarking...
time                 267.4 μs   (265.7 μs .. 270.5 μs)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 267.9 μs   (266.1 μs .. 271.6 μs)
std dev              8.434 μs   (3.973 μs .. 14.26 μs)
variance introduced by outliers: 27% (moderately inflated)
```



Day 2
------

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2019/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2019/src/AOC.Challenge.Day02.html

So the bytecode/VM problems start day 2 this year, eh?

This one was also pretty straightforward.  For these types of problems, I like
to use `Data.IntMap` or `Data.Sequence` for the memory, since they both have
*O(log n)* indexing.  `Data.Sequence` is the better choice here because it's
basically `IntMap` with the indices (0, 1, 2 ...) automatically given for us :)

I usually use `Data.Sequence` instead of `Data.Vector` because it has a better
story when you want to change the length (by adding or removing elements):
`Data.Vector` is very bad, unless you have some sort of amortized abstraction.
However, in this case we don't ever change the length, so `Data.Vector` is
technically just as good here :)

So parsing:

```haskell
import           Data.List.Split (splitOn)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

type Memory = (Int, Seq Int)

parse :: String -> Memory
parse = (0,) . Seq.fromList . map read . splitOn ","
```

We write our stepping function:

```haskell
step :: Memory -> Maybe Memory
step (p, r) = do
    o <- Seq.lookup p r >>= \case
      1 -> pure (+)
      2 -> pure (*)
      _ -> empty
    [a, b, c] <- traverse (`Seq.lookup` r) [p+1 .. p+3]
    [y, z]    <- traverse (`Seq.lookup` r) [a,b]
    pure (p + 4, Seq.update c (o y z) r)
```

And away we go!

```haskell
runProg :: Memory -> Maybe Int
runProg m@(_,r) = case step m of
  Nothing -> Seq.lookup 0 r
  Just m' -> runProg m'

part1 :: String -> Maybe Int
part1 str = runProg (p, r')
  where
    (p,r) = parse str
    r'    = Seq.update 1 12 . Seq.update 2 2 $ r
```

For part 2 we can just do a brute force search

```haskell
part2 :: String -> Maybe (Int, Int)
part2 str = listToMaybe
    [ (noun, verb)
    | noun <- [0..99]
    , verb <- [0..99]
    , let r' = Seq.update 1 noun . Seq.update 2 verb $ r
    , runProg (p, r') == Just 19690720
    ]
  where
    (p, r) = parse str
```

This doesn't take too long on my machine!  But for my [actual solution][d02g],
I actually used a binary search (that I had coded up for last year). I
noticed that `noun` increases the answer by a lot, and `verb` increases it by a
little, so by doing an binary search on `noun`, then an binary search
on `verb`, you can get a good answer pretty quickly.  My part 2 time (470 μs)
is only twice as long as my part 1 time (260 μs) with the binary search. Happy
that some prep time paid off :)

```haskell
part2' :: String -> Maybe (Int, Int)
part2' str =  do
    noun <- binaryMinSearch (\i ->
        runProg (p, Seq.update 1 (i + 1) r) > Just moon
      ) 0 99
    let r' = Seq.update 1 noun r
    verb <- binaryMinSearch (\i ->
        runProg (p, Seq.update 2 (i + 1) r) > Just moon
      ) 0 99
    pure (noun, verb)
  where
    moon = 19690720
    (p, r) = parse str
```

This gets us an O(log n) search instead of an O(n^2) search, cutting down times
pretty nicely.

Just for the same of completion, I'm including my implementation of
`binaryMinSearch` here.  It's tucked away in my utilities/common
functionality file normally!

```haskell
-- | Find the lowest value where the predicate is satisfied within the
-- given bounds.
binaryMinSearch
    :: (Int -> Bool)
    -> Int                  -- ^ min
    -> Int                  -- ^ max
    -> Maybe Int
binaryMinSearch p = go
  where
    go !x !y
        | x == mid || y == mid = Just (x + 1)
        | p mid                = go x mid
        | otherwise            = go mid y
      where
        mid = ((y - x) `div` 2) + x
```


### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 293.8 μs   (268.6 μs .. 340.0 μs)
                     0.944 R²   (0.905 R² .. 0.999 R²)
mean                 270.1 μs   (264.8 μs .. 291.0 μs)
std dev              32.13 μs   (6.392 μs .. 66.74 μs)
variance introduced by outliers: 84% (severely inflated)

>> Day 02b
benchmarking...
time                 463.7 μs   (457.6 μs .. 470.7 μs)
                     0.986 R²   (0.961 R² .. 0.999 R²)
mean                 493.8 μs   (472.7 μs .. 543.4 μs)
std dev              104.8 μs   (43.20 μs .. 189.6 μs)
variance introduced by outliers: 94% (severely inflated)
```

