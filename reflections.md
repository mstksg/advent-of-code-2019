Reflections
===========

[Table of Contents][]

[Table of Contents]: https://github.com/mstksg/advent-of-code-2019#reflections-and-benchmarks

Day 1
-----

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
time                 250.2 μs   (248.4 μs .. 252.2 μs)
                     0.997 R²   (0.992 R² .. 1.000 R²)
mean                 261.8 μs   (251.5 μs .. 284.5 μs)
std dev              46.74 μs   (6.059 μs .. 85.85 μs)
variance introduced by outliers: 92% (severely inflated)

>> Day 01b
benchmarking...
time                 267.4 μs   (265.4 μs .. 272.1 μs)
                     0.995 R²   (0.982 R² .. 1.000 R²)
mean                 268.0 μs   (265.0 μs .. 281.7 μs)
std dev              18.41 μs   (1.174 μs .. 42.16 μs)
variance introduced by outliers: 64% (severely inflated)
```

Day 2
-----

*[Prompt][d02p]* / *[Code][d02g]* / *[Rendered][d02h]*

[d02p]: https://adventofcode.com/2019/day/2
[d02g]: https://github.com/mstksg/advent-of-code-2019/blob/master/src/AOC/Challenge/Day02.hs
[d02h]: https://mstksg.github.io/advent-of-code-2019/src/AOC.Challenge.Day02.html

So the bytecode/VM problems start day 2 this year, eh?

This one was also pretty straightforward.  For these types of problems, I like
to use `Data.IntMap` or `Data.Sequence` for the memory, since they both have
*O(log n)* indexing.  `Data.Sequence` is the better choice here because it's
basically `IntMap` with the indices (0, 1, 2 ...) automatically given for us :)

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
    x  <- Seq.lookup p r
    o <- case x of
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
    , Just 19690720 <- [runProg (p, r')]
    ]
  where
    (p, r) = parse str
```

This doesn't take too long on my machine!  But for my [actual solution][d02g],
I actually used an exponential search (that I had coded up for last year): I
noticed that `noun` increases the answer by a lot, and `verb` increases it by a
little, so by doing an exponential search on `noun`, then an exponential search
on `verb`, you can get a good answer pretty quickly.  My part 2 time (580 μs)
is only twice as long as my part 1 time (260 μs) with the exponential search.
Happy that some prep time paid off :)

### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 261.1 μs   (257.7 μs .. 264.7 μs)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 260.6 μs   (258.7 μs .. 264.8 μs)
std dev              9.899 μs   (3.906 μs .. 17.63 μs)
variance introduced by outliers: 34% (moderately inflated)

>> Day 02b
benchmarking...
time                 577.1 μs   (573.2 μs .. 583.4 μs)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 576.3 μs   (573.5 μs .. 583.8 μs)
std dev              13.63 μs   (5.723 μs .. 26.78 μs)
variance introduced by outliers: 14% (moderately inflated)
```
