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
