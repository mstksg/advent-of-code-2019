
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
