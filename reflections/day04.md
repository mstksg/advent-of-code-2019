I should probably appreciate these Haskell freebies while they still last :)  I
have a feeling they're not going to be this frictionless for long!

It's handy to have a function for giving us consecutive pairs of items:

```haskell
consecs :: [a] -> [(a,a)]
consecs xs = zip xs (tail xs)
```

Now for the fun part: making our filters!  For part 1, we have two filters on
the digits: first, that the digits are monotonic, and second, that at least one
pair of consecutive digits matches:

```haskell
mono :: Ord a => [a] -> Bool
mono = all (\(x,y) -> y >= x) . consecs

dups :: Eq a => [a] -> Bool
dups = any (\(x,y) -> x == y) . consecs
```

For part 2, we have two filters: the same `mono` filter, but also that we have
a group that is *exactly* length two.  For that we can use `group`, which
groups a list into chunks of equal items: `group "abbbcc" == ["a","bbb","cc"]`.
We then check if any of the chunks have a length of exactly two:

```haskell
strictDups :: Eq a => [a] -> Bool
strictDups = any ((== 2) . length) . group
```

And from here, we just run our filters on the range and count the number of
items:

```haskell
part1 :: Int -> Int -> Int
part1 mn mx = length . filter (\x -> all ($ show x) [mono, dups      ])
            $ [mn .. mx]

part2 :: Int -> Int -> Int
part2 mn mx = length . filter (\x -> all ($ show x) [mono, strictDups]) . range
            $ [mn .. mx]
```

For parsing the range, we can use `splitOn` again:

```haskell
range :: String -> (x, y)
range str = (x, y)
  where
    [x, y] =  map read (splitOn "-" str)
```

(Also, note to self next time ... if going for time, if you just have two
numbers in your input, just enter the numbers directly into the source file at
first, heh, instead of trying to parse them)

