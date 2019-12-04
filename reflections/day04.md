I should probably appreciate these Haskell freebies while they still last :)  I
have a feeling they're not going to be this frictionless for long!

Parsing in the range we can use `splitOn` again:

```haskell
range :: String -> [Int]
range str = [x..y]
  where
    [x, y] =  read <$> splitOn "-" str
```

It's also handy to have a function for giving us consecutive pairs of items:

```haskell
consecs :: [a] -> [(a,a)]
consecs xs = zip xs (tail xs)
```

Now for the fun part: making our filters!  For part 1, we have two filters on
the digits: first, that the digits are monotonic, and second, that at least one
pair of consecutive digits matches:

```haskell
mono :: Ord a => [a] -> Bool
mono = all (\(x,y) -> x >= y) . consecs

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
part1 :: String -> Int
part1 = length . filter (\x -> all ($ show x) [mono, dups      ]) . range

part1 :: String -> Int
part1 = length . filter (\x -> all ($ show x) [mono, strictDups]) . range
```
