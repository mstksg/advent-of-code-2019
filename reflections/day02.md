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
