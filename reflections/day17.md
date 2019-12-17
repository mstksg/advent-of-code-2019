It's been a while since one of these!  I spent a lot of last week traveling and
it's been tough getting through the backlog :)

For today I'm only going to be discussing some parts of the solution that I
think are particularly interesting in Haskell: in particular, Part 2's path
construction and compression.

Once you have a set of points, it's useful to try to figure out the path to the
end.  From the constraints of the problem, we can make an educated guess that
our "pathfinding" has to be extremely simple in order to accommodate for the
small program size we can give.  Basically, it will be:

1.  Is there a spot in front of us?  If so, step forward and repeat from step 1.
2.  Otherwise, is there a spot to our left?  If so, turn left and repeat from
    step 1.
3.  Otherwise, is there a spot to our right?  If so, turn right and repeat from
    step 1.
4.  Otherwise, we've reached the end.

I'm going to use `Set Point` (where `Point` is `V2 Int`, for reasons discussed in
earlier problems) to describe our scaffolding, and a data type to keep track of
bot state.  The directionality will be tracked by keeping a unit vector in the
direction the bot is facing.

```haskell
type Point = V2 Int
data BotState = BS { bsPos :: Point, bsDir :: Point }
data Move = TurnLeft | GoForward | TurnRight
  deriving Eq

findPath :: Set Point -> BotState -> [Move]
findPath scaff = unfoldr go
  where
    go (BS p0 d0@(V2 dx dy))
        | forward   `S.member` scaff = Just (GoForward, BS forward d0       )
        | leftward  `S.member` scaff = Just (TurnLeft , BS p0      turnLeft )
        | rightward `S.member` scaff = Just (TurnRight, BS p0      turnRight)
      where
        forward   = p0 + d0
        turnLeft  = V2 dy    (-dx)
        turnRight = V2 (-dy) dx
        leftward  = p0 + turnLeft
        rightward = p0 + turnRight
```

To turn our path into a "run-length encoding" of instructions, we will convert
them into `Either Int Int`, where `Left n` means "turn left and go `n`
forward", and `Right n` means "turn right and go `n` forwards".  The easiest
way to do that is probably to use `group` and `chunksOf`

```haskell
pathToProg :: [Move] -> [Either Int Int]
pathToProg = traverse toInstr . chunksOf 2 . group
  where
    toInstr [[TurnLeft ],fs] = Just $ Left  (length fs)
    toInstr [[TurnRight],fs] = Just $ Right (length fs)
    toInstr _                = Nothing
```

Alright, so now form a `Set Point` and a `BotState` starting point, we get the
run-length encoding of our journey.  However, we now need to turn that into
repetitions of three distinct chunks, `A`, `B`, and `C`.

To do this, we can write a general combinator to turn *any* `[a]` into
encodings in terms of `A`, `B`, and `C` subprograms.  Let's call it:

```haskell
findProgs :: Eq a => [a] -> Maybe ([a], [a], [a])
```

If we start thinking about how we can pick these things, we notice some
interesting properties.  For example, for a string like `abcdefg`, we have many
possible options for `A`: it's either `a` or `ab` or `abc` or `abcd`, etc.  `A`
must be a prefix of our string. However, once we "commit" to an `A`, then that
also gives us our possibilities for `b`: in the same way, `b` must be a prefix
of the remaining string after we "eliminate" `A`.  So if we "pick" `A` to be
`abc`, the `B` can be either `d` or `de` or `def` or `defg`, etc.

This sort of "if we pick this ... then we can pick that ... and if we pick that
..." system is exactly what *Logic Programming* is great for!  And we can
actually do some nice logic programing in Haskell using the List monad.  I've
actually written about using the list monad for this purpose [multiple][wgc]
[times][money] over the years.

[wgc]: https://blog.jle.im/entries/series/+monadplus-success-failure-monads.html
[money]: https://blog.jle.im/entry/unique-sample-drawing-searches-with-list-and-statet.html

So let's lay out our full algorithm:

1.  We can pick `A` from any prefix of our string.
2.  Once we break out occurrences of our chosen `A` from the string, we can now
    pick `B` from any unbroken prefix of the remaining string.
3.  Once we break out occurrences of our chosen `B` from the string, we can now
    pick `C` from any unbroken prefix of the remaining string.
4.  Once we break out occurrences of our chosen `C` from the string, we only
    have a "real" solution if there are no other unclaimed items in the string.

This all translates pretty directly to usage of the `List` monad.  `findProgs`
will now return all valid `A`/`B`/`C` pairs:

```haskell
findProgs :: Eq a => [a] -> [([a], [a], [a])]
findProgs p0 = do
    a <- validPrefix p0

    let withoutA = splitOn' a p0
    b <- case withoutA of
        []        -> empty              -- 'A' consumed everything, whoops
        bs : _    -> validPrefix bs

    let withoutB = splitOn' b =<< withoutA
    c <- case withoutB of
        []        -> empty              -- 'A' and 'B' consumed everything, whoops
        cs : _    -> validPrefix cs

    let withoutC = splitOn' c =<< withoutB
    guard $ null withoutC

    pure (a, b, c)
  where
    -- | Get all valid prefixes
    validPrefix = take 4 . filter (not . null) . inits
    -- | a version of splitOn that only returns non-empty lists
    splitOn' x = filter (not . null) . splitOn x
```

Note that here I am using a simple predicate to filter out subprograms that are
"too long" (the `take 4` in `validPrefix`).  For a more robust solution, we can
do `validPrefix = filter validLength . inits`, testing on the length of the
strings that encode the programs.

And that is mostly it!  We can reconstruct our original program by using
iterated applications of `stripPrefix`, taking whatever prefix is *valid* at
every point:

```haskell
-- | Given an association list of subroutines and their "label", iteratively
-- chomp through a string replacing each occurence of the subroutine with the
-- label.
chomp :: Eq a => [([a], b)] -> [a] -> [b]
chomp progs = unfoldr go
  where
    go xs = asum
      [ (r,) <$> stripPrefix prog xs
      | (prog, r) <- progs
      ]
```

The nice thing about writing these functions "in general" (instead of just for
`Either Int Int`) is that it forces us to ignore some of the unimportant
details, and allows us only to use properties of lists (like lengths) and
equality testing.

And our final solution is, given a set of scaffolding points and an initial bot
state:

```haskell
data Prog = A | B | C

data Output = O
    { oProg :: [Prog]
    , oA    :: [Either Int Int]
    , oB    :: [Either Int Int]
    , oC    :: [Either Int Int]
    }

part2 :: Set Point -> BotState -> Maybe Output
part2 scaff b0 = listToMaybe (findProgs path) <&> \(a,b,c) ->     -- <&> is flip fmap
    O { oProg = chomp [(a, A), (b, B), (c, C)] path
      , oA    = a
      , oB    = b
      , oC    = c
      }
  where
    path = findPath scaff b0
```
