Okay, so I have a bit of backlog on my intcode-related posts (days 5, 7,
and 9).  But we've gotten to the point where the incode implementation isn't
the interesting part, but how we *use* it is, so maybe it's time for a fresh
start :)

This challenge affirmed my choice to use *[conduit][]* to model my Intcode VM.
(I actually use *[conduino][]*, my own lightweight alternative to *conduit*,
because it was able to handle something in Day 7 that I couldn't easily get
*conduit* to handle.  But since *conduit* is an actual industry-ready library
that is commonly used, I'm going to write this tutorial in terms of it instead)

[conduit]: https://hackage.haskell.org/package/conduit
[conduino]: https://hackage.haskell.org/package/conduino

For those unfamiliar with *conduit*, `ConduitT i o` is a monad transformer
(like `StateT s`, or `ReaderT r`, or `WriterT w`, etc.) that offers two new
primitives:

```haskell
await :: ConduitT i o m (Maybe i)
yield :: o -> ConduitT i o m ()
```

This *should* feel very similar to similar actions from `StateT`, `ReaderT`,
and `WriterT`:

```haskell
-- similar in form to 'await'
get :: StateT  s m s
ask :: ReaderT r m r

-- similar in form to 'yield'
put  :: s -> StateT  s m ()
tell :: w -> WriterT w m ()
```

You can think of `await` like reading from an input pipe, like *stdin*: you pick off the next
item the pipe is delivering you.  You can think of `yield` like writing to an
output pipe, like *stdout*.  You can then combine conduits to create new
conduits, like `c1 .| c2` -- it feeds the output of `c1` into the input of
`c2`, etc.

So for a type like `ConduitT i o m a`, `i` is the input stream's type, `o` is
the output stream's type, `m` is the underlying monad, and `a` is the result
type that is yielded when computation finishes.


My VM machine is essentially:

```haskell
intcodeVM :: Memory -> ConduitT Int Int m Memory
```

Given some starting memory state, you return a `ConduitT Int Int m Memory`:
take `Int`s as input, output `Int`s, and when it's done, output the finished
`Memory` once we halt.

So we have our transforming pipe...what sort of input does it need, and how are
we handling the output?

The input stream is relatively simple.  Let's put together a hull state:

```haskell
type Point = V2 Int         -- V2, from linear library
data Color = Black | White

data Hull = Hull
    { hDir :: Point         -- ^ unit-length direction vector
    , hPos :: Point
    , hMap :: Map Point Color
    }

emptyHull :: Hull
emptyHull = Hull (V2 0 1) 0 M.empty
```

The underlying monad of our `Conduit` (that all components will be able to
access) will be `State Hull`.

Our input pipe is will read the current hull point and output `0` or `1` based
on black or white:

```haskell
sensor :: ConduitT i Int (State Hull) a
sensor = forever $ do
    Hull _ p m <- get
    case M.lookup p m of
      Nothing    -> yield 0     -- black
      Just Black -> yield 0     -- black
      Just White -> yield 1     -- white
```

It'll just keep on reading and yielding, forever and ever.

Our output pipe will read the input of `intcodeVM` and adjust the state
appropriately --- it's slightly trickier because we have to parse the input and
modify the state.  `await` returns a `Maybe`, so if we get two `Just`'s then we
make our changes and repeat it all over again.  Otherwise, we're done.

```haskell
motors :: ConduitT Int o (State Hull) ()
motors = do
    color <- fmap parseColor <$> await
    turn  <- fmap parseTurn  <$> await
    case (color, turn) of
      (Just c, Just t) -> do
        modify $ \(Hull d p m) ->
          let d' = t d
          in  Hull d' (p + d') (M.insert p c m)
        motors                      -- recurse
      _                ->
        pure ()                     -- we're done!
  where
    parseColor 0 = Black
    parseColor 1 = White
    parseTurn  0 (V2 x y) = V2 (-y)   x     -- turn left
    parseTurn  1 (V2 x y) = V2   y  (-x)    -- turn right
```

And that's it!

```haskell
fullBot :: Memory -> Conduit i o (State Hull) ()
fullBot m = sensor
         .| intcodeVM m
         .| motors

```

We can run a full pipeline using `runConduit`:

```haskell
part1 :: Memory -> Int
part1 m = M.size m
  where
    Hull _ p m = execState (runConduit (fullBot m)) emptyHull
```

Part 2 is the same thing but we start on a painted hull:


```haskell
whiteHull :: Hull
whiteHull = Hull (V2 0 1) 0 (M.singleton 0 White)

part1 :: Memory -> Map Point Color
part1 m = m
  where
    Hull _ _ m = execState (runConduit (fullBot m)) whiteHull
```

The nice thing I like about the conduit method is that it lends itself really
well to "hooking up" the machine with input streams and output processing!  For
a machine that basically simulates stdin and stdout, it works very well, I
think!  You only need to think:

1.  How am I generating input?
2.  How am I processing output?

And your entire program will just be `generator .| intcodeVM m .| processor`.
This also worked pretty well as a mental model for Day 7 as well, because we
can easily pipe multiple independent machines: `intcodeVM m .| intcodeVM m .|
intcodeVM m`, and they will all maintain separate and independent memories as
they feed items to each other.  *conduit* handles all of the actual message
passing, and all you have to do is assemble your pipeline and let it churn
away!

Note that even if you didn't structure your intcode VM as a Conduit, it's
pretty easy to "turn it into" a `ConduitT Int Int`.  Integrating it into
conduit is nice even if you didn't intend to do it originally, using basic do
notation and combinations of `await` and `yield` and recursion.
