This one feels like another Haskell freebie from the early days.  I'm not
complaining, we'll take what we can get :)

We'll define a useful function that counts the number of items in a list that
is equal to a given value:

```haskell
numMatches :: Eq a => a -> [a] -> Int
numMatches x = length . filter (== x)
```

We can use the [`chunksOf`][chunksOf] function from the amazing *[split][]*
package to split our input into chunks of 150.  Then we can find the maximum of
those lines based on their zero count.  Then we encode the answer.

[chunksOf]: https://hackage.haskell.org/package/split/docs/Data-List-Split.html#v:chunksOf
[split]: https://hackage.haskell.org/package/split

```haskell
part1 :: String -> Int
part1 = encodeAnswer
      . minimumBy (comparing (numMatchs '0'))
      . chunksOf 150
  where
    encodeAnswer xs = numMatches '1' xs * numMatches '2' xs
```

For part 2, we can use `transpose` turn a list of lines into a list where every
item is all of the pixel data for that pixel.  So it would turn

```
["1234"
,"1234"
,"1234"
]
```

into

```
["111"
,"222"
,"333"
,"333"
]
```

which is exactly what we need to process it.

Finding the 'pixel value' of each pixel is basically the first non-`2` pixel in
each list.  The first way that came to my mind was to use `dropWhile (==
'2')`, but `filter (/= '2')` would have worked as well.

```haskell
part2 :: String -> String
part2 = map (head . dropWhile (== '2'))
      . transpose
      . chunksOf 150
```

And that's it!  Well, almost.  Part 2 requires looking at 0/1 transparency data
and deducing our image.  For me, I wrote a function to display it nicely:

```haskell
showImage :: String -> String
showImage = unlines
          . chunksOf 25         -- number of columns
          . map (\case '0' -> ' '; _ -> '#')
```

```
#  # ###  #  # #### ###
#  # #  # #  # #    #  #
#  # ###  #  # ###  #  #
#  # #  # #  # #    ###
#  # #  # #  # #    #
 ##  ###   ##  #    #
```

