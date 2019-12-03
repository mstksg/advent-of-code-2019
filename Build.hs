#!/usr/bin/env stack
-- stack --install-ghc runghc --resolver lts-14.16 --package shake --package template --package directory --package containers --package text --package filepath --package strip-ansi-escape

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Assemble README and Reflections

import           Data.Char
import           Data.Foldable
import           Data.String.AnsiEscapeCodes.Strip.Text
import           Data.Text.Template
import           Data.Traversable
import           Development.Shake
import           Development.Shake.FilePath
import           System.Directory
import           System.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as TL

-- CONSTANTS
year :: Integer
year = 2019
github :: String
github = "mstksg"

ctx0 :: M.Map T.Text T.Text
ctx0 = M.fromList [
    ("year"  , T.pack (show year))
  , ("github", T.pack github     )
  ]

opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Loud
                    , shakeThreads   = 0
                    }

parseDayFp :: FilePath -> Maybe Int
parseDayFp = readMaybe . filter isDigit . takeBaseName

mkLinks :: Int -> [String]
mkLinks d = [
    printf "[d%02dg]: https://github.com/mstksg/advent-of-code-%04d/blob/master/src/AOC/Challenge/Day%02d.hs"
      d year d
  , printf "[d%02dh]: https://mstksg.github.io/advent-of-code-%04d/src/AOC.Challenge.Day%02d.html"
      d year d
  , printf "[d%02dr]: https://github.com/mstksg/advent-of-code-%04d/blob/master/reflections.md#day-%d"
      d year d
  , printf "[d%02db]: https://github.com/mstksg/advent-of-code-%04d/blob/master/reflections.md#day-%d-benchmarks"
      d year d
  ]

main :: IO ()
main = do
    Just days <- fmap S.fromList . traverse parseDayFp
        <$> listDirectory "reflections"

    shakeArgs opts $ do
      want ["README.md", "reflections.md"]

      "reflections.md" %> \fp -> do
        bodies <- forM (toList days) $ \d -> do
          refl   <- T.pack <$> readFile' ("reflections" </> printf "day%02d.md" d)
          bench  <- T.pack <$> readFile' ("bench-out" </> printf "day%02d.txt" d)
          let ctx = ctx0 <> M.fromList
                [ ("daylong"   , T.pack $ printf "%02d" d)
                , ("dayshort"  , T.pack $ printf "%d" d  )
                , ("body"      , refl                    )
                , ("benchmarks", bench                   )
                ]
          flip substitute (ctx M.!) . T.pack <$> readFile' "template/reflection.md.template"

        let toc = flip map (toList days) $ \d ->
                    printf "* [Day %d](#day-%d)" d d

            ctx = ctx0 <> M.fromList
              [ ("toc" , T.pack $ unlines toc                        )
              , ("body", TL.toStrict $ TL.intercalate "\n\n\n" bodies)
              ]

        out <- flip substitute (ctx M.!) . T.pack <$> readFile' "template/reflections.md.template"

        writeFileChanged fp (TL.unpack out)

      "bench-out/*.txt" %> \fp -> do
        let Just d = parseDayFp fp
        Stdout out <- cmd ("stack run --" :: String) (printf "bench %d" d :: String)
        writeFileChanged fp . T.unpack . T.strip . stripAnsiEscapeCodes . T.pack $ out

      "README.md" %> \fp -> do
        let mkRow d
              | d `S.member` days =
                  printf "| Day %2d    | [x][d%02dr]   | [x][d%02dg] | [x][d%02dh]  | [x][d%02db]  |"
                    d d d d d
              | otherwise         =
                  printf "| Day %2d    |             |           |            |            |"
                    d
            table = unlines $
               "| Challenge | Reflections | Code      | Rendered   | Benchmarks |"
             : "| --------- | ----------- | --------- | ---------- | ---------- |"
             : map mkRow [1..25]

            links = concatMap mkLinks days
            ctx = ctx0 <> M.fromList
                [ ("table", T.pack table          )
                , ("links", T.pack (unlines links))
                ]

        out <- flip substitute (ctx M.!) . T.pack <$> readFile' "template/README.md.template"

        writeFileChanged fp (TL.unpack out)

      "clean" ~> do
        removeFilesAfter "_build" ["//*"]
        removeFilesAfter "bench-out" ["//*"]

