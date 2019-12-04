#!/usr/bin/env stack
-- stack --install-ghc runghc --package shake --package template --package directory --package containers --package text --package filepath --package strip-ansi-escape --package html-entities --package time

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wall          #-}

-- | Assemble README and Reflections

import           Data.Char
import           Data.Foldable
import           Data.Maybe
import           Data.String.AnsiEscapeCodes.Strip.Text
import           Data.Text.Template
import           Data.Time
import           Data.Traversable
import           Development.Shake
import           Development.Shake.FilePath
import           Text.Printf
import           Text.Read
import qualified Data.Map                               as M
import qualified Data.Set                               as S
import qualified Data.Text                              as T
import qualified Data.Text.Lazy                         as TL
import qualified HTMLEntities.Text                      as H

-- CONSTANTS
year :: Integer
year = 2019

ctx0 :: M.Map T.Text T.Text
ctx0 = M.fromList [
    ("year"  , T.pack (show year))
  , ("github", "mstksg"          )
  , ("name"  , "Justin Le"       )
  , ("email" , "justin@jle.im"   )
  ]

opts :: ShakeOptions
opts = shakeOptions { shakeFiles     = "_build"
                    , shakeVersion   = "1.0"
                    , shakeVerbosity = Chatty
                    , shakeThreads   = 1    -- for benchmarks to work properly
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

reflPath :: Int -> FilePath
reflPath d = "reflections" </> printf "day%02d.md" d
reflOutPath :: Int -> FilePath
reflOutPath d = "_reflections" </> printf "day%02d.md" d
reflXmlPath :: Int -> FilePath
reflXmlPath d = "_reflections" </> printf "day%02d.xml" d
benchPath :: Int -> FilePath
benchPath d = "bench-out" </> printf "day%02d.txt" d

main :: IO ()
main = shakeArgs opts $ do
    want ["README.md", "reflections.md", "feed.xml"]

    "reflections.md" %> \fp -> do
        days   <- getDays
        bodies <- forM (toList days) $ \d ->
          T.pack <$> readFile' (reflOutPath d)
        let toc = flip map (toList days) $ \d ->
                    printf "* [Day %d](#day-%d)" d d

            ctx = ctx0 <> M.fromList
              [ ("toc" , T.pack $ unlines toc         )
              , ("body", T.intercalate "\n\n\n" bodies)
              ]
        writeTemplate fp ctx "template/reflections.md.template"

    "README.md" %> \fp -> do
        days <- getDays
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
        writeTemplate fp ctx "template/README.md.template"

    "feed.xml" %> \fp -> do
        days <- getDays
        bodies <- forM (toList days) $ \d ->
          T.pack <$> readFile' (reflXmlPath d)
        time <- utcToZonedTime (read "EST") <$> liftIO getCurrentTime
        let ctx = ctx0 <> M.fromList
              [ ("body", T.intercalate "\n" bodies)
              , ("time", T.pack . formatTime defaultTimeLocale rfc822DateFormat $ time )
              ]
        writeTemplate fp ctx "template/feed.xml.template"

    "_reflections/*.md" %> \fp -> do
        let Just d = parseDayFp fp
        refl   <- T.pack <$> readFile' (reflPath  d)
        bench  <- T.pack <$> readFile' (benchPath d)
        let ctx = ctx0 <> M.fromList
              [ ("daylong"   , T.pack $ printf "%02d" d)
              , ("dayshort"  , T.pack $ printf "%d" d  )
              , ("body"      , refl                    )
              , ("benchmarks", bench                   )
              ]
        writeTemplate fp ctx "template/reflection.md.template"

    "_reflections/*.xml" %> \fp -> do
        let Just d = parseDayFp fp
        refl <- readFile' (reflOutPath d)
        Stdout html <- cmd ("pandoc -t html -f markdown" :: String)
                           (Stdin refl)
        let time = ZonedTime
                     (LocalTime (fromGregorian year 12 d)
                                (TimeOfDay 1 0 0)
                     )
                     (read "EST")
            ctx = ctx0 <> M.fromList
              [ ("day" , T.pack $ printf "%d" d )
              , ("body", H.text . T.pack $ html )
              , ("time", T.pack . formatTime defaultTimeLocale rfc822DateFormat $ time )
              ]
        writeTemplate fp ctx "template/feed-item.xml.template"

    "bench-out/*.txt" %> \fp -> do
        let Just d = parseDayFp fp
        Stdout out <- cmd ("stack run --" :: String) (printf "bench %d" d :: String)
        writeFileChanged fp . T.unpack . T.strip . stripAnsiEscapeCodes . T.pack $ out

    "clean" ~> do
      removeFilesAfter "_reflections" ["//*"]
      removeFilesAfter "bench-out" ["//*"]
      removeFilesAfter "_build" ["//*"]
  where
    getDays = S.fromList . mapMaybe parseDayFp <$> getDirectoryFiles "reflections" ["*.md"]
    writeTemplate fp ctx templ = do
      out <- flip substitute (ctx M.!) . T.pack <$> readFile' templ
      writeFileChanged fp (TL.unpack out)
